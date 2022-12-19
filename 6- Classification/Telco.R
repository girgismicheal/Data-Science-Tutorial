install.packages("caret")
install.packages("e1071")
install.packages("readr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("corrplot")
install.packages("MASS")
install.packages("rms")
install.packages("ROCR")
install.packages("pROC")
install.packages("randomForest")
install.packages("ggpubr")
install.packages("gplots")

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(caret)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)

## Explore Data
WA_Fn_UseC_Telco_Customer_Churn <- read_delim("C:/Users/olubi/Desktop/WA_Fn-UseC_-Telco-Customer-Churn.csv")
telco <- WA_Fn_UseC_Telco_Customer_Churn
telco <- data.frame(telco)
str(telco)
summary(telco)

#### Observations with Missing Values
telco <- telco[complete.cases(telco),] 

#### Continuous Variables - check for distributions.

#Check the NonthlyCharges distribution
ggplot(data = telco, aes(MonthlyCharges, color = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)
#The # of current customers with MonthlyCharges below $25 is extremely high. 
#For customers with Monthlycharges > $30, the distributions are similar between who churned and who did not churn.

#Check the TotalCharges distribution for skewness 
ggplot(data = telco, aes(TotalCharges, color = Churn))+
  geom_freqpoly(binwidth = 200, size = 1)
#The distribution of TotalCharges is highly positive skew for all customers no matter whether they churned or not. 

#Check the Tenure distribution for skewness 
ggplot(data = telco, aes(tenure, colour = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)
#The distributions for tenure are very different between customers who churned and who didn't churn. 
#For customers who churned, the distribution is positively skewed, i.e., customers who churned are more likely to cancel the service in the first couple of months. 
#For current customers who didn't churn, there are two spikes.2nd spike is much more drastic than the 1st, i.e, a large group of current customers have been using the service more than 5 years.

#heck for correlations between totalcharges, monthlycharges & tenure
telco %>%
  dplyr::select (TotalCharges, MonthlyCharges, tenure) %>%
  cor() %>%
  corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)
#The plot shows high correlations between Totalcharges & tenure and also, between TotalCharges & MonthlyCharges. 
#Pay attention to these variables when training models later.

#The tenure represents time period in months. To better find patterns with time, change it to a factor with 5 
#levels, with each level representing a bin of tenure in years. Use mutate() function
telco %>%
  mutate(tenure_year = case_when(tenure <= 12 ~ "0-1 year",
                                 tenure > 12 & tenure <= 24 ~ "1-2 years",
                                 tenure > 24 & tenure <= 36 ~ "2-3 years",
                                 tenure > 36 & tenure <= 48 ~ "3-4 years",
                                 tenure > 48 & tenure <= 60 ~ "4-5 years",
                                 tenure > 60 & tenure <= 72 ~ "5-6 years")) -> telco
telco$tenure <-NULL # remove tenure
table(telco$tenure_year)
summary(telco)

#### Categorical Variables
#Phone Service. & MultipleLines have rows wirh the value of "No Phone Service". Are they related?
table(telco[, c("PhoneService","MultipleLines")])

#When the value of Phone Service is "No", the value of Multiplelines shows "No Phone Service." The"No Phone Service" 
#value in the Multiplelines column actually does not have any predicting power. 

#The same problem appeared between Internet Service and Online Security, OnlineBackup, DeviceProtection, TechSupport, 
#StreamingTV and StreamingMovies. When the value of Internet Service is "No", the values of the following 6 columns show "No Internet Service."

table(telco[, c("InternetService", "OnlineSecurity")])
table(telco[, c("InternetService", "OnlineBackup")])
table(telco[, c("InternetService", "DeviceProtection")])
table(telco[, c("InternetService", "TechSupport")])
table(telco[, c("InternetService", "StreamingTV")])
table(telco[, c("InternetService", "StreamingMovies")])

#The problem can be addressed in the data preparation.
#Now I will check the distributions of churn by the levels of yes or no 
#for the above 7 variables. remove the rows with "No phone service" and "No internet service" in the plot.  

telco %>%
  mutate(SeniorCitizen = ifelse(SeniorCitizen == 0, "No", "Yes")) -> categorical

categorical %>%
  dplyr::select(gender:Dependents, PhoneService:PaymentMethod, Churn) -> categorical 

categorical %>%
  dplyr::select(MultipleLines, OnlineSecurity:StreamingMovies, Churn) %>%
  filter(MultipleLines != "No phone service" &
           OnlineSecurity != "No internet service") -> c2

gather(c2, columns, value, -Churn) -> c3

ggplot(c3)+
  geom_bar(aes(x = value, fill = Churn), position = "fill", stat = "count")+
  facet_wrap(~columns)+ 
  xlab("Attributes")

#The customers who subscribe the service of DeviceProtection, OnlineBackup, OnlineSecurity and TechSupport have lower 
#churn rate compared to the customers who dont. However, the churn rates do not have big difference between customers 
#who have the service of MultipleLines, StreamingMovies and StreamingTV or not. 

categorical %>%
dplyr::select(Contract:Churn) -> c4

ggplot(c4) +
  geom_bar(aes(x = Contract, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p7

ggplot(c4) +
  geom_bar(aes(x = PaperlessBilling, fill = Churn), position = "fill", stat = "count", 
           show.legend = T) -> p8

ggplot(c4) +
  geom_bar(aes(x = PaymentMethod, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) +
  scale_x_discrete(labels = c("Bank transfer", "Credit card", "Electronic check", "Mail check"))+
  theme(axis.text= element_text(size=7)) -> p9

ggarrange(p7,p8,p9, ncol = 2, nrow = 2)


#The customers who sign longer contract have lower churn rate (Two year < One year < Month-to-month).  
#The customers who choose paperlessbilling have higher churn rate.   
#The customers who pay with electronic check have higher churn rate than customers who pay with other methods.  
       

#Check if churn rates are different among the attributes about customers basic information. 
categorical %>%
  dplyr::select(gender:Dependents, PhoneService, InternetService, Churn) %>%
  mutate(Gender_male = ifelse(gender =="Male", "Yes", "No")) -> c1 

c1$gender <- NULL

ggplot(c1) +
  geom_bar(aes(x = Gender_male, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p1
ggplot(c1) +
  geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p2
ggplot(c1) +
  geom_bar(aes(x = Partner, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p3    
ggplot(c1) +
  geom_bar(aes(x = Dependents, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p4  
ggplot(c1) +
  geom_bar(aes(x = PhoneService, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p5
ggplot(c1) +
  geom_bar(aes(x = InternetService, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p6

ggarrange(p1,p2,p3,p4,p5,p6, ncol = 3, nrow = 2)


#The churn rates are not changed by genders and phone service.   
#The senior customers have higher churn rate.   
#The customers who have partners or dependents have lower churn rate.


#### Check Churn Rate for the full dataset
telco %>%
  summarise(Total = n(), n_Churn = sum(Churn == "Yes"), p_Churn = n_Churn/Total)

#There are 26.6% of customers churn.


# Decision Tree
### Data Preparation
#Decision tree models can handle categorical variables without one-hot encoding them, and one-hot encoding will degrade 
#tree-model performance. Thus, I will re-prepare the data for decision tree and random forest models. I keep the "telco" data 
##classification tree models.

telcotree <- telco

telcotree$customerID <- NULL

telcotree %>%
  mutate_if(is.character, as.factor) -> telcotree

str(telcotree)


#Split the data into training and test sets.

set.seed(818)
tree <- sample(0:1, size= nrow(telcotree), prob = c(0.75,0.25), replace = TRUE)
traintree <- telcotree[tree == 0, ]
testtree <- telcotree[tree == 1, ]


### Train Model1
#First use all variables to build the model_tree1. 

model_tree1 <- rpart(formula = Churn ~., data = traintree, 
                     method = "class", parms = list(split = "gini"))


### Cross Validation (Confusion Matrix and AUC() for modeltree1

predict(model_tree1, data = traintree, type = "class") -> traintree_pred1
predict(model_tree1, data = traintree, type = "prob") -> traintree_prob1
predict(model_tree1, newdata= testtree, type = "class") -> testtree_pred1
predict(model_tree1, newdata = testtree, type = "prob") -> testtree_prob1

#For the Training Set
confusionMatrix(data = traintree_pred1, reference = traintree$Churn)
traintree_actual <- ifelse(traintree$Churn == "Yes", 1,0)
roc <- roc(traintree_actual, traintree_prob1[,2], plot= TRUE, print.auc=TRUE)

#For the Test Set:
confusionMatrix(data = testtree_pred1, reference = testtree$Churn)
testtree_actual <- ifelse(testtree$Churn == "Yes", 1,0)
roc <- roc(testtree_actual, testtree_prob1[,2], plot = TRUE, print.auc = TRUE)

#For the training set, the Accuracy is 0.79 and the AUC is 0.800. For the test set, the Accuracy is 0.78 and the AUC is 0.78.

### Train Model2
#Since Totalcharges, MonthlyCharges and tenure are highly correlated, which may effect the performance of the 
#decision tree models. Remove the TotalCharges column to train the second model.

model_tree2 <- rpart(formula = Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + 
                       MultipleLines + InternetService + OnlineSecurity + TechSupport +
                       OnlineBackup + DeviceProtection + StreamingTV + StreamingMovies + 
                       Contract + PaperlessBilling + tenure_year +
                       PaymentMethod + MonthlyCharges, data = traintree, 
                       method = "class", parms = list(split = "gini"))

### Cross Validation for modeltree2
predict(model_tree2, data = traintree, type = "class") -> traintree_pred2
predict(model_tree2, data = traintree, type = "prob") -> traintree_prob2
predict(model_tree2, newdata= testtree, type = "class") -> testtree_pred2
predict(model_tree2, newdata = testtree, type = "prob") -> testtree_prob2


#For the Training Set:
confusionMatrix(data = traintree_pred2, reference = traintree$Churn)
traintree_actual <- ifelse(traintree$Churn == "Yes", 1,0)
roc <- roc(traintree_actual, traintree_prob2[,2], plot= TRUE, print.auc=TRUE)

#For the Test Set:
testtree_actual <- ifelse(testtree$Churn == "Yes", 1,0)
confusionMatrix(data = testtree_pred2, reference = testtree$Churn)
roc <- roc(testtree_actual, testtree_prob2[,2], plot = TRUE, print.auc = TRUE)


#For the training set, the Accuracy is 0.80 and the AUC is 0.80. For the test set, the Accuracy is 0.78 and the AUC is 0.78.
#Compared to the performance of the first model, the performance of the second model is just a little bit better.
#Therefore, use model 2 as the final classification tree model.


# Random Forest
### Data Preparation
#Use the same data prepared for Classification Tree models.

### Train Model
set.seed(802)
modelrf1 <- randomForest(formula = Churn ~., data = traintree)
print(modelrf1)

### Cross Validation for modelrf1
predict(modelrf1, traintree, type = "class") -> trainrf_pred
predict(modelrf1, traintree, type = "prob") -> trainrf_prob
predict(modelrf1, newdata = testtree, type = "class") -> testrf_pred
predict(modelrf1, newdata = testtree, type = "prob") -> testrf_prob


#For the Training Set: 
confusionMatrix(data = trainrf_pred, reference = traintree$Churn)
trainrf_actual <- ifelse(traintree$Churn == "Yes", 1,0)
roc <- roc(trainrf_actual, trainrf_prob[,2], plot= TRUE, print.auc=TRUE)

#For the Test Set:
confusionMatrix(data = testrf_pred, reference = testtree$Churn)
testrf_actual <- ifelse(testtree$Churn == "Yes", 1,0)
roc <- roc(testrf_actual, testrf_prob[,2], plot = TRUE, print.auc = TRUE)


#For the training set, the Accuracy is 0.97 and the AUC is almost 1. For the test set, the Accuracy is 0.79 and the AUC is 0.82.

### Tuning 

#### Tuning mtry with tuneRF
set.seed(818)
modelrf2 <- tuneRF(x = subset(traintree, select = -Churn), y = traintree$Churn, ntreeTry = 500, doBest = TRUE)
print(modelrf2)

#When mtry = 2, OOB decreases from 20.11% to 19.67%

#### Grid Search based on OOB error
#first establish a list of possible values for mtry, nodesize and sampsize.
mtry <- seq(2, ncol(traintree) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(traintree) * c(0.7, 0.8)
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

#create a loop to find the combination with the optimal oob err. 
oob_err <- c()
for (i in 1:nrow(hyper_grid)) {
  model <- randomForest(formula = Churn ~ ., 
                        data = traintree,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
  }

opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])


#The optimal hyperparameters are mtry = 2, nodesize = 7, sampsize = 3658.2

### Train model 2 with optimal hyperparameters.
set.seed(802)
modelrf3 <- randomForest(formula = Churn ~., data = traintree, mtry = 2, nodesize = 7, sampsize = 3658.2)
print(modelrf3)

#OOB of modelrf3 decreases a little bit to 19.79% with the optimal combination. The OOB of modelrf2 is 19.67%. 
#So use modelrf2 as the final random forest model.

### Cross Validation for modelrf2
predict(modelrf2, traintree, type = "class") -> trainrf_pred2
predict(modelrf2, traintree, type = "prob") -> trainrf_prob2
predict(modelrf2, newdata = testtree, type = "class") -> testrf_pred2
predict(modelrf2, newdata = testtree, type = "prob") -> testrf_prob2


#For the Training Set: 
confusionMatrix(data = trainrf_pred2, reference = traintree$Churn)
trainrf_actual <- ifelse(traintree$Churn == "Yes", 1,0)
roc <- roc(trainrf_actual, trainrf_prob2[,2], plot= TRUE, print.auc=TRUE)


#For the Test Set:
confusionMatrix(data = testrf_pred2, reference = testtree$Churn)
testrf_actual <- ifelse(testtree$Churn == "Yes", 1,0)
roc <- roc(testrf_actual, testrf_prob2[,2], plot = TRUE, print.auc = TRUE)


#For the training set, the Accuracy is 0.88 and AUC is 0.95. For the test set, the Accuracy is 0.79 and the AUC is 0.82. 
#Compared to the performance of the first model, which Accuracy = 0.97, AUC = 0.995 for the training set, and Accuracy = 0.79,
#AUC = 0.82 for the test set. The second model works a little better.


### Variable Importance
varImpPlot(modelrf2,type=2)


### Summary for Random Forest Model
#The final random forest model has the Accuracy of 0.79 and AUC of 0.82 for the test set.   
#According to the Variable Importance plot, TotalCharges, MonthlyCharges, Tenure_year and Contract are the top 4 most important 
#variables to predict churn. The PhoneSerivce, Gender, SeniorCitizen, Dependents, Partner, MultipleLines, PaperlessBilling, StreamingTV,
#Movies, DeviceProtection and OnlineBackup have very small effect on Churn.








