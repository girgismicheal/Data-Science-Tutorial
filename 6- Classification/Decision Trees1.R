#DECISION TREES IN R

#The algorithm used in the Decision Tree in R is the Gini Index, information gain
#There are different packages available to build a decision tree in R:
#  rpart, party,  CART (classification and regression).

#**********************************************************************************************************
#*BUILDING A SIMPLE DECISION TREE I
#*
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

head(iris)

set.seed(42)
sample_split <- sample.split(Y = iris$Species, SplitRatio = 0.75)
train_set <- subset(x = iris, sample_split == TRUE)
test_set <- subset(x = iris, sample_split == FALSE)

model <- rpart(Species ~ ., data = train_set, method = "class") #specify method as class since we are dealing with classification
model

#plot the model
rpart.plot(model)

#Select features by checking feature importance
#importances <- varImp(model) #use the varImp() function to determine how much predictive power lies in each feature
#importances %>% arrange(desc(Overall))


# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#Make predictions
preds <- predict(model, newdata = test_set, type = "class") #use the predict() function and pass in the testing subset
preds

#Print the confusion Matrix
confusionMatrix(test_set$Species, preds)

#*************************************************************************************************************
#SELECTING FEATURES BASED ON CORRELATION

#Highly correlated features are linearly dependent & have same effect on the dependent variable,
#therefore, one of the features can be dropped


library(tidyverse) #perform data manipulation & visualization 
library(caret) # cross - validation methods, provides findCorrelation function
library(ISLR)# import desired dataset

path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
#Correlation is usually computed on two quantitative variables
data <-read.csv(path, stringsAsFactors = TRUE )
glimpse(data)
titanic_data <- sapply(data, unclass)           #Convert categorical variables
titanic_data 

titanic_df <- subset(titanic_data, select = -survived)

#calculate correlation matrix
correlationMatrix <- cor(titanic_df)

# summarize the correlation matrix
print(correlationMatrix)

# find attributes that are highly corrected (ideally > 0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

hc = sort(highlyCorrelated)
hc
reduced_Data = titanic_df[ ,-c(hc)]
print (reduced_Data)


#Using Corrplot
install.packages("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r") # to use rquery.cormat
#calculate correlation matrix
rquery.cormat(titanic_df)
cormat<-rquery.cormat(titanic_df, graphType="heatmap")



##plot correlation matrix using Heatmap
install.packages("corrplot")

library(corrplot)
library(reshape2) #to meld the data frame
library(ggplot2)

corrplot(cor(titanic_data[,2:13]),        # Correlation matrix
         method = "circle",                # Correlation plot method (method = number, circle, pie, or color)
         type = "full",                   # Correlation plot style (also "upper" and "lower")
         diag = TRUE,                     # If TRUE (default), adds the diagonal
         tl.col = "black",                # Labels color
         bg = "white",                    # Background color
         title = "",                      # Main title
         col = NULL,                      # Color palette
         tl.cex =0.7,
         cl.ratio =0.2)                            

corrplot(cor(titanic_data[,2:13],method = "pearson"),diag = TRUE, #spearman can e used on qualitative data
         method = "ellipse",
         tl.cex = 0.7, tl.col = "black", cl.ratio = 0.2)




#**********************************************************************************************************
#BUILDING A SIMPLE DECISION TREE II

#import essential libraries
install.packages("readr")
library(rpart)
library(readr)
library(caTools)
library(dplyr)
library(party)
library(partykit)
library(rpart.plot)


#read data & store it inside the titanic_data variable
titanic_data <- "https://goo.gl/At238b" %>% 
  read.csv %>% # read in the data
  select(survived, embarked, sex, sibsp, parch, fare) %>%
  mutate(embarked = factor(embarked), sex = factor(sex))

#split data into training and testing sets
set.seed(123)
sample_data = sample.split(titanic_data, SplitRatio = 0.75)
train_data <- subset(titanic_data, sample_data == TRUE)
test_data <- subset(titanic_data, sample_data == FALSE)

sample_data
dim(train_data)

#Plot decision tree
rtree <- rpart(survived ~ ., train_data)

#plot conditional parting plot
ctree_ <- ctree(survived ~ ., train_data)
plot(ctree_)
rpart.plot(rtree)


#**********************************************************************************************************
#BUILDING A SIMPLE DECISION TREE III

#Step 1: load require libraries & dataset
install.packages("tidyverse")

library(tidyverse) #perform data manipulation & visualization 
library(caret) # cross - validation methods
library(ISLR)# import desired dataset

#step 2: Data Manipulation
#Load the data set
set.seed(678)
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic_data <-read.csv(path)
head(titanic_data)

#or load & select variables
titanic_data <- "https://goo.gl/At238b" %>% 
  read.csv %>% # read in the data
  select(survived, embarked, sex, sibsp, parch, fare) %>%
  mutate(embarked = factor(embarked), sex = factor(sex))

glimpse(titanic_data) #display data set in details
tail(titanic_data) # check the last rows

# sample to ensure data values from all features are included
shuffle_index <- sample(1:nrow(titanic_data)) 
head(shuffle_index)
titanic_data <- titanic_data[shuffle_index, ]

table(titanic_data$survived) #check values present in the "survived" column
anyNA(titanic_data)

#Step 3: Clean the data
library(dplyr)
clean_titanic <- titanic_data %>%
  #Drop variables home.dest, name, sex and ticket
  select(-c(home.dest, cabin, name, sex, ticket)) %>% 
  #Create factor variables for pclass and survived
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  #Drop the NA
  na.omit() 

glimpse(clean_titanic)


#Step 4: Model Building
set.seed(123) #generates a reproducible random sampling

#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 10)

#fit a decision tree model and use k-fold CV to evaluate performance
dtree_fit_gini <- train(survived~., data = titanic_data, method = "rpart", parms = list(split = "gini"), trControl = ctrl, tuneLength = 10)

#Step 5: Evaluate - view summary of k-fold CV               
print(dtree_fit_gini) #metrics give us an idea of how well the model performed on previously unseen data

#view final model
dtree_fit_gini$finalModel
prp(dtree_fit_gini$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
dtree_fit_gini$resample

#Check accuracy
test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$V7 )  #check accuracy
