
install.packages("adabag")
install.packages("rpart")
install.packages("caret")

library(adabag)
library(rpart)
library(caret)
bank.df <- read.csv("https://www.biz.uiowa.edu/faculty/jledolter/datamining/UniversalBank.csv")
str(bank.df)
bank.df <- bank.df[ , -c(1, 5)] # Drop ID and zip code columns.

# transform Personal.Loan into categorical variable
bank.df$PersonalLoan <- as.factor(bank.df$PersonalLoan)

# partition the data
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# single tree
tr <- rpart(PersonalLoan ~ ., data = train.df)
pred <- predict(tr, valid.df, type = "class")
confusionMatrix(pred, valid.df$PersonalLoan)

# bagging
bag <- bagging(PersonalLoan ~ ., data = train.df)
pred <- predict(bag, valid.df, type = "class")
confusionMatrix(class, PersonalLoan)

# boosting
boost <- boosting(PersonalLoan ~ ., data = bank.df)
pred <- predict(boost, valid.df, type = "class")
confusionMatrix(class, PersonalLoan)
