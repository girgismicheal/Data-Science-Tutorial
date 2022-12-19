### R code from vignette source 'ch-decision-trees.rnw'
## install R packages from CRAN
pkgs <- c("caret", "data.table", "dplyr", "e1071", "ggplot2", "MASS", "party","rpart.plot", "scatterplot3d")


## exclude packages already installed
pkgs.installed <- installed.packages()
pkgs.to.install <- setdiff(pkgs, pkgs.installed[, 1])
print("Packages to install:")
print(pkgs.to.install)
install.packages(pkgs.to.install)

#use iris data set
str(iris) #Display structure & result shows the predictor values
set.seed(1234) 
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3)) # Create the sample
trainData <- iris[ind==1,]
trainData
testData <- iris[ind==2,]

#*****************************************************************************************************
#use party package to model decision tree

#The package "party" has the function ctree() which is used to create and analyze decision tree
#Syntax: ctree(formula, data)
#formula: formula describing the predictor and response variables.
#data: name of the data set used

library(party) # Load the party package
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)

# check the prediction
table(predict(iris_ctree), trainData$Species)

#plot tree
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")


# predict on test data
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)


