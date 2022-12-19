#Extracts from Machine Learning using R - K. Ramasubramanian & A. Singh, 2019


#**********************************************************************
#Data Cleaning: Removing inconsistencies from data, e.g., missing values & following a standard format in abbreviations

# Dealing with NAs (Not Available): NAs are missing values and will always lead to wrong interpretation
# If you have large volume of data - remove/ignore
# If small dataset - Feature imputation
emp <-read.csv("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Tut3-EDA_Data_Preparation/employees.csv", header =TRUE, sep =",")
employees_qual <- read.csv("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Tut3-EDA_Data_Preparation/employees_qual.csv")


#Correcting the inconsistency
employees_qual$Qual =as.character(employees_qual$Qual)
employees_qual

#The ifelse() command checks the condition specified and changes the value when the condition is met
employees_qual$Qual <- ifelse(employees_qual$Qual %in% c("Phd","phd","PHd"), "PhD", employees_qual$Qual)
employees_qual

#Store the output from right_join in the variables impute_salary
impute_salary <-right_join(emp, employees_qual, by ="Code")
impute_salary

#Calculate the average salary for each Qualification
ave_age <-ave(impute_salary$Salary.US.Dollar., impute_salary$Qual, FUN = function(x) mean(x, na.rm =TRUE))
ave_age

#Fill the NAs with the average values
#the is.na() function provides tests for missing values.
x <- c(1, 2, 3, NA, 4)
is.na(x)
#[1] FALSE FALSE FALSE TRUE FALSE

#The ifelse() command checks the condition specified under is.na,
impute_salary$Salary.US.Dollar. <-ifelse(is.na(impute_salary$Salary.US.Dollar.), ave_age, impute_salary$Salary.US.Dollar.)
impute_salary



#*******************************************************************************************
marathon <-read.csv("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Tut3-EDA_Data_Preparation/marathon.csv")
marathon[1:5,]
#Re-express Categorical Field Values
#Install & load package plyr
install.packages("plyr")
library(plyr)

#revalue() function replaces values in the variable given in the x input, according to the rules given in the replace input

marathon.num <- revalue(x = marathon$Type, replace= c("First-Timer" = 0, "Frequents" = 1, "Professional" = 2))
marathon[1:5,]

#Append values to the dataframe
marathon$numbers <- marathon.num
marathon[1:5,]

# or Append in place
marathon$Type <- marathon.num
marathon[1:5,]

#View the whole dataframe again
marathon


#*******************************Data Exploration****************************************
#Summary Statistics
#summary() function displays several common summary statistics

summary(marathon)

#*********Measuring the central tendency – mean and median
mean(marathon$Finish_Time)
median(marathon$Finish_Time)


#*******Measuring spread – quartiles and the five-number summary
#Written in order, they are:
#1. Minimum (Min.)
#2. First quartile, or Q1 (1st Qu.)
#3. Median, or Q2 (Median)
#4. Third quartile, or Q3 (3rd Qu.)
#5. Maximum (Max.)


#First quartile:
quantile(marathon$Finish_Time, 0.25)

#Second quartile or median:
quantile(marathon$Finish_Time, 0.5)

#median(marathon$Finish_Time)

#Third quartile:
quantile(marathon$Finish_Time, 0.75)

#The interquartile range is the difference between the 75th percentile and 25th percentile 
quantile(marathon$Finish_Time, 0.75, names = FALSE) - quantile(marathon$Finish_Time, 0.25, names =FALSE)

#OR
IQR(marathon$Finish_Time)

#Range: The span between the minimum and maximum value 
range(marathon$Finish_Time)

var(marathon$Finish_Time)
sd(marathon$Finish_Time)

#The function apply() is useful when the same function is to be applied to several variables in a data frame
apply(marathon[,c(1:3)], MARGIN=2, FUN=sd)
#Other functions, such as lapply() and sapply(), apply a function to a list or vector.
#tapply applies a function to each cell of a ragged array
tapply(marathon$Finish_Time,marathon$Type, mean)
tapply(marathon$Finish_Time,marathon$Type, sd)


#*****************Visualizing Numeric Variables********************************************

#Frequency plot helps in understanding the distribution of values in a discrete or continuous variable
plot(marathon$Type, xlab ="Marathoners Type", ylab ="Number of Marathoners")


#Boxplot
boxplot(Finish_Time ~Type,data=marathon, main="Marathon Data", xlab="Type of Marathoner", ylab="Finish Time")

#Histograms
hist(marathon$Finish_Time, main = "Number of Marathoners", xlab = "Marathoners Type")


#********Standardizing Numeric Fields****************************************************

#Use the scale() function to standardize
marathon$Finish_Time <- scale(x = marathon$Finish_Time)
marathon$Finish_Time

#**************************************************
#Identifying Outliers
#find outliers by using the query() function, which identifies
#rows that meet a particular condition

marathon_outliers <-  marathon[ which(marathon$Finish_Time < -3 | marathon$Finish_Time > 3), ]
marathon_outliers


#*******Equal width binning********************************************************8
dataset <- c(0, 4, 12, 16, 16, 18, 24, 26, 28)

library(classInt)
classIntervals(dataset, 4)
x <- classIntervals(dataset, 4, style = 'equal')

#*******Equal frequency binning******************************
classIntervals(dataset, 4, style = 'quantile')

#**************************************************************
set.seed(1)
dataset <- runif(100, 0, 10) # some random data
bins<-4
minimumVal<-min(dataset)
maximumVal<-max(dataset)
width=(maximumVal-minimumVal)/bins;
cut(dataset, breaks=seq(minimumVal, maximumVal, width))

#plot frequencies in the bins
barplot(table(cut(dataset, breaks=seq(minimumVal, maximumVal, width))))



#***********************************************************************8
# random sample of 5 observations
s <- sample(row.names(housing.df), 5)
housing.df[s,]

# oversample houses with over 10 rooms
s <- sample(row.names(housing.df), 5, prob = ifelse(housing.df$ROOMS>10, 0.9, 0.01))
housing.df[s,]