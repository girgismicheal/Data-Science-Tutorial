#Extracts from Machine Learning using R - K. Ramasubramanian & A. Singh, 2019
#Function str( ) : provides a method for displaying the structure of a data frame
#output shows four useful tidbits about the data.
#• The number of rows and columns in the data
#• Variable name or column header in the data
#• Data type of each variable
#• Sample values for each variable

emp <-read.csv("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Tut3-EDA_Data_Preparation/employees.csv", header =TRUE, sep =",")
str(emp)

dim(emp) # to determine the number of records

n=5
emp$index <- c(1:n) # creates an index on the records
head(emp) # gives the 1st 5 records


#Function make.names( )

#Manually overriding the naming convention
names(emp) <- c('Code','First Name','Last Name', 'Salary(US Dollar)')

# Look at the variable name
emp

# Now let's clean it up using make.names
names(emp) <- make.names(names(emp))
# Look at the variable name after cleaning
emp

#Function table()
# Shows the frequency distribution in a one- or two-way tabular format
#Find duplicates
table(emp$Code)

#Find common names
table(emp$First.Name)

#----------------------------------------------------------#
emp_qual <- read.csv("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Tut3-EDA_Data_Preparation/employees_qual.csv")

#Combining multiple sources using Merge and dplyr joins
#Inner join: Returns rows where a matching value for the variable is found in both
merge(emp, emp_qual, by ="Code")

#Left join: Returns all rows from the first data frame even if a matching value for the second is not found
merge(emp, emp_qual, by ="Code", all.x =TRUE)

#Right join: Returns all rows from second data frame even if a matching value for the 1st is not found
merge(emp, emp_qual, by ="Code", all.y =TRUE)

#Full Join: Returns all rows from the first and second data frame
merge(emp, emp_qual, by ="Code", all =TRUE)

#Joins using dplyr library
install.packages("dplyr")
library(dplyr)
inner_join(emp, emp_qual, by ="Code")
left_join(emp, emp_qual, by ="Code")
right_join(emp, emp_qual, by ="Code")
full_join(emp, emp_qual, by ="Code")

##########################################################Data Vu=isualization###############

# Installation
#The easiest way to get ggplot2 is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just ggplot2:
install.packages("ggplot2")

library(ggplot2)
mpg    #data set that’s bundled with ggplot2. 
       #includes information about the fuel economy of popular car models
       #in 1999 and 2008, collected by the US Environmental Protection Agency

#***********************************************************************************

#bar chart
class <- c("Undergrad", "postgrad","grad")
count<- c(28,3,9)

d <- data.frame(class, count)
d
barplot(d$count, main = "Enroll Chart",
        xlab="Level", ylab="Enrollment",
        col="skyblue")


d <- read.csv("http://lessRstats.com/data/employee.csv")
ggplot(d, aes(Dept)) + geom_bar() + coord_flip()



#************************************************************************************
#*Melt Function in R
#*built-in function to reshape & elongate the dataframe

install.packages("MASS") 
install.packages("reshape2") 
install.packages("reshape") 

library(MASS) 
library(reshape2) 
library(reshape) 


A <- c(1,2,3,4,2,3,4,1) 
B <- c(1,2,3,4,2,3,4,1) 
a <- c(10,20,30,40,50,60,70,80) 
b <- c(100,200,300,400,500,600,700,800) 
data <- data.frame(A,B,a,b) 

print("Original data frame:\n") 
print(data) 

melt_data <- melt(data, id = c("A","B")) 

print("Reshaped data frame:\n") 
print(melt_data)


#************************************************************************************
#*Histogram
#*
# Population
install.packages("reshape"); library(reshape)

Population_all <- read.csv("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Dataset - Data Visualization/Population All Year.csv")
Population_all
Population_all_Long_Format <- melt(Population_all, id = "Country")
Population_all_Long_Format
names(Population_all_Long_Format) <- c("Country", "Year", "Pop_Billion")

#use substr() to extract parts of character string in year
#syntax:
  #substr(text, start, stop)
  #substring(text, first, last = 1000000L)
Population_all_Long_Format$Year <- substr(Population_all_Long_Format$Year, 2,length(Population_all_Long_Format$Year))
Population_all_Long_Format

#Developed Country
Population_Developed <- Population_all_Long_Format[!(Population_all_Long_Format$Country %in% c('India','China','Australia','Brazil','Canada','France','United States')),]
Population_Developed

ggplot(Population_Developed, aes(Pop_Billion, fill = Country)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..),col="black") + 
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Population (in Billion)") +
  ylab("Frequency") +
  ggtitle("Population (in Billion): Histogram")


#**************************************************************************************************
##Density

library(ggplot2)

ggplot(Population_Developed, aes(Pop_Billion, fill = Country)) + 
  geom_density(alpha = 0.2, col="black") +
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Population (in Billion)") +
  ylab("Frequency") +
  ggtitle("Population (in Billion): Density")

#*******************************************************************************************
#Box plot

# GDP

GDP_all <- read.csv("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Dataset - Data Visualization/GDP All Year.csv")
GDP_all_Long_Format <- melt(GDP_all, id = "Country")
names(GDP_all_Long_Format) <- c("Country", "Year", "GDP_USD_Trillion")
GDP_all_Long_Format$Year <- substr(GDP_all_Long_Format$Year, 2,length(GDP_all_Long_Format$Year))


ggplot(GDP_all_Long_Format, aes(factor(Country), GDP_USD_Trillion)) +
  geom_boxplot(aes(fill = factor(Country)))+
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Country") +
  ylab("GDP (in Trillion US $)") +
  ggtitle("GDP (in Trillion US $): Boxplot - Top 10 Countries")


#*******************************************************************************************
#*
#Scatter plot
GDP_Pop <- read.csv("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Dataset - Data Visualization/GDP and Population 2015.csv")

ggplot(GDP_Pop, aes(x=Population_Billion, y=GDP_Trilion_USD))+
  geom_point(aes(color=Country),size = 5) + 
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Population ( in Billion)") +
  ylab("GDP (in Trillion US $)") +
  ggtitle("Population Vs GDP - Top 10 Countries")

#*******************************************************************************************
#Bubble Chart
library(corrplot)
library(reshape2)
library(ggplot2)
library(scales)

#Bubble chart

bc <- read.delim("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Dataset - Data Visualization/BubbleChart_GapMInderData.txt")
bc_clean <- droplevels(subset(bc, continent != "Oceania"))
str(bc_clean)

bc_clean_subset <- subset(bc_clean, year == 2007)
bc_clean_subset$year = as.factor(bc_clean_subset$year)

ggplot(bc_clean_subset, aes(x = gdpPercap, y = lifeExp)) + scale_x_log10() +
  geom_point(aes(size = sqrt(pop/pi)), pch = 21, show.legend = FALSE) +
  scale_size_continuous(range=c(1,40)) + 
  facet_wrap(~ continent) +
  aes(fill = continent) +
  scale_fill_manual(values = c("#FAB25B", "#276419", "#529624", "#C6E79C")) +
  xlab("GDP Per Capita(in US $)")+
  ylab("Life Expectancy(in years)")+
  ggtitle("Bubble Chart - GDP Per Captita Vs Life Expectency") +
  theme(text=element_text(size=12),
        title=element_text(size=14,face="bold"))


#*************************************************************************************************
#*Correlation Plot
#*more on correlation plots from: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
install.packages("corrplot")

library(corrplot)
library(reshape2)
library(ggplot2)

correlation_world <- read.csv("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Dataset - Data Visualization/Correlation Data.csv")

corrplot(cor(correlation_world[,2:6],method = "pearson"),diag = FALSE,
         method = "ellipse",
         tl.cex = 0.7, tl.col = "black", cl.ratio = 0.2
)


#*************************************************************************************************
#Heat Map
#use the built-in R dataset mtcars

#load reshape2 package to use melt() function
library(reshape2)

#melt mtcars into long format
melt_mtcars <- melt(mtcars)

#add column for car name
melt_mtcars$car <- rep(row.names(mtcars), 11)

#view first six rows of melt_mtcars
head(melt_mtcars)

#use rescale to enhance color variation of variables

#load libraries
library(plyr)
library(scales)

#rescale values for all variables in melted data frame
melt_mtcars <- ddply(melt_mtcars,.(variable), transform, rescale = rescale(value))

#create heatmap using rescaled values
ggplot(melt_mtcars, aes(variable, car)) +
  geom_tile(aes(fill = rescale), colour = "white") +
  scale_fill_gradient(low = "white", high = "red")


#********************************************************************************************

World_Comp_GDP <- read.csv("C:/Users/OLUBISI/Desktop/Egypt AI Course/DTI5126 - Fundamentals of Data Science/Tutorials/Dataset - Data Visualization/World GDP and Sector.csv")

World_Comp_GDP_Long_Format <- melt(World_Comp_GDP, id = "Sector")
names(World_Comp_GDP_Long_Format) <- c("Sector", "Year", "USD")

World_Comp_GDP_Long_Format$Year <- substr(World_Comp_GDP_Long_Format$Year, 2,length(World_Comp_GDP_Long_Format$Year))

# calculate midpoints of bars

World_Comp_GDP_Long_Format_Label <- ddply(World_Comp_GDP_Long_Format, .(Year), 
                                          transform, pos = cumsum(USD) - (0.5 * USD))

ggplot(World_Comp_GDP_Long_Format_Label, aes(x = Year, y = USD, fill = Sector)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = USD, y = pos), size = 3) +
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Year") +
  ylab("% of GDP") +
  ggtitle("Contribution of various sector in the World GDP")
