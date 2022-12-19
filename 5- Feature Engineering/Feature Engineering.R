#Data Extraction

#url.data <- 'https://data.cityofchicago.org/api/views/x2n5-8w5q/rows.csv?accessType¼DOWNLOAD'
crime.data <- read.csv("C:/Users/olubi/Desktop/Egypt DTI5126 - Summer 2022/Tutorials/Tut6 - Feature Engineering/Crimes_-_One_year_prior_to_present.csv", na.strings= '')

#Data Exploration
str(crime.data) #use str() to display internal structure of data neatly

#str() is a compact version of summary(), which provides a detailed summary of the data
summary(crime.data)

#Duplicated values
# CASE is the identifier for the incidence. CASE should have all unique values
# There are 3 rows in the data that have a case value equal to HT572234
#These duplicated rows need to be removed. We can do this using a combination of the subset() and the duplicated() function.
crime.data <- subset(crime.data, !duplicated(crime.data$CASE.))
summary(crime.data)

#Missing Values - Most raw data sets will have issues like duplicated rows, missing values, incorrectly imputed values, and outliers
crime.data <- subset(crime.data, !is.na(crime.data$LATITUDE))
crime.data <- subset(crime.data, !is.na(crime.data$WARD))

#Remove illogical values in certain rows, e.g., one of the values in the CASE variable is "CASE#."
crime.data <- crime.data[crime.data$CASE. != 'CASE#',]

#head() function shows the first few observations of the data/column
head(crime.data$DATE..OF.OCCURRENCE)

#Currently, date is stored as a factor variable. To make R recognize that it is in fact a date, we
#need to present it to R as a date object. Use as.POSIXlt() function
crime.data$date <- as.POSIXlt(crime.data$DATE..OF.OCCURRENCE, format= "%m/%d/%Y %H:%M")
head(crime.data$date)

#separate the time stamps from the date part using the times()function
install.packages("chron")
library(chron)
crime.data$time <- times(format(crime.data$date, "%H:%M:%S"))
head(crime.data$time)

#To check time intervals of the day where criminal activity is more prevalent, bucket the timestamps into a few categories and then see the distribution across the buckets 
time.tag <- chron(times= c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:59:00"))
time.tag

crime.data$time.tag <- cut(crime.data$time, breaks=time.tag,
                           labels =c("00-06","06-12", "12-18", "18-00"), include.lowest= TRUE)
table(crime.data$time.tag) #The distribution of crime incidents across the day suggests that crimes are more frequent during the earlierhalf of the day

#recode the date variable to contain just the date part by stripping the timestamps.
crime.data$date <- as.POSIXlt(strptime(crime.data$date, format = "%Y-%m-%d"))
head(crime.data$date)

#use the date of incidence to determine which day of the week and which month of the year the crime occurred
crime.data$day <- weekdays(crime.data$date, abbreviate= TRUE)
crime.data$month <- months(crime.data$date, abbreviate=TRUE)


#Use the primary description to categorize different crime types.
#data contain about 31 crime types, not all of which are mutually exclusive. We can combine
#two or more similar categories into one to reduce this number and make the analysis a bit easier

table(crime.data$PRIMARY.DESCRIPTION)
length(unique(crime.data$PRIMARY.DESCRIPTION))

crime.data$crime <- as.character(crime.data$PRIMARY.DESCRIPTION)

crime.data$crime <- ifelse(crime.data$crime %in% c("CRIM SEXUAL ASSAULT",
                                                   "PROSTITUTION", "SEX OFFENSE"), "SEX", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime %in% c("MOTOR VEHICLE THEFT"),
                           "MVT", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("GAMBLING", "INTERFERE WITH PUBLIC OFFICER", 
                                                   "INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION",
                                                   "LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION",
                                                   "PUBLIC INDECENCY", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"),
                           "NONVIO", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime == "CRIMINAL DAMAGE", "DAMAGE", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime == "CRIMINAL TRESPASS", "TRESPASS", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATION",
                                                   "OTHER NARCOTIC VIOLATION"), "DRUG", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime == "DECEPTIVE PRACTICE","FRAUD", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("OTHER OFFENSE", "OTHER OFFENSE"), "OTHER", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("KIDNAPPING", "WEAPONS VIOLATION", 
                                                   "OFFENSE INVOLVING CHILDREN"), "VIO", crime.data$crime)

table(crime.data$crime)

#The data on whether the crime incident led to an arrest or not is currently stored with Yes/No inputs. Convert to numeric
crime.data$ARREST <- ifelse(as.character(crime.data$ARREST) == "Y", 1, 0)


#Visualizations - Visualizing Temporal elements

library(ggplot2)
qplot(crime.data$crime, xlab = "Crime", main ="Crimes in Chicago") + 
    scale_y_continuous("Number of crimes")

qplot(crime.data$time.tag, xlab ="Time of day", main = "Crimes by time of day") +
  scale_y_continuous("Number of crimes")

crime.data$day <- factor(crime.data$day, levels= c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

qplot(crime.data$day, xlab = "Day of week", main = "Crimes by day of week ") +
  scale_y_continuous("Number of crimes")

crime.data$month <- factor(crime.data$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                                      ""Aug", "Sep", "Oct", "Nov", "Dec"))

qplot(crime.data$month, xlab = "Month", main= "Crimes by month") + 
  scale_y_continuous("Number of crimes")

#see how different crimes vary by different times of the day, roll-up the data.
#An easy way to roll-up data is by using the aggregate() function
temp <- aggregate(crime.data$crime, by = list(crime.data$crime,
                                           crime.data$time.tag), FUN = length)
names(temp) <- c("crime", "time.tag", "count")

#To construct the plot, we use the ggplot() function from the ggplot2
ggplot(temp, aes(x = crime, y = factor(time.tag))) +
  geom_tile(aes(fill = count)) +
  scale_x_discrete("Crime", expand = c(0,0)) +
  scale_y_discrete("Time of day", expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes", low = "white", high = "steelblue") +
  theme_bw() +
  ggtitle("Crimes by time of day") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line
      (colour = NA))



