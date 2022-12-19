##  Identifying Frequently-Purchased Groceries with Association Rules
## Perform market basket analysis for a month's worth of transactions at a moderately-sized supermarket

##Step 1: Data collection
## Utilize purchase data from one month of operation at a real-world grocery store. 
## The data contain 9,835 transactions, or about 327 transactions/day 
## Transactional data is stored in free-form (not as rows & columns). Each row represent a single transaction

## Step 2: Exploring and preparing the data ----

# load the grocery data into a sparse matrix
library(arules)
library(arulesViz)
data("Groceries")
class(Groceries)

summary(Groceries)

inspect(head(Groceries, 12))
# look at the first five transactions
inspect(Groceries[1:5])

# examine the frequency of items
itemFrequency(Groceries[, 1:3])

# plot the frequency of items
itemFrequencyPlot(Groceries, support = 0.1)
itemFrequencyPlot(Groceries, topN = 20)


# visualization of a random sample of 100 transactions
image(sample(Groceries, 100))

## Step 3: Training a model on the data ----
library(arules)

# default settings result in zero rules learned
apriori(Groceries)

# set better support and confidence levels to learn more rules
groceryrules <- apriori(Groceries, parameter = list(support =
                                                      0.006, confidence =0.25, minlen = 2),
                        appearance = list(lhs="whole milk", default="rhs"))
                        
groceryrules


## Step 4: Evaluating model performance ----
# summary of grocery association rules
summary(groceryrules)

# look at the first three rules
inspect(groceryrules[1:3])

## Step 5: Improving model performance ----

# sorting grocery rules by lift to determine actionable rules
inspect(sort(groceryrules, by = "lift")[1:5])

# finding subsets of rules containing any berry items
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

# writing the rules to a CSV file
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# converting the rule set to a data frame
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

