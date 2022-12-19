
# Setup the dimension tables

state_table <- 
  data.frame(key=c("CA", "NY", "WA", "ON", "QU"),
             name=c("California", "new York", "Washington", "Ontario", "Quebec"),
             country=c("USA", "USA", "USA", "Canada", "Canada"))

month_table <- 
  data.frame(key=1:12,
             desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))

prod_table <- 
  data.frame(key=c("Printer", "Tablet", "Laptop"),
             price=c(225, 570, 1120))


#********************Create function to generate Fact table********************************************

# Function to generate the Sales table
gen_sales <- function(no_of_recs) {
  
  # Generate transaction data randomly
  loc <- sample(state_table$key, no_of_recs, 
                replace=T, prob=c(2,2,1,1,1))
  time_month <- sample(month_table$key, no_of_recs, replace=T)
  time_year <- sample(c(2012, 2013), no_of_recs, replace=T)
  prod <- sample(prod_table$key, no_of_recs, replace=T, prob=c(1, 3, 2))
  unit <- sample(c(1,2), no_of_recs, replace=T, prob=c(10, 3))
  amount <- unit*prod_table[prod,]$price
  
  sales <- data.frame(month=time_month,
                      year=time_year,
                      loc=loc,
                      prod=prod,
                      unit=unit,
                      amount=amount)
  
  # Sort the records by time order
  sales <- sales[order(sales$year, sales$month),]
  row.names(sales) <- NULL
  return(sales)
}

# Now create the sales fact table
sales_fact <- gen_sales(500)

# Look at a few records
head(sales_fact)



#************************BUILD A CUBE*********************************************
# Build up a cube
revenue_cube <- 
  tapply(sales_fact$amount, 
         sales_fact[,c("prod", "month", "year", "loc")], 
         FUN=function(x){return(sum(x))})

# Showing the cells of the cube
revenue_cube

dimnames(revenue_cube)

#*************************SLICE****************************************

# List the sales happening in "2012", "Jan", or query for the sales happening in "2012", "Jan", only "Tablet".

# cube data in Jan, 2012
revenue_cube[, "1", "2012",]

# cube data in Jan, 2012
revenue_cube["Tablet", "1", "2012",]

#************************DICE************************************

#5...sales happening in Jan/ Feb/Mar, Laptop/Tablet, CA/NY
revenue_cube[c("Tablet","Laptop"), 
             c("1","2","3"), 
             ,
             c("CA","NY")]


#*************************ROLLUP***********************************
# annual revenue for each product and collapse the location dimension;
apply(revenue_cube, c("year", "prod"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})


#**********************DRILL DOWN************************************
#*#annual and monthly revenue for each product and collapse the location dimension;

apply(revenue_cube, c("year", "month", "prod"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})


#****************PIVOT*******************************************
#*#analyze the revenue by year and month.
apply(revenue_cube, c("year", "month"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

#analyze the revenue by product and location
apply(revenue_cube, c("prod", "loc"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
