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

#What would be the improvement you get by populating prod table this way:
prod_table <-data.frame("key"=factor(x=key,levels=c("Printer","Tablet","Laptop"),ordered=TRUE),
             price =c(225, 570, 1120))
