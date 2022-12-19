#annual and monthly revenue for each product and collapse the location dimension;

apply(revenue_cube, c("year", "month", "prod"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
