#analyze the revenue by year and month.
apply(revenue_cube, c("year", "month"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

#analyze the revenue by product and location
apply(revenue_cube, c("prod", "loc"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
