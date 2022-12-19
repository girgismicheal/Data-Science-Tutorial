# annual revenue for each product and collapse the location dimension;
apply(revenue_cube, c("year", "prod"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
