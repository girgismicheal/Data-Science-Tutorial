
# Build up a cube
revenue_cube <- 
    tapply(sales_fact$amount, 
           sales_fact[,c("prod", "month", "year", "loc")], 
           FUN=function(x){return(sum(x))})

# Showing the cells of the cude
revenue_cube

dimnames(revenue_cube)
