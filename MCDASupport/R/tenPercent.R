# returns sequence of from -10 % of x to +10 % of x by 1 %
#
# param
#   x ... numeric value
tenPercent <- function(x){
  if(!is.numeric(x)) stop('Numeric value as base for percentage computation expected')
  if(x != 0){
    t <- seq(from = x - x/10, to = x + x/10, by = x/100)
  }else{
    t <- rep(0, times = 21)
  }
  return(t)
}
