norm_logarithm <- function(tonorm, minmax = 'max'){
  #validate input
  if(minmax != 'max' && minmax != 'min') stop('minmax parameter expected to be max or min.')
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numeric vector.')

  p <- Reduce("*", tonorm)
  z <- log(tonorm)/p
  if(minmax == 'min') z <- (1 - z)/(length(tonorm) - 1)
  return(z)
}
