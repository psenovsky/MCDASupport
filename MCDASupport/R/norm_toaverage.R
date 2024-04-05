norm_toaverage <- function(tonorm){
  #validate input
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numerix vector.')

  av <- mean(tonorm)
  if(av == 0) stop('Mean of the tonorm vector is zero, so it is imposible to normalize it using this function.')

  z <- 100 * tonorm / av
  return(z)
}
