norm_tobest <- function(tonorm){
  #validate input
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numeric vector.')

  maximum <- max(tonorm)
  if(maximum == 0) stop('Maximal value in the vector is 0, unable to normalize using this function.')
  z <- 100 * tonorm/maximum
  return(z)
}
