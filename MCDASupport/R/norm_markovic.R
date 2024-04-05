norm_markovic <- function(tonorm){
  #validate input
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numeric vector.')

  maximum <- max(tonorm)
  minimum <- min(tonorm)
  z <- 1 - (tonorm - minimum)/maximum
  return(z)
}
