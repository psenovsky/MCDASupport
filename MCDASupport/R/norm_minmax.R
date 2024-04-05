norm_minmax <- function(tonorm, minmax = 'max'){
  #validate input
  if(minmax != 'max' && minmax != 'min') stop('minmax parameter expected to be max or min.')
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numeric vector.')

  minimum <- min(tonorm)
  maximum <- max(tonorm)

  if(maximum == minimum) stop('All values same, nothing to normalize')

  z <- (tonorm - minimum)/(maximum - minimum)
  if(minmax == 'min') z <- 1 - z
  return(z)
}
