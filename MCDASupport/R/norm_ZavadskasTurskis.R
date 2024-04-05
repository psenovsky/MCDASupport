norm_ZavadskasTurskis <- function(tonorm, minmax = 'max'){
  #validate input
  if(minmax != 'max' && minmax != 'min') stop('minmax parameter expected to be max or min.')
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numeric vector.')

  maximum <- max(tonorm)
  minimum <- min(tonorm)
  if(minmax == 'max'){
    z <- 1 - abs((maximum - tonorm)/maximum)
  }else{
    z <- 1 - abs((minimum - tonorm)/minimum)
  }
  return(z)
}
