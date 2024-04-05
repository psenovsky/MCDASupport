norm_nonlinear <- function(tonorm, minmax = 'max'){
  #validate input
  if(minmax != 'max' && minmax != 'min') stop('minmax parameter expected to be max or min.')
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numeric vector.')

  if(minmax == 'max'){
    z <- (tonorm/max(tonorm))^2
  }else{
    z <- (min(tonorm)/tonorm)^2
  }
  return(z)
}
