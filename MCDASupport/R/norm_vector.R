norm_vector <- function(tonorm, minmax = 'max'){
  #validate input
  if(minmax != 'max' && minmax != 'min') stop('minmax parameter expected to be max or min.')
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numeric vector.')

  x2 <- tonorm * tonorm
  if(minmax == 'max'){
    z <- tonorm / sqrt(sum(x2))
  }else{
    z <- (1/tonorm)/sqrt(sum(1/x2))
  }
  return(z)
}
