norm_linearagreg <- function(tonorm, minmax = 'max'){
  #validate input
  if(minmax != 'max' && minmax != 'min') stop('minmax parameter expected to be max or min.')
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numeric vector.')

  sx <- sum(tonorm)
  if(sx == 0) stop('sum of all values = 0, use different normalization method.')
  if(minmax == 'max'){
    z <- tonorm/sx
  }else{
    z <- (1/tonorm)/sum(1/tonorm)
  }
  return(z)
}
