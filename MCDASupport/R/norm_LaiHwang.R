norm_LaiHwang <- function(tonorm, minmax = 'max'){
  #validate input
  if(minmax != 'max' && minmax != 'min') stop('minmax parameter expected to be max or min.')
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numeric vector.')

  maximum <- max(tonorm)
  minimum <- min(tonorm)
  if(minmax == 'max'){
    z <- tonorm/(maximum - minimum)
  }else{
    z <- tonorm/(minimum - maximum)
  }
  return(z)
}
