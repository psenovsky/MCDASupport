norm_zscore <- function(tonorm){
  #validate input
  if(!is.vector(tonorm)) stop('tonorm paramerer expected to be vector.')
  if(!is.numeric(tonorm)) stop('tonorm expected to be numerix vector.')

  z <- (tonorm - mean(tonorm))/sd(tonorm)
  return(z)
}
