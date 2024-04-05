# function rescales the performance matrix
# parameters:
#   M - performance matrix
mcda_rescale_pm <- function(M) {

  #parameter validity check
  if (!(is.matrix(M) || (is.data.frame(M)))) stop('wrong performance matrix, should be a matrix or a data frame')
  if (ncol(M) < 2 || nrow(M) < 2) stop('less than 2 criteria or 2 alternatives')
  i <- sapply(M, is.numeric)
  if (length(i[i %in% F]) > 0) stop('Performance matrix must be numeric')

  colMaxs <- function(M) apply(M, 2, max, na.rm = T)
  colMins <- function(M) apply(M, 2, min, na.rm = T)
  M <- sweep(M, 2, colMins(M), FUN = "-")
  M <- sweep(M, 2, colMaxs(M) - colMins(M), FUN = "/")
  return(M)
}
