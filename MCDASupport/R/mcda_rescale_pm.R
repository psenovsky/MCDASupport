# function rescales the performance matrix
# parameters:
#   M - performance matrix
mcda_rescale_pm <- function(m) {
  #parameter validity check
  if (!(is.matrix(m) || (is.data.frame(m)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (ncol(m) < 2 || nrow(m) < 2) stop("less than 2 criteria or 2 alternatives")
  i <- sapply(m, is.numeric)
  if (length(i[i %in% FALSE]) > 0) stop("Performance matrix must be numeric")

  col_maxs <- function(m) apply(m, 2, max, na.rm = TRUE)
  col_mins <- function(m) apply(m, 2, min, na.rm = TRUE)
  m <- sweep(m, 2, col_mins(m), FUN = "-")
  m <- sweep(m, 2, col_maxs(m) - col_mins(m), FUN = "/")
  return(m)
}
