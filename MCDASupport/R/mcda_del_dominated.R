# function removes dominated alternatives from the results
# parameters
#   M - performance matrix
#   minmaxcriteria - the direction of the criteria
#   digit - number of digits to round to
mcda_del_dominated <- function(M, minmaxcriteria = 'max', digits = 2) {
  #check validity of parameters
  if(!is.numeric(digits) || digits < 0 ) stop("digits must be number >= 0")

  Dom <- mcda_get_dominated(M, minmaxcriteria)
  out <- round(M[rowSums(Dom$dominationMatrix) == 0,], digits)
  return(out)
}
