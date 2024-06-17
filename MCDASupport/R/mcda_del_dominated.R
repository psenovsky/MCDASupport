# function removes dominated alternatives from the results
# parameters
#   M - performance matrix
#   minmaxcriteria - the direction of the criteria
#   digit - number of digits to round to
mcda_del_dominated <- function(m, minmaxcriteria = "max", digits = 2) {
  #check validity of parameters
  if (!is.numeric(digits) || digits < 0) stop("digits must be number >= 0")

  dom <- mcda_get_dominated(m, minmaxcriteria)
  out <- round(m[rowSums(dom$dominationMatrix) == 0, ], digits)
  return(out)
}
