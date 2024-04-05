# Function responsible for checking consistency of Electre 1S input parametrs. Function is also utilized by Electre_1S_sensitivity.

# parameters:
#   PM             - performance matrix
#   w              - weights of the criteria
#   Q              - indifference threshold
#   P              - preference thresholds
#   V              - veto thresholds
#   minmaxcriteria - vector with identification of direction of the criteria
#   lambda         - cut-off criterion
Electre_1S_paramCheck <- function(PM, w, Q, P, V, minmaxcriteria = 'max', lambda = 0.5, VERBOSE = FALSE){

  ## input data consistency check
  Electre_4_paramCheck(PM = PM, P = P, Q = Q, V = V, minmaxcriteria = minmaxcriteria)
  ncri <- ncol(PM)  #no. of criteria
  if (!is.vector(w)) stop('criteriaWeights should be a vector')
  if (!is.numeric(w)) stop('criteriaWeights should be a numeric vector')
  if (ncri != length(w)) stop('length of criteriaWeights should be checked')
  if (!is.null(lambda) && !is.numeric(lambda)) stop('if lambda is set, it must be numeric value')
  if (lambda < 0.5 || lambda > 1) stop('Lambda value must be in interval <0.5;1>')
  ## End of checking the validity of the "inputs"

}
