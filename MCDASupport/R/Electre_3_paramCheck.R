# Common function to check integrity of the function parameters for functions Electre_3 and Electre_3_sensitivity
#
# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   w  - weights
#   P  - preference threshold
#   Q  - indifference threshold
#   V  - veto threshold
#   alpha - first discrimination threshold
#   beta  - second discrimination threshold
Electre_3_paramCheck <- function(PM, w, P, Q, V, minmaxcriteria = 'max', alpha = 0.3, beta = 0.15){

  # use shared checks for the method (Electre 4 uses subset of Electre 3 parameters)
  # only values checked - computationally the methods are different
  Electre_4_paramCheck(PM = PM, P = P, Q = Q, V = V, minmaxcriteria = minmaxcriteria)

  # specific checks for Electre 3
  if (!is.vector(w)) stop('criteria weights should be a vector')
  if (!is.numeric(w)) stop('criteria weights should be a numeric vector')
  ncri <- ncol(PM)  #no. of criteria
  if (ncri != length(w)) stop('number of criteria weights must be same as number of criteria')
  if (!is.numeric(alpha)) stop('alpha discrimination threshold should be a number')
  if (!is.numeric(beta)) stop('beta discrimination threshold should be a number')
  if (alpha < beta) stop('alpha shoud be > then beta (discrimination threshold)')

}
