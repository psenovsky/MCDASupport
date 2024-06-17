# Function to check integrity of the function parameters for functions
# Electre_4 and Electre_4_sensitivity
#
# note this function is also being used for checking of subset of Electre 3
# and 1S parameters (Electre_3_paramCheck), which are shared acros Electre 3
# and 4
#
# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   P  - preference threshold
#   Q  - indifference threshold
#   V  - veto threshold
Electre_4_paramCheck <- function(PM, P, Q, V, minmaxcriteria = "max") {

  # with < 2 criteria or alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(PM) || (is.data.frame(PM)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (!is.numeric(unlist(PM))) {
    stop("Only numeric values in performance matrix expected")
  }
  ncri  <- ncol(PM)  #no. of criteria
  cri  <- colnames(PM)
  t <- c(P, Q, V)
  if (!is.vector(t, mode = "numeric")) {
    stop("all thresholds must be a numeric vectors")
  }
  t <- c(length(P), length(V), length(Q))
  if (!all(t == ncri)) {
    stop("number of elements in thresholds != number of criteria")
  }
  if (all(P == Q)) {
    stop("Preference and indifference thresholds are defined same. 
          Revisit to distinguish them or use another method.")
  }

  # check consistency of the thresholds
  if (any(Q < 0 | Q > P | P >= V)) {
    print("Problem with consistency of thresholds:")
    print(paste0("  - Q < 0 for criteria:", cri[Q < 0]))
    print(paste0("  - Q > P for criteria:", cri[Q > P]))
    print(paste0("  - P >= V for criteria:", cri[P >= V]))
    stop("correct the thresholds please")
  }
}
