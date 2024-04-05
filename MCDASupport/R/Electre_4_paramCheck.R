# Function to check integrity of the function parameters for functions Electre_4 and Electre_4_sensitivity
#
# note this function is also being used for checking of subset of Electre 3 and 1S parameters (Electre_3_paramCheck), which are shared acros Electre 3 and 4
#
# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   P  - preference threshold
#   Q  - indifference threshold
#   V  - veto threshold
Electre_4_paramCheck <- function(PM, P, Q, V, minmaxcriteria = 'max'){

  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop('less than 2 criteria or 2 alternatives')
  if (!(is.matrix(PM) || (is.data.frame(PM)))) stop('wrong performance matrix, should be a matrix or a data frame')
  if (!is.numeric(unlist(PM))) stop('Only numeric values in performance matrix expected')
  ncri  <- ncol(PM)  #no. of criteria
  cri  <- colnames(PM)
  if (!is.vector(P)) stop('preference thresholds should be a vector')
  if (!is.numeric(P)) stop('preference thresholds should be a numeric vector')
  if (length(P) != ncri) stop('number of preference thresholds != number of criteria')
  if (!(is.vector(Q))) stop('indifference thresholds should be a vector')
  if (!is.numeric(Q)) stop('indifference thresholds be a numeric vector')
  if (length(Q) != ncri) stop('number of indifference thresholds != number of criteria')
  if (!is.vector(V)) stop('veto thresholds should be a vector')
  if (!is.numeric(V)) stop('veto thresholds be a numeric vector')
  if (length(V) != ncri) stop('number of veto thresholds != number of criteria')
  if (all(P == Q)) stop('Preference and indifference thresholds are defined same. Revisit to distinguish them or use another method.')

  prob <- 0
  for(j in 1:ncri){ # check consistency of the thresholds
    # 0 <= Qj <= Pj < Vj
    if(0 > Q[j]) {
      prob <- prob + 1
      print(cat("Q < 0 for criterion: ", cri[j]))
    }
    if(Q[j] > P[j]){
      prob <- prob + 1
      print(cat("Q > P for criterion: ", cri[j]))
    }
    if(P[j] >= V[j]){
      prob <- prob + 1
      print(cat("P >= V for criterion: ", cri[j]))
    }
    if(prob > 0) stop("Problem with consistency of threshold, see notes above.")
  }
}
