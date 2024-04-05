# Entropy weight method
#
# parametrs
#   PM - performance matrix of the alternatives in the criteria (criteria in columns and alternatives in rows)
#   minmaxcriteria - vector of min/max for the criteria to se direction of the optimisation
#   VERBOSE - TRUE to print output to screen
EWM <- function(PM, minmaxcriteria = 'max', VERBOSE = FALSE){

  ## check validity of the objects manipulated by the current function
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(PM) || (is.data.frame(PM)))) stop('wrong performanceMatrix, should be a matrix or a data frame')
  if (!is.numeric(unlist(PM))) stop('Only numeric values in performance matrix expected')
  PM <- util_pm_minmax(PM, minmaxcriteria) #validate minmax and invert scales if necessary
  nalt <- nrow(PM)
  ## End of checking the validity of the "inputs"

  # function computes maximum in columns in matrix or data frame
  colMax <- function(data){
    t <- apply(data, 2, max)
    return(t)
  }

  EM <- sweep(PM, 2, colMax(PM), "/") # 1. normalize PM: EM = PM_ij/max(PM_j)
  # sum_{i=1}^n EM_{ij}, where j = criteria, i = alternatives, n = number of alternatives
  p <- sweep(EM, 2, colSums(EM), "/") # probability of criteria to occur
  p2 <- p * log(p)
  na_values <- is.na(p2) # required since if p = 0, then ln(0) = -inf, so 0 * ln(0) = NaN
  p2[na_values] <- 0
  Ej <- -(1/log(nalt)) * colSums(p2) # E_j = -P sum_{i=1}^n p_{ij} * log_e (p_{ij})
  divj <- abs(1 - Ej) # degree of divergence
  # Entropy weight
  Ewj <- divj/sum(divj) # Ew_j = div_j / sum_{j=1}^m div_j

  if(VERBOSE){
    print('Normalized performance matrix')
    print(EM)
    print('probability of the criteria to occur')
    print(p2)
    print('degree of divergence')
    print(divj)
    print('entropy weights for the criteria')
    print(Ewj)
  }

  out <- list(
    normalizedPM = EM,
    probabilityCriteria = p2,
    degreeDivergence = divj,
    entropyWeight = Ewj
  )
  return(out)
}
