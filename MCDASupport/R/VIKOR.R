# VIKOR function
#
# parameters
#   PM - performance matrix
#   w - weights
#   minmax - min/max value or vector of mixed values for criteria orientation
#   v - weight for strategy of majority of the criteria (0-1)
#   VERBOSE - T to dump results into console
VIKOR <- function(PM, w, minmax = "max", v = NULL, VERBOSE = FALSE) {
  ## check validity of the objects manipulated by the current function
  # with < 2 criteria or 2 alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(PM) || (is.data.frame(PM)))) {
    stop("wrong performanceMatrix, should be a matrix or a data frame")
  }
  if (!is.numeric(unlist(PM))) {
    stop("Only numeric values in performance matrix expected")
  }
  PM <- util_pm_minmax(PM, minmax) #validate minmax and invert scales if neccessary
  if (!(is.vector(w, mode = "numeric"))) {
    stop("criteriaWeights should be a vector")
  }
  if (ncol(PM) != length(w)) stop("length of criteriaWeights should be checked")
  if (is.null(v)) {
    v <- (ncri + 1) / (2 * ncri)
  }else if (!is.numeric(v)) {
    stop("weight of the strategy v must be numeric value (or NULL)
         for procedure to work")
  }else if (v < 0 || v > 1) {
    stop("weight of the strategy v must be in interval 0-1 (or NULL).")
  }
  ## End of checking the validity of the "inputs"

  ncri <- ncol(PM)  #no. of criteria
  cri  <- colnames(PM)

  #Step 1. Determine the Best and the Worst Values of All Criteria Functions
  #f*j = f_max, f-j = f_min
  f_max <- PM %>%
    group_by() %>%
    summarise(across(1:ncri, max)) %>%
    unlist()
  f_min <- PM %>%
    group_by() %>%
    summarise(across(1:ncri, min)) %>%
    unlist()

  #Step 2. Compute the Values Si and Ri (validated)
  bw <- cbind(f_max, f_min, f_max - f_min)
  rownames(bw) <- cri
  SR <- VIKORIndexes(PM, bw, w, v)
  # here original VIKOR stops, other steps implemented in extended version of
  # VIKOR

  #print results
  if (VERBOSE) {
    print("S metric")
    print(SR$S)
    print("R metric")
    print(SR$R)
    print("Q metric")
    print(SR$Q)
    print("Compromise solution")
    print(unlist(SR$compromiseSolution))
  }

  out <- list(
    S = SR$S,
    R = SR$S,
    Q = SR$Q,
    compromiseSolution = SR$compromiseSolution
  )
  return(out)
}