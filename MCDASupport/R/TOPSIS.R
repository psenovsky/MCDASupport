# TOPSIS function
#
# parameters
#   PM - performance matrix
#   w - weights
#   minmax - min/max value or vector of mixed values for criteria orientation
#   VERBOSE - T to dump outputs to console
TOPSIS <- function(PM, w, minmax = "max", VERBOSE = FALSE) {
  ## check validity of the objects manipulated by the current function
  # with < 2 criteria or 2 alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(PM) || (is.data.frame(PM)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (!is.numeric(unlist(PM))) {
    stop("Only numeric values in performance matrix expected")
  }
  PM <- util_pm_minmax(PM, minmax) #validate minmax and invert scales if neccessary
  PM <- as.data.frame(PM)
  if (!(is.vector(w, mode = "numeric"))) {
    stop("criteria weights should be a vector")
  }
  if (ncol(PM) != length(w)) {
    stop("length of criteria weights should be checked")
  }
  ## End of checking the validity of the "inputs"

  ncri <- ncol(PM)  #no. of criteria
  alt  <- rownames(PM)
  cri  <- colnames(PM)

  #Step 1. Normalization of the Decision Matrix (r) (validated)
  r <- sapply(1:ncri, function(j) PM[, j] / sqrt(sum(PM[, j] * PM[, j])))
  colnames(r) <- cri
  rownames(r) <- alt

  # Step 2. Calculation of the Weighted Normalized Decision Matrix (v)
  v <- sweep(r, MARGIN = 2, w, `*`)
  colnames(v) <- cri
  rownames(v) <- alt
  v <- as.data.frame(v)

  # steps 3 - 5 in topsis_ideal
  top_i <- topsis_ideal(v)
  #Step 6. order by C
  ordered_c <- sort(top_i$closenes, decreasing = TRUE)

  #print results
  if (VERBOSE) {
    print("normalized performace matrix")
    print(r)
    print("weighted normalized performance matrix")
    print(v)
    print("Positive ideal soluation A*")
    print(top_i$a_ideal)
    print("Anti-ideal solution A-")
    print(top_i$a_anti)
    print("Closenes to ideal variant")
    print(top_i$d_ideal)
    print("Closeness to anti-ideal variant")
    print(top_i$d_anti)
    print("Relative closeness of the alternatives to ideal solution")
    print(ordered_c)
  }

  out <- list(
    C = ordered_c,
    normPM = r,
    weightPM = v,
    A_ideal = top_i$a_ideal,
    A_anti = top_i$a_anti,
    D_ideal = top_i$d_ideal,
    D_anti = top_i$d_anti
  )
  return(out)
}
