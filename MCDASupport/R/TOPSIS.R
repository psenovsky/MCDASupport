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
  if (!(is.vector(w, mode = "numerix"))) {
    stop("criteria weights should be a vector")
  }
  if (ncol(PM) != length(w)) {
    stop("length of criteria weights should be checked")
  }
  ## End of checking the validity of the "inputs"

  nalt <- nrow(PM)  #no. of alternatives
  ncri <- ncol(PM)  #no. of criteria
  alt  <- rownames(PM)
  cri  <- colnames(PM)

  #Step 1. Normalization of the Decision Matrix (r) (validated)
  r <- sapply(1:ncri, function(j) PM[, j] / sqrt(sum(PM[, j] * PM[, j])))
  colnames(r) <- cri
  rownames(r) <- alt

  # Step 2. Calculation of the Weighted Normalized Decision Matrix (v)
  # (validated)
  v <- sweep(r, MARGIN = 2, w, `*`)
  colnames(v) <- cri
  rownames(v) <- alt
  v <- as.data.frame(v)

  # Step 3. Determination of the Ideal (A_ideal) and Anti-ideal (A_anti)
  # solutions (validated), as max or minimums in the criteria)
  A_ideal <- v %>%
    group_by() %>%
    summarise(across(1:ncri, max)) %>%
    unlist()
  A_anti <- v %>%
    group_by() %>%
    summarise(across(1:ncri, min)) %>%
    unlist()
  names(A_ideal) <- cri
  names(A_anti) <- cri

  # Step 4. Calculation of the Separation Measures (distance from ideal/anti
  # ideal solution) (validated)
  D_ideal <- sqrt(sapply(1:nalt, function(i) sum((v[i, ] - A_ideal)^2)))
  D_anti <- sqrt(sapply(1:nalt, function(i) sum((v[i, ] - A_anti)^2)))
  names(D_ideal) <- alt
  names(D_anti) <- alt

  # Step 5. Calculation of the Relative Closeness to the Ideal Solution
  # (validated)
  C <- D_anti / (D_anti + D_ideal) #always 0-1, the closer to 1, the better
  names(C) <- alt

  #Step 6. order by C
  orderedC <- sort(C, decreasing = TRUE)

  #print results
  if (VERBOSE) {
    print("normalized performace matrix")
    print(r)
    print("weighted normalized performance matrix")
    print(v)
    print("Positive ideal soluation A*")
    print(A_ideal)
    print("Anti-ideal solution A-")
    print(A_anti)
    print("Closenes to ideal variant")
    print(D_ideal)
    print("Closeness to anti-ideal variant")
    print(D_anti)
    print("Relative closeness of the alternatives to ideal solution")
    print(orderedC)
  }

  out <- list(
    C = orderedC,
    normPM = r,
    weightPM = v,
    A_ideal = A_ideal,
    A_anti = A_anti,
    D_ideal = D_ideal,
    D_anti = D_anti
  )
  return(out)
}
