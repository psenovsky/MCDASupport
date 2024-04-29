# function to identify dominated alternatives from the solution
# parameters:
#   M - performance matrix
#   minmaxcriteria - the direction of the criteria
mcda_get_dominated <- function(m, minmaxcriteria = "max") {
  #check validity of inputs
  ncrit <- ncol(m)
  if (nrow(m) < 2 || ncrit < 2) {
    stop("Performance matrix must have at minimum 2 alteratives and criteria")
  }
  if (!(is.matrix(m) || (is.data.frame(m)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }

  pm <- util_pm_minmax(m, minmaxcriteria)
  nalt  <- nrow(pm)

  dom <- matrix(data = 0, nrow = nalt, nalt)
  colnames(dom) <- rownames(dom) <- rownames(m)
  dominated_ones <- rep(0, nalt)
  names(dominated_ones) <- rownames(m)

  for (i in 1:nalt) {
    for (j in 1:nalt) {
      if (i == j) next
      if (all(pm[i, ] >= pm[j, ])) {
        dom[j, i] <- 1
        dominated_ones[j] <- 1
      }
    }
  }
  dominated_alternatives <- names(dominated_ones[dominated_ones == 1])

  out <- list(
    dominationMatrix = t(dom),
    dominatedAlternatives = dominated_alternatives
  )
  return(out)
}
