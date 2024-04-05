# function to identify dominated alternatives from the solution
# parameters:
#   M - performance matrix
#   minmaxcriteria - the direction of the criteria
mcda_get_dominated <- function(M, minmaxcriteria = 'max') {
  #check validity of inputs
  ncrit <- ncol(M)
  if(nrow(M) < 2 || ncrit < 2) stop('Performance matrix must have at minimum 2 alteratives and criteria')
  if (!(is.matrix(M) || (is.data.frame(M)))) stop("wrong performance matrix, should be a matrix or a data frame")

  PM <- util_pm_minmax(M, minmaxcriteria)
  nalt  <- nrow(PM)

  Dom <- matrix(data=0, nrow=nalt, nalt)
  colnames(Dom) <- rownames(Dom) <- rownames(M)
  dominatedOnes <- rep(0, nalt)
  names(dominatedOnes) <- rownames(M)

  for(i in 1:nalt) {
    for(j in 1:nalt) {
      if(i == j) next
      if(all(PM[i,] >= PM[j,])) {
        Dom[j,i] <- 1
        dominatedOnes[j] <- 1
      }
    }
  }
  dominatedAlternatives <- names(dominatedOnes[dominatedOnes == 1])

  out <- list(
    dominationMatrix = t(Dom),
    dominatedAlternatives = dominatedAlternatives
  )
  return(out)
}
