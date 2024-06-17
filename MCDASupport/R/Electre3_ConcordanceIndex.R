# Method for computing Concordance Indexes refactored from ELECTRE III
# method as it is also utilized by ELECTRE 1S method
#
# parameters:
#   - PM - performance matrix
#   - P  - preference threshold
#   - Q  - indifference threshold
#   - w  - weights
Electre3_ConcordanceIndex <- function(PM, P, Q, w) {
  nalt <- nrow(PM)
  ncri <- ncol(PM)
  alt  <- rownames(PM)
  cm   <- matrix(data = 0, nrow = nalt, ncol = nalt)  #concordance matrix
  cjw  <- list()   #temp variable holds cj (concordance matrix for the criteria)
  d <- matrix(data = 0, nrow = nalt, ncol = nalt)  #template for empty matrix
  colnames(d) <- alt
  rownames(d) <- alt
  for (k in 1:ncri) { #index for criteria
    c  <- d  #temp var for cj
    diff_pq <- P[k] - Q[k]
    for (i in 1:nalt) {    #index for a
      diff_piqk <- P[i] - Q[k]
      for (j in 1:nalt) {  #index for b
        #concordance matrix
        #represents percentage of weights of criteria that concord with the
        # proposition 'a outranks b'
        # eq C(a,b) = 1/W * SUM(wj*cj(a,b))
        # eq W = SUM(wj)
        # cj(a,b) = 1 if PMj(a) + Qj(a) >= PMj(b)
        # cj(a,b) = 0 if PMj(a) + Pj(a) < PMj(b)
        # else c(a,b) = (PMj(a) - PMj(b) + Pj(a)) / (Pj(a) - Qj(a))
        if (PM[i, k] + Q[k] >= PM[j, k]) {
          c[i, j] <- 1
        } else if (PM[i, k] + P[k] < PM[j, k]) {
          c[i, j] <- 0
        } else {
          c[i, j] <- ifelse((diff_piqk != 0),
                            (PM[i, k] - PM[j, k] + P[k]) / diff_pq, 1)
        }
      }
    }
    cjw[[k]] <- c * w[k]
  }
  cm <- Reduce("+", cjw) / sum(w)
  colnames(cm) <- alt
  rownames(cm) <- alt

  out <- list(
    ConcordanceMatrix = cm
  )
  return(out)
}
