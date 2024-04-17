# Electre 1S implementation
# parameters:
#   PM             - performance matrix
#   w              - weights of the criteria
#   Q              - indifference threshold
#   P              - preference thresholds
#   V              - veto thresholds
#   minmaxcriteria - vector with identification of direction of the criteria
#   lambda         - cut-off criterion
#   VERBOSE        - output results to the console?
Electre_1S <- function(PM, w, Q, P, V, minmaxcriteria = 'max', lambda = 0.5, VERBOSE = FALSE){

  # check consistency of parameters
  Electre_1S_paramCheck(PM = PM, w = w, P = P, Q = Q, V = V,
                        minmaxcriteria = minmaxcriteria, lambda = lambda)

  PM   <- util_pm_minmax(PM, minmaxcriteria) #validate minmax and invert scales if necessary
  ncri <- ncol(PM)  #no. of criteria
  nalt <- nrow(PM)  #no. of alternatives
  alt  <- rownames(PM)

  #overall concordance index oci
  t <- Electre3_ConcordanceIndex(PM, P, Q, w)
  oci <- t$ConcordanceMatrix

  # discordance index of the criteria
  # dj(ai,ak) = 0, if gj(ak) - gj(ai) < Vj (ai,ak) - qj (ai,ak) * 
  # ((1-c(a,b))/(1-c))
  # else dj(ai,ak) = 1
  w_sum <- 1
  di <- matrix(0, nalt, nalt)
  rownames(di) <- alt
  colnames(di) <- alt
  diag(di) <- 1
  for (i in 1:nalt) {
    dj_ab <- matrix(0, nalt, nalt)
    rownames(dj_ab) <- alt
    colnames(dj_ab) <- alt
    for (j in 1:ncri) {
      for (k in 1:ncri) {
        if (j != k) {
          if (w[k] / sum(w) != 0) w_sum <- (w[k] / sum(w))
          if (PM[j, k] - PM[i, k] >= (V[k] - Q[k] * ((1 - oci[i, j] - (w_sum)) / (1 - lambda - (w_sum))))) {
            di[i, j] <- 1
          }
        }
      }
    }
  }

  # credibility matrix
  cred <- matrix(0, nalt, nalt)
  rownames(cred) <- alt
  colnames(cred) <- alt
  for (i in 1:nalt) {
    for (j in 1:nalt) {
      if (i != j && oci[i, j] >= lambda && di[i, j] == 0) cred[i, j] <- 1
    }
  }

  G  <- simplify(graph_from_adjacency_matrix(cred)) #remove loops
  AM <- Graph2AdjancancyMatrix(G, alt)

  # establish kernel of the decision
  t <- ELECTRE1_Kernel(AM)

  # output to screen
  if (VERBOSE) {
    print("Concordance matrix")
    print(oci)
    print("discordance matrix")
    print(di)
    print("Credibility matrix")
    print(AM)
    print("kernel")
    print(t$kernel)
    print("Dominated alternatives")
    print(t$dominated)
    print("Warning - experimental implementation,
    do not use in production environment")
  }

  out <- list(
    ConcordanceMatrix = oci,
    DiscordanceIndex = di,
    CredibilityIndex = AM,
    Kernel = t$kernel,
    Dominated = t$dominated,
    graph = t$graph
  )
  return(out)
}
