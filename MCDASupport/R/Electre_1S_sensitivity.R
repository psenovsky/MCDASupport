# Electre 1S implementation
# parameters:
#   PM             - performance matrix
#   w              - weights of the criteria
#   Q              - indifference threshold
#   P              - preference thresholds
#   V              - veto thresholds
#   minmaxcriteria - vector with identification of direction of the criteria
#   lambda         - cut-off criterion
Electre_1S_sensitivity <- function(PM, w, Q, P, V, minmaxcriteria = 'max', lambda = 0.5){

  # check consistency of parameters
  Electre_1S_paramCheck(PM = PM, w = w, P = P, Q = Q, V = V,
                        minmaxcriteria = minmaxcriteria, lambda = lambda)
  ncri <- length(P)
  nalt <- nrow(PM)
  cri <- colnames(PM)
  alt <- rownames(PM)

  # internal function to prepare row of the dataframe
  df_row <- function(kernel, alt, sensitivity, criterium, val){
    t <- rep(0, times = nalt + 2)
    for (k in kernel) {
      if (k %in% alt) {
        pos <- which(alt == k)
        t[pos] <- 1
      }
    }
    t[nalt + 1] <- sensitivity
    t[nalt + 2] <- criterium
    t[nalt + 3] <- val
    return(t)
  }

  #prepare hyperparameters
  hyp_P <- hyp_Q <- hyp_V <- matrix(0, nrow = 21, ncol = ncri)
  colnames(hyp_P) <- colnames(hyp_Q) <- colnames(hyp_V) <- cri
  nhyp <- 21 # value of thresholds +/- 10 % should always be 21 elements
  df <- data.frame()
  for (i in 1:ncri) { # for all criteria
    t_p <- tenPercent(P[i])
    t_q <- tenPercent(Q[i])
    t_v <- tenPercent(V[i])

    # sensitivity of the preference thresholds
    for (j in 1:nhyp) { #iterate over values t_* for the criterium
      thres <- P
      thres[i] <- t_p[j]
      tp <- Electre_1S(PM = PM, w = w, P = thres, Q = Q, V = V, minmaxcriteria,
                       lambda = lambda, FALSE)
      df <- rbind(df, df_row(tp$Kernel, alt = alt, sensitivity = "P",
                             criterium = cri[i], val = thres[i]))
      thres <- Q
      thres[i] <- t_q[j]
      tq <- Electre_1S(PM = PM, w = w, P = P, Q = thres, V = V, minmaxcriteria,
                       lambda = lambda, FALSE)
      df <- rbind(df, df_row(tq$Kernel, alt = alt, sensitivity = "Q",
                             criterium = cri[i], val = thres[i]))
      thres <- V
      thres[i] <- t_v[j]
      tv <- Electre_1S(PM = PM, w = w, P = P, Q = Q, V = thres,
                       minmaxcriteria, lambda = lambda, FALSE)
      df <- rbind(df, df_row(tv$Kernel, alt = alt, sensitivity = "V",
                             criterium = cri[i], val = thres[i]))
    }
  }
  colnames(df) <- c(alt, "sensitivity", "criterium", "treshold value")
  return(df)
}
