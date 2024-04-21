# Test sensitivity for ELECTRE IV method parameters
#
# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   P  - preference threshold
#   Q  - indifference threshold
#   V  - veto threshold
#
# solution for the problem is dominance matrix which can be visualized
# into network graph
Electre_4_sensitivity <- function(PM, P, Q, V, minmaxcriteria = "max") {

  # check validity of the objects manipulated by the current function
  Electre_4_paramCheck(PM = PM, P = P, Q = Q, V = V,
                       minmaxcriteria = minmaxcriteria)
  ncri <- length(P)
  nalt <- nrow(PM)
  cri <- colnames(PM)
  alt <- rownames(PM)

  #prepare hyperparameters
  hyp_P_cri <- list()
  hyp_Q_cri <- list()
  hyp_V_cri <- list()
  hyp_P_cri_graph <- list()
  hyp_Q_cri_graph <- list()
  hyp_V_cri_graph <- list()
  hyp_P <- hyp_Q <- hyp_V <- matrix(0, nrow = 21, ncol = ncri)
  colnames(hyp_P) <- colnames(hyp_Q) <- colnames(hyp_V) <- cri
  nhyp <- 21 # value of thresholds +/- 10 % should always be 21 elements
  for (i in 1:ncri) { # for all criteria
    t_p <- tenPercent(P[i])
    t_q <- tenPercent(Q[i])
    t_v <- tenPercent(V[i])
    hyp_P[, i] <- t_p
    hyp_Q[, i] <- t_q
    hyp_V[, i] <- t_v

    # sensitivity of the preference thresholds
    hyp_P_result <- hyp_Q_result <- hyp_V_result <- matrix(0, ncol = nalt, nrow = 21)
    colnames(hyp_P_result) <- colnames(hyp_Q_result) <- colnames(hyp_V_result) <- alt
    for (j in 1:nhyp) { #iterate over values t_* for the criterium
      thres <- P
      thres[i] <- t_p[j]
      tp <- Electre_4(PM, thres, Q, V, minmaxcriteria, FALSE)
      thres <- Q
      thres[i] <- t_q[j]
      tq <- Electre_4(PM, P, thres, V, minmaxcriteria, FALSE)
      thres <- V
      thres[i] <- t_v[j]
      tv <- Electre_4(PM, P, Q, thres, minmaxcriteria, FALSE)
      hyp_P_result[j, ] <- tp$finalRankingUnsorted
      hyp_Q_result[j, ] <- tq$finalRankingUnsorted
      hyp_V_result[j, ] <- tv$finalRankingUnsorted
    }

    hyp_P_cri[[i]] <- hyp_P_result
    hyp_Q_cri[[i]] <- hyp_Q_result
    hyp_V_cri[[i]] <- hyp_V_result

    hyp_P_result <- as.data.frame(hyp_P_result)
    hyp_Q_result <- as.data.frame(hyp_Q_result)
    hyp_V_result <- as.data.frame(hyp_V_result)

    title <- paste0("preference threshold for ", cri[i])
    hyp_P_cri_graph[[i]] <- rankGraph(hyp_P_result, t_p, title)
    title <- paste0("indifference threshold for ", cri[i])
    hyp_Q_cri_graph[[i]] <- rankGraph(hyp_Q_result, t_q, title)
    title <- paste0("veto threshold for ", cri[i])
    hyp_Q_cri_graph[[i]] <- rankGraph(hyp_Q_result, t_p, title)
  }

  # prepare outputs of the function
  out <- list(
    hyp_P = hyp_P,
    hyp_Q = hyp_Q,
    hyp_V = hyp_V,
    hyp_P_criteria = hyp_P_cri,
    hyp_Q_criteria = hyp_Q_cri,
    hyp_V_criteria = hyp_V_cri,
    hyp_P_criteriaGraph = hyp_P_cri_graph,
    hyp_Q_criteriaGraph = hyp_Q_cri_graph,
    hyp_V_criteriaGraph = hyp_V_cri_graph
  )
  return(out)
}
