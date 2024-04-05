# Sensitivity analysis for ELECTRE TRI method. Sensivity of results is checked to changes in the thresholds
#
# parameters
#   PM             - performance matrix
#   profiles       - Matrix containing, in each row, the lower profiles of the categories.
# The columns are named according to the criteria, and the rows are named according to the categories.
# The index of the row in the matrix corresponds to the rank of the category.
#   profile_names  - Vector containing profiles' names
#   w              - weights of the criteria
#   Q              - indifference threshold
#   P              - preference thresholds
#   V              - veto thresholds
#   minmaxcriteria - vector with identification of direction of the criteria
#   lambda         - cut-off criterion
# note pyDecisions sets lambda = 1
Electre_TRI_sensitivity <- function(PM, profiles, profiles_names,
                                    w, Q, P, V, minmaxcriteria = 'max', lambda=0.75){


  # check validity of parametrs specific for sensitivity analysis
  Electre_4_paramCheck(PM = PM, Q = Q, P = P, V = V, minmaxcriteria = minmaxcriteria)
  if (!is.vector(w) || !is.numeric(w)) stop('criteria weights should be a numeric vector')
  if (!is.numeric(lambda) || lambda < 0.5 || lambda > 1) stop('for lambda number expected in <0.5;1>')

  ncri <- length(P)
  nalt <- nrow(PM)
  cri <- colnames(PM)
  alt <- rownames(PM)

  #prepare hyperparameters
  hyp_P_cri_opt <- list()
  hyp_Q_cri_opt <- list()
  hyp_V_cri_opt <- list()
  hyp_P_cri_pes <- list()
  hyp_Q_cri_pes <- list()
  hyp_V_cri_pes <- list()
  hyp_P_cri_graph_opt <- list()
  hyp_Q_cri_graph_opt <- list()
  hyp_V_cri_graph_opt <- list()
  hyp_P_cri_graph_pes <- list()
  hyp_Q_cri_graph_pes <- list()
  hyp_V_cri_graph_pes <- list()
  hyp_P <- hyp_Q <- hyp_V <- matrix(0, nrow = 21, ncol = ncri)
  colnames(hyp_P) <- colnames(hyp_Q) <- colnames(hyp_V) <- cri
  nhyp <- 21 # value of thresholds +/- 10 % should always be 21 elements
  for(i in 1:ncri){ # for all criteria
    t_p <- tenPercent(P[i])
    t_q <- tenPercent(Q[i])
    t_v <- tenPercent(V[i])
    hyp_P[,i] <- t_p
    hyp_Q[,i] <- t_q
    hyp_V[,i] <- t_v

    # sensitivity of the preference thresholds
    hyp_P_result_opt <- hyp_Q_result_opt <- hyp_V_result_opt <- matrix(0, ncol = nalt, nrow = 21)
    hyp_P_result_pes <- hyp_Q_result_pes <- hyp_V_result_pes <- matrix(0, ncol = nalt, nrow = 21)
    colnames(hyp_P_result_opt) <- colnames(hyp_Q_result_opt) <- colnames(hyp_V_result_opt) <- alt
    colnames(hyp_P_result_pes) <- colnames(hyp_Q_result_pes) <- colnames(hyp_V_result_pes) <- alt
    for(j in 1:nhyp){ #iterate over values t_* for the criterium
      thres <- P
      thres[i] <- t_p[j]
      tp <- Electre_TRI(PM = PM, profiles = profiles, profiles_names = profiles_names, w = w, Q = Q, P = thres, V = V, minmaxcriteria = minmaxcriteria, lambda = lambda, VERBOSE = FALSE)
      thres <- Q
      thres[i] <- t_q[j]
      tq <- Electre_TRI(PM = PM, profiles = profiles, profiles_names = profiles_names, w = w, Q = thres, P = P, V = V, minmaxcriteria = minmaxcriteria, lambda = lambda, VERBOSE = FALSE)
      thres <- V
      thres[i] <- t_v[j]
      tv <- Electre_TRI(PM = PM, profiles = profiles, profiles_names = profiles_names, w = w, Q = Q, P = P, V = thres, minmaxcriteria = minmaxcriteria, lambda = lambda, VERBOSE = FALSE)
      hyp_P_result_opt[j,] <- tp$optimisticUnsorted
      hyp_Q_result_opt[j,] <- tq$optimisticUnsorted
      hyp_V_result_opt[j,] <- tv$optimisticUnsorted
      hyp_P_result_pes[j,] <- tp$pesimisticUnsorted
      hyp_Q_result_pes[j,] <- tq$pesimisticUnsorted
      hyp_V_result_pes[j,] <- tv$pesimisticUnsorted
    }

    hyp_P_cri_opt[[i]] <- hyp_P_result_opt
    hyp_Q_cri_opt[[i]] <- hyp_Q_result_opt
    hyp_V_cri_opt[[i]] <- hyp_V_result_opt
    hyp_P_cri_pes[[i]] <- hyp_P_result_pes
    hyp_Q_cri_pes[[i]] <- hyp_Q_result_pes
    hyp_V_cri_pes[[i]] <- hyp_V_result_pes

    hyp_P_result_opt <- as.data.frame(hyp_P_result_opt)
    hyp_Q_result_opt <- as.data.frame(hyp_Q_result_opt)
    hyp_V_result_opt <- as.data.frame(hyp_V_result_opt)
    hyp_P_result_pes <- as.data.frame(hyp_P_result_pes)
    hyp_Q_result_pes <- as.data.frame(hyp_Q_result_pes)
    hyp_V_result_pes <- as.data.frame(hyp_V_result_pes)

    title <- paste0('preference threshold, optimistic proc. for ', cri[i])
    hyp_P_cri_graph_opt[[i]] <- rankGraph(hyp_P_result_opt, t_p, title)
    title <- paste0('indifference threshold, optimistic proc. for ', cri[i])
    hyp_Q_cri_graph_opt[[i]] <- rankGraph(hyp_Q_result_opt, t_q, title)
    title <- paste0('veto threshold, optimistic proc. for ', cri[i])
    hyp_Q_cri_graph_opt[[i]] <- rankGraph(hyp_Q_result_opt, t_p, title)
    title <- paste0('preference threshold, pesimistic proc. for ', cri[i])
    hyp_P_cri_graph_pes[[i]] <- rankGraph(hyp_P_result_pes, t_p, title)
    title <- paste0('indifference threshold, pesimistic proc. for ', cri[i])
    hyp_Q_cri_graph_pes[[i]] <- rankGraph(hyp_Q_result_pes, t_q, title)
    title <- paste0('veto threshold, pesimistic proc. for ', cri[i])
    hyp_Q_cri_graph_pes[[i]] <- rankGraph(hyp_Q_result_pes, t_p, title)
  }

  # prepare outputs of the function
  out <- list(
    hyp_P = hyp_P,
    hyp_Q = hyp_Q,
    hyp_V = hyp_V,
    hyp_P_criteria_optimistic = hyp_P_cri_opt,
    hyp_Q_criteria_optimistic = hyp_Q_cri_opt,
    hyp_V_criteria_optimistic = hyp_V_cri_opt,
    hyp_P_criteria_pesimistic = hyp_P_cri_pes,
    hyp_Q_criteria_pesimistic = hyp_Q_cri_pes,
    hyp_V_criteria_pesimistic = hyp_V_cri_pes,
    hyp_P_criteriaGraph_optimistic = hyp_P_cri_graph_opt,
    hyp_Q_criteriaGraph_optimistic = hyp_Q_cri_graph_opt,
    hyp_V_criteriaGraph_optimistic = hyp_V_cri_graph_opt,
    hyp_P_criteriaGraph_pesimistic = hyp_P_cri_graph_pes,
    hyp_Q_criteriaGraph_pesimistic = hyp_Q_cri_graph_pes,
    hyp_V_criteriaGraph_pesimistic = hyp_V_cri_graph_pes
  )
  return(out)
}
