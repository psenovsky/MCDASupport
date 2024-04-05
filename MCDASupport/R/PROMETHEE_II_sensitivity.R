# Sensitivity analysis for PROMETHEE I method. Sensivity of results is checked to changes in the thresholds
#
# parameters
#   PM - performance matrix
#   preferenceFunction - vector of preference functions types to derive preference when comparing PM
#   w - weights
#   minmax - min/max value or vector of mixed values for criteria orientation
#   indifferenceTreshold list of indifference thresholds
#   prefferenceThreshold list of prefference thresholds
#   intermediateThreshold lish of intermediate thresholds for Gaussian function type
PROMETHEE_II_sensitivity <- function(PM, preferenceFunction, w, minmax = 'max',
                        indifferenceTreshold = NULL, prefferenceThreshold = NULL,
                        intermediateThreshold = NULL){

  # check validity of parametrs specific for sensitivity analysis
  nalt <- nrow(PM)
  cri <- colnames(PM)
  ncri <- length(cri)
  alt <- rownames(PM)

  #prepare hyperparameters
  hyp_P_cri_net <- list() # net flow
  hyp_Q_cri_net <- list()
  hyp_I_cri_net <- list()
  hyp_P_cri_graph_net <- list()
  hyp_Q_cri_graph_net <- list()
  hyp_I_cri_graph_net <- list()
  hyp_P <- hyp_Q <- hyp_I <- matrix(0, nrow = 21, ncol = ncri)
  colnames(hyp_P) <- colnames(hyp_Q) <- colnames(hyp_I) <- cri
  nhyp <- 21 # value of thresholds +/- 10 % should always be 21 elements
  for(i in 1:ncri){ # for all criteria
    t_p <- tenPercent(prefferenceThreshold[i])
    t_q <- tenPercent(indifferenceTreshold[i])
    t_i <- tenPercent(intermediateThreshold[i])
    hyp_P[,i] <- t_p
    hyp_Q[,i] <- t_q
    hyp_I[,i] <- t_i

    # sensitivity of the preference thresholds
    hyp_P_result_net <- hyp_Q_result_net <- hyp_I_result_net <- matrix(0, ncol = nalt, nrow = 21)
    colnames(hyp_P_result_net) <- colnames(hyp_Q_result_net) <- colnames(hyp_I_result_net) <- alt
    for(j in 1:nhyp){ #iterate over values t_* for the criterium
      thres <- prefferenceThreshold
      thres[i] <- t_p[j]
      tp <- PROMETHEE_II(PM = PM, preferenceFunction = preferenceFunction, w = w, minmax = minmax, indifferenceTreshold = indifferenceTreshold, prefferenceThreshold = thres,
                        intermediateThreshold = intermediateThreshold, VERBOSE = FALSE)
      thres <- indifferenceTreshold
      thres[i] <- t_q[j]
      tq <- PROMETHEE_II(PM = PM, preferenceFunction = preferenceFunction, w = w, minmax = minmax, indifferenceTreshold = thres, prefferenceThreshold = prefferenceThreshold,
                        intermediateThreshold = intermediateThreshold, VERBOSE = FALSE)
      thres <- intermediateThreshold
      thres[i] <- t_i[j]
      ti <- PROMETHEE_II(PM = PM, preferenceFunction = preferenceFunction, w = w, minmax = minmax, indifferenceTreshold = indifferenceTreshold, prefferenceThreshold = prefferenceThreshold,
                        intermediateThreshold = thres, VERBOSE = FALSE)
      hyp_P_result_net[j,] <- tp$netFlow
      hyp_Q_result_net[j,] <- tq$netFlow
      hyp_I_result_net[j,] <- ti$netFlow
    }

    hyp_P_cri_net[[i]] <- hyp_P_result_net
    hyp_Q_cri_net[[i]] <- hyp_Q_result_net
    hyp_I_cri_net[[i]] <- hyp_I_result_net

    hyp_P_result_net <- as.data.frame(hyp_P_result_net)
    hyp_Q_result_net <- as.data.frame(hyp_Q_result_net)
    hyp_I_result_net <- as.data.frame(hyp_I_result_net)

    title <- paste0('preference threshold, net flow for ', cri[i])
    hyp_P_cri_graph_net[[i]] <- rankGraph(hyp_P_result_net, t_p, title)
    title <- paste0('indifference threshold, net flow for ', cri[i])
    hyp_Q_cri_graph_net[[i]] <- rankGraph(hyp_Q_result_net, t_q, title)
    title <- paste0('intemediate threshold, net flow for ', cri[i])
    hyp_I_cri_graph_net[[i]] <- rankGraph(hyp_I_result_net, t_i, title)
  }

  # prepare outputs of the function
  out <- list(
    hyp_P = hyp_P,
    hyp_Q = hyp_Q,
    hyp_I = hyp_I,
    hyp_P_criteria_netFlow = hyp_P_cri_net,
    hyp_Q_criteria_netFlow = hyp_Q_cri_net,
    hyp_I_criteria_netFlow = hyp_I_cri_net,
    hyp_P_criteriaGraph_netFlow = hyp_P_cri_graph_net,
    hyp_Q_criteriaGraph_netFlow = hyp_Q_cri_graph_net,
    hyp_I_criteriaGraph_netFlow = hyp_I_cri_graph_net
  )
  return(out)
}
