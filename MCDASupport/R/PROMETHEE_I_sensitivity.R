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
PROMETHEE_I_sensitivity <- function(PM, preferenceFunction, w, minmax = 'max',
                        indifferenceTreshold = NULL, prefferenceThreshold = NULL,
                        intermediateThreshold = NULL){


  # check validity of parametrs specific for sensitivity analysis
  nalt <- nrow(PM)
  cri <- colnames(PM)
  ncri <- length(cri)
  alt <- rownames(PM)

  #prepare hyperparameters
  hyp_P_cri_pos <- list() # positive flow
  hyp_Q_cri_pos <- list()
  hyp_I_cri_pos <- list()
  hyp_P_cri_neg <- list() # negative flow
  hyp_Q_cri_neg <- list()
  hyp_I_cri_neg <- list()
  hyp_P_cri_graph_pos <- list()
  hyp_Q_cri_graph_pos <- list()
  hyp_I_cri_graph_pos <- list()
  hyp_P_cri_graph_neg <- list()
  hyp_Q_cri_graph_neg <- list()
  hyp_I_cri_graph_neg <- list()
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
    hyp_P_result_pos <- hyp_Q_result_pos <- hyp_I_result_pos <- matrix(0, ncol = nalt, nrow = 21)
    hyp_P_result_neg <- hyp_Q_result_neg <- hyp_I_result_neg <- matrix(0, ncol = nalt, nrow = 21)
    colnames(hyp_P_result_pos) <- colnames(hyp_Q_result_pos) <- colnames(hyp_I_result_pos) <- alt
    colnames(hyp_P_result_neg) <- colnames(hyp_Q_result_neg) <- colnames(hyp_I_result_neg) <- alt
    for(j in 1:nhyp){ #iterate over values t_* for the criterium
      thres <- prefferenceThreshold
      thres[i] <- t_p[j]
      tp <- PROMETHEE_I(PM = PM, preferenceFunction = preferenceFunction, w = w, minmax = minmax, indifferenceTreshold = indifferenceTreshold, prefferenceThreshold = thres,
                        intermediateThreshold = intermediateThreshold, VERBOSE = FALSE)
      thres <- indifferenceTreshold
      thres[i] <- t_q[j]
      tq <- PROMETHEE_I(PM = PM, preferenceFunction = preferenceFunction, w = w, minmax = minmax, indifferenceTreshold = thres, prefferenceThreshold = prefferenceThreshold,
                        intermediateThreshold = intermediateThreshold, VERBOSE = FALSE)
      thres <- intermediateThreshold
      thres[i] <- t_i[j]
      ti <- PROMETHEE_I(PM = PM, preferenceFunction = preferenceFunction, w = w, minmax = minmax, indifferenceTreshold = indifferenceTreshold, prefferenceThreshold = prefferenceThreshold,
                        intermediateThreshold = thres, VERBOSE = FALSE)
      hyp_P_result_pos[j,] <- tp$positiveFlow
      hyp_Q_result_pos[j,] <- tq$positiveFlow
      hyp_I_result_pos[j,] <- ti$positiveFlow
      hyp_P_result_neg[j,] <- tp$negativeFlow
      hyp_Q_result_neg[j,] <- tq$negativeFlow
      hyp_I_result_neg[j,] <- ti$negativeFlow
    }

    hyp_P_cri_pos[[i]] <- hyp_P_result_pos
    hyp_Q_cri_pos[[i]] <- hyp_Q_result_pos
    hyp_I_cri_pos[[i]] <- hyp_I_result_pos
    hyp_P_cri_neg[[i]] <- hyp_P_result_neg
    hyp_Q_cri_neg[[i]] <- hyp_Q_result_neg
    hyp_I_cri_neg[[i]] <- hyp_I_result_neg

    hyp_P_result_pos <- as.data.frame(hyp_P_result_pos)
    hyp_Q_result_pos <- as.data.frame(hyp_Q_result_pos)
    hyp_I_result_pos <- as.data.frame(hyp_I_result_pos)
    hyp_P_result_neg <- as.data.frame(hyp_P_result_neg)
    hyp_Q_result_neg <- as.data.frame(hyp_Q_result_neg)
    hyp_I_result_neg <- as.data.frame(hyp_I_result_neg)

    title <- paste0('preference threshold, positive flow for ', cri[i])
    hyp_P_cri_graph_pos[[i]] <- rankGraph(hyp_P_result_pos, t_p, title)
    title <- paste0('indifference threshold, positive flow for ', cri[i])
    hyp_Q_cri_graph_pos[[i]] <- rankGraph(hyp_Q_result_pos, t_q, title)
    title <- paste0('intemediate threshold, positive flow for ', cri[i])
    hyp_I_cri_graph_pos[[i]] <- rankGraph(hyp_I_result_pos, t_i, title)
    title <- paste0('preference threshold, negative flow for ', cri[i])
    hyp_P_cri_graph_neg[[i]] <- rankGraph(hyp_P_result_neg, t_p, title)
    title <- paste0('indifference threshold, negative flow for ', cri[i])
    hyp_Q_cri_graph_neg[[i]] <- rankGraph(hyp_Q_result_neg, t_q, title)
    title <- paste0('intermediate threshold, negative flow for ', cri[i])
    hyp_I_cri_graph_neg[[i]] <- rankGraph(hyp_I_result_neg, t_i, title)
  }

  # prepare outputs of the function
  out <- list(
    hyp_P = hyp_P,
    hyp_Q = hyp_Q,
    hyp_I = hyp_I,
    hyp_P_criteria_positiveFlow = hyp_P_cri_pos,
    hyp_Q_criteria_positiveFlow = hyp_Q_cri_pos,
    hyp_I_criteria_positiveFlow = hyp_I_cri_pos,
    hyp_P_criteria_negativeFlow = hyp_P_cri_neg,
    hyp_Q_criteria_negativeFlow = hyp_Q_cri_neg,
    hyp_I_criteria_negativeFlow = hyp_I_cri_neg,
    hyp_P_criteriaGraph_positiveFlow = hyp_P_cri_graph_pos,
    hyp_Q_criteriaGraph_positiveFlow = hyp_Q_cri_graph_pos,
    hyp_I_criteriaGraph_positiveFlow = hyp_I_cri_graph_pos,
    hyp_P_criteriaGraph_negativeFlow = hyp_P_cri_graph_neg,
    hyp_Q_criteriaGraph_negativeFlow = hyp_Q_cri_graph_neg,
    hyp_I_criteriaGraph_negativeFlow = hyp_I_cri_graph_neg
  )
  return(out)
}
