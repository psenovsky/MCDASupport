# Superiority and inferiority ranking (SIR) method
#
# parameters
#   PM - performance matrix (alternatives in rows, criteria in columns)
#   w - weights
#   d - preference function vector
#   minmax - min/max value or vector of mixed values for criteria orientation
#   indifferenceTreshold list of indifference thresholds
#   prefferenceThreshold list of prefference thresholds
#   intermediateThreshold list of intermediate thresholds for Gaussian
#                         function type
#   SAW  - if set to TRUE, SIR-SAW is used as aggregation procedure, otherwise
#          SIR-TOPSIS is used
#   VERBOSE - if set to T dump output to console
SIR <- function(PM, w, d, minmax = "max", indifferenceTreshold = NULL, prefferenceThreshold = NULL,
                intermediateThreshold = NULL, SAW = TRUE, VERBOSE = FALSE) {
  ## check validity of the objects manipulated by the current function
  # with < 2 criteria or 2 alternatives, there is no MCDA problem
  PM <- util_pm_minmax(PM, minmax) #validate minmax and invert scales if neccessary
  preferenceFunction <- d
  t <- promethee_param_check(PM, preferenceFunction, w, indifferenceTreshold,
                             prefferenceThreshold, intermediateThreshold)
  ## End of checking the validity of the "inputs"

  nalt <- nrow(PM)  #no. of alternatives
  ncri <- ncol(PM)
  alt  <- rownames(PM)
  cri  <- colnames(PM)
  qj   <- indifferenceTreshold
  pj   <- prefferenceThreshold
  sj   <- t

  #pairwaise comparison
  DK <- lapply(1:ncri, function(k) {
    DKf <- outer(1:nalt, 1:nalt, Vectorize(function(i, j) PM[i, k] - PM[j, k]))
    rownames(DKf) <- alt
    colnames(DKf) <- alt
    DKf # return value for the function
  })
  names(DK) <- cri

  #preference degree (validated in PROMETHEE)
  Pj <- preferenceDegree(nalt, ncri, DK, d, qj, pj, sj, alt, cri)

  #superiority (Si) and inferiority (Ii) index
  si <- sapply(1:ncri, function(i) rowSums(Pj[[i]]))
  ii <- sapply(1:ncri, function(i) colSums(Pj[[i]]))
  rownames(si) <- alt
  rownames(ii) <- alt
  colnames(si) <- cri
  colnames(ii) <- cri

  #S-flow a I-flow
  if (SAW) {
    #SIR SAW agregation method
    s_flow <- rowSums(sweep(si, MARGIN = 2, w, `*`))
    i_flow <- rowSums(sweep(ii, MARGIN = 2, w, `*`))
  } else {
    #SIR TOPSIS agregation method
    # as oposed to TOPSIS the procedure works separately with Si and Ii
    # matrixes to derive S and I flow
    # Determination of the Ideal (A_ideal) and Anti-ideal (A_anti) Solutions,
    # as max or minimums in the criteria
    si <- as.data.frame(si)
    ii <- as.data.frame(ii)
    t <- topsis_ideal(si)
    s_flow <- t$closenes

    # Determination of the Ideal (A_ideal) and Anti-ideal (A_anti) Solutions,
    # as max or minimums in the criteria
    # note that for Ii max and min are switched - min for ideal and max for
    # anti ideal solution
    t <- topsis_ideal(ii, inferiority = TRUE)
    i_flow <- t$closenes
  }

  # first and second complete ranking (according to S and I flows)
  net_flow <- s_flow - i_flow
  relative_flow <- s_flow / (s_flow + i_flow)
  flows <- cbind(s_flow, i_flow, net_flow, relative_flow)
  rownames(flows) <- alt
  colnames(flows) <- c("S-flow", "I-flow", "n-flow", "r-flow")
  names(s_flow) <- alt
  names(i_flow) <- alt
  names(net_flow) <- alt
  names(relative_flow) <- alt
  s_flow <- sort(s_flow, decreasing = TRUE)
  i_flow <- sort(i_flow, decreasing = FALSE)
  net_flow <- sort(net_flow, decreasing = TRUE)
  relative_flow <- sort(relative_flow, decreasing = TRUE)
  rank_s_flow <- cumsum(c(1, diff(s_flow) != 0))
  rank_i_flow <- cumsum(c(1, diff(i_flow) != 0))
  names(rank_s_flow) <- names(s_flow)
  names(rank_i_flow) <- names(i_flow)

  # partial ranking
  partial_ranking <- outer(1:nalt, 1:nalt, Vectorize(function(i, j) {
    if (i == j) {
      return(0) #no meaning evaluation alternative with itself
    } else if ((rank_s_flow[alt[i]] < rank_s_flow[alt[j]] && rank_i_flow[alt[i]] < rank_i_flow[alt[j]]) ||
               (rank_s_flow[alt[i]] < rank_s_flow[alt[j]] && rank_i_flow[alt[i]] == rank_i_flow[alt[j]]) ||
               (rank_s_flow[alt[i]] == rank_s_flow[alt[j]] && rank_i_flow[alt[i]] < rank_i_flow[alt[j]])) {
      return("P") #preference
    } else if (rank_s_flow[alt[i]] == rank_s_flow[alt[j]] && rank_i_flow[alt[i]] == rank_i_flow[alt[j]]) {
      return("I") #indifference
    }else if (rank_s_flow[alt[i]] < rank_s_flow[alt[j]] && rank_i_flow[alt[i]] > rank_i_flow[alt[j]]) {
      return("R") #incomparable
    }
  }))
  rownames(partial_ranking) <- alt
  colnames(partial_ranking) <- alt

  if (VERBOSE) {
    print("Flows in the solution")
    print(flows)
  }

  out <- list(
    superiorityFlow = s_flow,
    inferiorityFlow = i_flow,
    rankSFlow = rank_s_flow,
    rankIFlow = rank_i_flow,
    partialRanking = partial_ranking,
    netFlow = net_flow,
    relativeFlow = relative_flow,
    flows = flows
  )
  return(out)
}
