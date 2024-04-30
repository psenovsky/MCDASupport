# Superiority and inferiority ranking (SIR) method
#
# parameters
#   PM - performance matrix (alternatives in rows, criteria in columns)
#   w - weights
#   d - preference function array
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
  t <- promethee_param_check(PM, preferenceFunction, w, indifferenceTreshold, prefferenceThreshold,
                      intermediateThreshold)
  if (!t) stop("Error checking parameters")
  ## End of checking the validity of the "inputs"

  nalt <- nrow(PM)  #no. of alternatives
  ncri <- ncol(PM)
  alt  <- rownames(PM)
  cri  <- colnames(PM)

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
  Si <- sapply(1:ncri, function(i) rowSums(Pj[[i]]))
  Ii <- sapply(1:ncri, function(i) colSums(Pj[[i]]))
  rownames(Si) <- alt
  rownames(Ii) <- alt
  colnames(Si) <- cri
  colnames(Ii) <- cri

  #S-flow a I-flow
  if (SAW) {
    #SIR SAW agregation method
    Sflow <- rowSums(sweep(Si, MARGIN = 2, w, `*`))
    Iflow <- rowSums(sweep(Ii, MARGIN = 2, w, `*`))
  } else {
    #SIR TOPSIS agregation method
    # as oposed to TOPSIS the procedure works separately with Si and Ii
    # matrixes to derive S and I flow
    # Determination of the Ideal (A_ideal) and Anti-ideal (A_anti) Solutions
    # (validated), as max or minimums in the criteria
    Si <- as.data.frame(Si)
    Ii <- as.data.frame(Ii)
    A_ideal <- Si %>%
      group_by() %>%
      summarise(across(1:ncri, max)) %>%
      unlist()
    A_anti <- Si %>%
      group_by() %>%
      summarise(across(1:ncri, min)) %>%
      unlist()
    names(A_ideal) <- cri
    names(A_anti) <- cri

    #Step 4. Calculation of the Separation Measures (distance from ideal/anti ideal solution) (validated TOPSIS)
    D_ideal <- sqrt(sapply(1:nalt, function(i) sum((Si[i, ] - A_ideal)^2)))
    D_anti <- sqrt(sapply(1:nalt, function(i) sum((Si[i, ] - A_anti)^2)))
    names(D_ideal) <- alt
    names(D_anti) <- alt
    
    # Step 5. Calculation of the Relative Closeness to the Ideal Solution 
    # (validated TOPSIS)
    Sflow <- D_anti / (D_anti + D_ideal) #always 0-1, the closer to 1, the better

    # Determination of the Ideal (A_ideal) and Anti-ideal (A_anti) Solutions
    # (validated TOPSIS), as max or minimums in the criteria
    # note that for Ii max and min are switched - min for ideal and max for
    # anti ideal solution
    A_ideal <- Ii %>%
      group_by() %>%
      summarise(across(1:ncri, min)) %>%
      unlist()
    A_anti <- Ii %>%
      group_by() %>%
      summarise(across(1:ncri, max)) %>%
      unlist()
    names(A_ideal) <- cri
    names(A_anti) <- cri

    # Step 4. Calculation of the Separation Measures (distance from ideal/anti
    # ideal solution) (validated TOPSIS)
    D_ideal <- sqrt(sapply(1:nalt, function(i) sum((Ii[i, ] - A_ideal)^2)))
    D_anti <- sqrt(sapply(1:nalt, function(i) sum((Ii[i, ] - A_anti)^2)))
    names(D_ideal) <- alt
    names(D_anti) <- alt

    # Step 5. Calculation of the Relative Closeness to the Ideal Solution
    # (validated TOPSIS)
    Iflow <- D_anti / (D_anti + D_ideal) #always 0-1, the closer to 1, the better
  }

  # first and second complete ranking (according to S and I flows)
  netFlow <- Sflow - Iflow
  relativeFlow <- Sflow / (Sflow + Iflow)
  flows <- cbind(Sflow, Iflow, netFlow, relativeFlow)
  rownames(flows) <- alt
  colnames(flows) <- c("S-flow", "I-flow", "n-flow", "r-flow")
  names(Sflow) <- alt
  names(Iflow) <- alt
  names(netFlow) <- alt
  names(relativeFlow) <- alt
  Sflow <- sort(Sflow, decreasing = TRUE)
  Iflow <- sort(Iflow, decreasing = FALSE)
  netFlow <- sort(netFlow, decreasing = TRUE)
  relativeFlow <- sort(relativeFlow, decreasing = TRUE)
  rankSFlow <- cumsum(c(1, diff(Sflow) != 0))
  rankIFlow <- cumsum(c(1, diff(Iflow) != 0))
  names(rankSFlow) <- names(Sflow)
  names(rankIFlow) <- names(Iflow)

  # partial ranking
  partialRanking <- outer(1:nalt, 1:nalt, Vectorize(function(i, j) {
    if (i == j) {
      return(0) #no meaning evaluation alternative with itself
    } else if ((rankSFlow[alt[i]] < rankSFlow[alt[j]] && rankIFlow[alt[i]] < rankIFlow[alt[j]]) ||
             (rankSFlow[alt[i]] < rankSFlow[alt[j]] && rankIFlow[alt[i]] == rankIFlow[alt[j]]) ||
             (rankSFlow[alt[i]] == rankSFlow[alt[j]] && rankIFlow[alt[i]] < rankIFlow[alt[j]])){
      return("P") #preference
    }else if(rankSFlow[alt[i]] == rankSFlow[alt[j]] && rankIFlow[alt[i]] == rankIFlow[alt[j]]){
      return("I") #indifference
    }else if(rankSFlow[alt[i]] < rankSFlow[alt[j]] && rankIFlow[alt[i]] > rankIFlow[alt[j]]){
      return("R") #incomparable
    }
  }))
  rownames(partialRanking) <- alt
  colnames(partialRanking) <- alt

  if (VERBOSE) {
    print("Flows in the solution")
    print(flows)
  }

  out <- list(
    superiorityFlow = Sflow,
    inferiorityFlow = Iflow,
    rankSFlow = rankSFlow,
    rankIFlow = rankIFlow,
    partialRanking = partialRanking,
    netFlow = netFlow,
    relativeFlow = relativeFlow,
    flows = flows
  )
  return(out)
}
