# Superiority and inferiority ranking (SIR) method
#
# parameters
#   PM - performance matrix (alternatives in rows, criteria in columns)
#   w - weights
#   d - preference function array
#   minmax - min/max value or vector of mixed values for criteria orientation
#   indifferenceTreshold list of indifference thresholds
#   prefferenceThreshold list of prefference thresholds
#   intermediateThreshold lish of intermediate thresholds for Gaussian function type
#   SAW  - if set to T, SIR-SAW is used as aggregation procedure, otherwise SIR-TOPSIS is used
#   VERBOSE - if set to T dump output to console
SIR <- function(PM, w, d, minmax = 'max', indifferenceTreshold = NULL, prefferenceThreshold = NULL,
                intermediateThreshold = NULL, SAW = TRUE, VERBOSE = FALSE){
  ## check validity of the objects manipulated by the current function
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop('less than 2 criteria or 2 alternatives')
  if (!(is.matrix(PM) || (is.data.frame(PM)))) stop('wrong performance matrix, should be a matrix or a data frame')
  if (!is.numeric(unlist(PM))) stop('Only numeric values in performance matrix expected')
  PM <- util_pm_minmax(PM, minmax) #validate minmax and invert scales if neccessary
  if (!is.data.frame(PM)) PM <- as.data.frame(PM)
  ncri <- ncol(PM)  #no. of criteria
  qj   <- indifferenceTreshold
  pj   <- prefferenceThreshold
  sj   <- intermediateThreshold
  if (!(is.vector(w))) stop('criteria weights should be a vector')
  if (!is.numeric(w)) stop('criteriaWeights should be a numeric vector')
  if (sum(w) != 1) stop('Sum of weights is expected to be 1')
  for(w1 in w){
    if(w1 < 0) stop('all weights must be greater then or equal to 0')
  }
  if (ncol(PM)!=length(w)) stop("length of criteriaWeights should be checked")
  fTypes <- c('default', 'U-shape', 'V-shape', 'level', 'linear', 'Gaussian')
  indifTypes <- c('U-shape', 'level', 'linear')
  indifTypesVector <- rep(F, times=ncri)
  prefTypes <- c('V-shape', 'level', 'linear')
  prefTypesVector <- rep(F, times=ncri)
  if (!(is.vector(d))) stop('preferenceFunction must be a vector')
  for(i in 1:ncri){ #check for consistency of preference function
    if(!(d[i] %in% fTypes)) stop('unsupported type of preference function used only: default, U-shape, V-shape, level, linear, Gaussian supported.')
    if(d[i] %in% indifTypes) indifTypesVector[i] <- T
    if(d[i] %in% prefTypes) prefTypesVector[i] <- T
  }
  if (T %in% prefTypesVector){ # prefference threshold required, check its consistency
    if(is.null(pj)) stop('some choosen preference functions need to have set prefference threshold.')
    if (!is.numeric(pj)) stop('prefferenceThreshold must be a numeric vector')
    if (length(pj) != ncri) stop('number of elements in prefferenceThreshold must be equal to number of criteria (set value to 0 for criteria with preference function that does not require this parameter).')
    for(i in 1:ncri){
      if (prefTypesVector[i] && pj[i] < 0){
        msg <- cat('prefference threshold for criterion ', i, ' set < 0, only non negative values accepted.')
        stop(msg)
      }
    }
  }
  if('Gaussian' %in% d){ # Gaussian type of prefference function check intermediate threshold for consistency
    if (is.null(sj) && is.null(pj) && is.null(qj)) stop('intermediate threshold required (and not possible to derive from indiference and prefference thresholds).')
    if (is.null(sj)) { # try to derive sj from other thresholds
      if (!is.numeric(pj) || !is.numeric(qj) || length(pj) != ncri || length(qj) != ncri) stop('intermediate threshold required (failed to derive it form other thresholds due to its inconsistencies)')
      sj <- (pj + qj)/2
      if (length(sj[sj < 0]) > 0) stop('intermediate threshold required (failed to derive it form other thresholds due to its inconsistencies)')
    }
  }
  #clean-up (remove variables used only for consistency check)
  fTypes     <- NULL
  indifTypes <- NULL
  prefTypes  <- NULL
  indifTypesVector <- NULL
  prefTypesVector  <- NULL
  w1 <- NULL
  ## End of checking the validity of the "inputs"

  nalt <- nrow(PM)  #no. of alternatives
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
  #print(DK) #DEBUG info

  #preference degree (validated in PROMETHEE)
  Pj <- preferenceDegree(nalt, ncri, DK, d, qj, pj, sj, alt, cri)
  #print(Pj) #DEBUG info

  #superiority (Si) and inferiority (Ii) index
  Si <- sapply(1:ncri, function(i) rowSums(Pj[[i]]))
  Ii <- sapply(1:ncri, function(i) colSums(Pj[[i]]))
  rownames(Si) <- alt
  rownames(Ii) <- alt
  colnames(Si) <- cri
  colnames(Ii) <- cri
  # print('Si (superiority)') # DEBUG info
  # print(Si)
  # print('Ii (inferiority)')
  # print(Ii)

  #S-flow a I-flow
  if(SAW){
    #SIR SAW agregation method
    Sflow <- rowSums(sweep(Si, MARGIN=2, w, `*`))
    Iflow <- rowSums(sweep(Ii, MARGIN=2, w, `*`))
  }else{
    #SIR TOPSIS agregation method
    # as oposed to TOPSIS the procedure works separately with Si and Ii matrixes to derive S and I flow
    # Determination of the Ideal (A_ideal) and Anti-ideal (A_anti) Solutions (validated)
    # (as max or minimums in the criteria)
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
    D_ideal <- sqrt(sapply(1:nalt, function(i) sum((Si[i,] - A_ideal)^2)))
    D_anti <- sqrt(sapply(1:nalt, function(i) sum((Si[i,] - A_anti)^2)))
    names(D_ideal) <- alt
    names(D_anti) <- alt
    # print('D ideal') # DEBUG info
    # print(D_ideal)
    # print('D anti ideal')
    # print(D_anti)

    #Step 5. Calculation of the Relative Closeness to the Ideal Solution (validated TOPSIS)
    Sflow <- D_anti/(D_anti + D_ideal) #always 0-1, the closer to 1, the better

    # Determination of the Ideal (A_ideal) and Anti-ideal (A_anti) Solutions (validated TOPSIS)
    # (as max or minimums in the criteria)
    # note that for Ii max and min are switched - min for ideal and max for anti ideal solution
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

    #Step 4. Calculation of the Separation Measures (distance from ideal/anti ideal solution) (validated TOPSIS)
    D_ideal <- sqrt(sapply(1:nalt, function(i) sum((Ii[i,] - A_ideal)^2)))
    D_anti <- sqrt(sapply(1:nalt, function(i) sum((Ii[i,] - A_anti)^2)))
    names(D_ideal) <- alt
    names(D_anti) <- alt

    #Step 5. Calculation of the Relative Closeness to the Ideal Solution (validated TOPSIS)
    Iflow <- D_anti/(D_anti + D_ideal) #always 0-1, the closer to 1, the better
  }

  # first and second complete ranking (according to S and I flows)
  netFlow <- Sflow - Iflow
  relativeFlow <- Sflow / (Sflow + Iflow)
  flows <- cbind(Sflow, Iflow, netFlow, relativeFlow)
  rownames(flows) <- alt
  colnames(flows) <- c('S-flow', 'I-flow', 'n-flow', 'r-flow')
  names(Sflow) <- alt
  names(Iflow) <- alt
  names(netFlow) <- alt
  names(relativeFlow) <- alt
  Sflow <- sort(Sflow, decreasing = T)
  Iflow <- sort(Iflow, decreasing = F)
  netFlow <- sort(netFlow, decreasing = T)
  relativeFlow <- sort(relativeFlow, decreasing = T)
  rankSFlow <- cumsum(c(1, diff(Sflow) != 0))
  rankIFlow <- cumsum(c(1, diff(Iflow) != 0))
  names(rankSFlow) <- names(Sflow)
  names(rankIFlow) <- names(Iflow)

  # partial ranking
  partialRanking <- outer(1:nalt, 1:nalt, Vectorize(function(i, j) {
    if(i == j){
      return(0) #no meaning evaluation alternative with itself
    }else if((rankSFlow[alt[i]] < rankSFlow[alt[j]] && rankIFlow[alt[i]] < rankIFlow[alt[j]]) ||
             (rankSFlow[alt[i]] < rankSFlow[alt[j]] && rankIFlow[alt[i]] == rankIFlow[alt[j]]) ||
             (rankSFlow[alt[i]] == rankSFlow[alt[j]] && rankIFlow[alt[i]] < rankIFlow[alt[j]])){
      return('P') #preference
    }else if(rankSFlow[alt[i]] == rankSFlow[alt[j]] && rankIFlow[alt[i]] == rankIFlow[alt[j]]){
      return('I') #indifference
    }else if(rankSFlow[alt[i]] < rankSFlow[alt[j]] && rankIFlow[alt[i]] > rankIFlow[alt[j]]){
      return('R') #incomparable
    }
  }))
  rownames(partialRanking) <- alt
  colnames(partialRanking) <- alt

  if(VERBOSE){
    print('Flows in the solution')
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
