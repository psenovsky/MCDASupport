# Function provides basic computations for PROMETHEE I, II and III methods.
# It computes positive and negative preference flows which are used as basis
# of recomendation formulation.
#
# parameters
#   PM - performance matrix
#   preferenceFunction - vector of preference functions types to derive
#                        preference when comparing PM
#   w - weights
#   indifferenceTreshold list of indifference thresholds
#   prefferenceThreshold list of prefference thresholds
#   intermediateThreshold list of intermediate thresholds for Gaussian
#                         function type
PROMETHEE <- function(PM, preferenceFunction, w, indifferenceTreshold = NULL, prefferenceThreshold = NULL,
                      intermediateThreshold = NULL) {
  ## check validity of the objects manipulated by the current function
  # with < 2 criteria or 2 alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(PM) || (is.data.frame(PM)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (!is.numeric(unlist(PM))) {
    stop("Only numeric values in performance matrix expected")
  }
  ncri <- ncol(PM)  #no. of criteria
  qj   <- indifferenceTreshold
  pj   <- prefferenceThreshold
  sj   <- intermediateThreshold
  fTypes <- c("default", "U-shape", "V-shape", "level", "linear", "Gaussian")
  indifTypes <- c("U-shape", "level", "linear")
  indifTypesVector <- rep(FALSE, times = ncri)
  prefTypes <- c("V-shape", "level", "linear")
  prefTypesVector <- rep(FALSE, times = ncri)
  if (!(is.vector(preferenceFunction))) {
    stop("preferenceFunction must be a vector")
  }
  for (i in 1:ncri) { #check for consistency of preference function
    if (!(preferenceFunction[i] %in% fTypes)) {
      stop("unsupported type of preference function used only: default,
           U-shape, V-shape, level, linear, Gaussian supported.")
    }
    if (preferenceFunction[i] %in% indifTypes) indifTypesVector[i] <- TRUE
    if (preferenceFunction[i] %in% prefTypes) prefTypesVector[i] <- TRUE
  }
  if (TRUE %in% indifTypesVector) {
    # indifference threshold required, check its consistency
    if (!is.vector(qj, mode = "numeric")) {
      stop("indifferenceTreshold must be a numeric vector")
    }
    if (length(qj) != ncri) {
      stop("number of elements in indifferenceTreshold must be equal to number
           of criteria (set value to 0 for criteria with preference function
           that does not require this parameter).")
    }
    for (i in 1:ncri) {
      if (indifTypesVector[i] && qj[i] < 0) {
        msg <- cat("indifference threshold for criterion ", i,
                   " set < 0, only non negative values accepted.")
        stop(msg)
      }
    }
  }
  if (TRUE %in% prefTypesVector) {
    # prefference threshold required, check its consistency
    if (!is.vector(pj, mode = "numeric")) {
      stop("prefferenceThreshold must be a numeric vector")
    }
    if (length(pj) != ncri) {
      stop("number of elements in prefference threshold must be equal to number
           of criteria (set value to 0 for criteria with preference function
           that does not require this parameter).")
    }
    for (i in 1:ncri) {
      if (prefTypesVector[i] && pj[i] < 0) {
        msg <- cat("prefference threshold for criterion ", i,
                   " set < 0, only non negative values accepted.")
        stop(msg)
      }
    }
  }
  if ("Gaussian" %in% preferenceFunction) {
    # Gaussian type of prefference function check intermediate threshold
    # for consistency
    if (is.null(sj) && is.null(pj) && is.null(qj)) {
      stop("intermediate threshold required (and not possible to derive from
           indiference and prefference thresholds).")
    }
    if (is.null(sj)) { # try to derive sj from other thresholds
      if (!is.numeric(pj) || !is.numeric(qj) || length(pj) != ncri ||
            length(qj) != ncri) {
        stop("intermediate threshold required (failed to derive it form other
             thresholds due to its inconsistencies)")
      }
      sj <- (pj + qj) / 2
      if (length(sj[sj < 0]) > 0) {
        stop("intermediate threshold required (failed to derive it form other
             thresholds due to its inconsistencies)")
      }
    }
  }
  if (!is.numeric(w) || length(w) != ncri) {
    stop("weights not set for all criteria.")
  }

  #clean-up (remove variables used only for consistency check)
  fTypes     <- NULL
  indifTypes <- NULL
  prefTypes  <- NULL
  indifTypesVector <- NULL
  prefTypesVector  <- NULL
  ## End of checking the validity of the "inputs"

  nalt <- nrow(PM)  #no. of alternatives
  alt  <- rownames(PM) #list of alternatives
  cri  <- colnames(PM) #list of criteria

  #pairwaise comparison (validated)
  DK <- lapply(1:ncri, function(k) {
    DKf <- outer(1:nalt, 1:nalt, Vectorize(function(i, j) PM[i, k] - PM[j, k]))
    rownames(DKf) <- alt
    colnames(DKf) <- alt
    DKf
  })
  names(DK) <- cri

  #preference degree (validated)
  Pj <- preferenceDegree(nalt, ncri, DK, preferenceFunction, qj, pj, sj,
                         alt, cri)

  #criteria flows (validated)
  pf_cri <- sapply(1:ncri, function(k) {
    t <- Pj[[k]]
    sapply(1:nalt, function(i) sum(t[i, ]) / (nalt - 1))
  })
  nf_cri <- sapply(1:ncri, function(k) {
    t <- Pj[[k]]
    sapply(1:nalt, function(i) sum(t[, i]) / (nalt - 1))
  })
  net_cri <- pf_cri - nf_cri
  colnames(pf_cri)  <- cri
  colnames(nf_cri)  <- cri
  colnames(net_cri) <- cri
  rownames(pf_cri)  <- alt
  rownames(nf_cri)  <- alt
  rownames(net_cri) <- alt

  #weighted unicriteria flows (validated)
  pf_cri_w <- sweep(pf_cri, MARGIN = 2, w, `*`)
  nf_cri_w <- sweep(nf_cri, MARGIN = 2, w, `*`)
  net_cri_w <- pf_cri_w - nf_cri_w
  colnames(pf_cri_w)  <- cri
  colnames(nf_cri_w)  <- cri
  colnames(net_cri_w) <- cri
  rownames(pf_cri_w)  <- alt
  rownames(nf_cri_w)  <- alt
  rownames(net_cri_w) <- alt

  #global preference flows
  pf <- rowSums(pf_cri_w)
  nf <- rowSums(nf_cri_w)
  netf <- pf - nf
  names(pf) <- alt
  names(nf) <- alt
  names(netf) <- alt

  out <- list(
    positiveFlowCriteria = pf_cri,
    negativeFlowCriteria = nf_cri,
    netFlowCriteria = net_cri,
    weightedPositiveFlowCriteria = pf_cri_w,
    weightedNegativeFlowCriteria = nf_cri_w,
    weightedNetFlowCriteria = net_cri_w,
    positiveFlow = pf,
    negativeFlow = nf,
    netFlow = netf,
    preferenceDegreeUnw = Pj,
    pairweiseComparison = DK
  )
  return(out)
}
