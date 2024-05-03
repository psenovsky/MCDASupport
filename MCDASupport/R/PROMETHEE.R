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
  t <- promethee_param_check(PM, preferenceFunction, w, indifferenceTreshold, prefferenceThreshold,
                             intermediateThreshold)
  if (!t) stop("Error checking parameters")
  ## End of checking the validity of the "inputs"

  nalt <- nrow(PM)  #no. of alternatives
  ncri <- ncol(PM)
  alt  <- rownames(PM) #list of alternatives
  cri  <- colnames(PM) #list of criteria
  qj   <- indifferenceTreshold
  pj   <- prefferenceThreshold
  sj   <- intermediateThreshold
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
