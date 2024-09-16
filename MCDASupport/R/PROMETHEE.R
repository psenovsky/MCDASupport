#' general method for computations of preference flows in PROMETHEE methods
#'  (for internal use only)
#'
#' @description
#' PROMETHEE stands for Preference ranking organization method for enrichment
#'  evaluation. Is family of populat alternatives outranking computation
#'  methods.It constructs its recomendations based on positive and negative
#'  preference flows between the alternatives.
#'
#' This function provides general function implementing computation of these
#'  flows to be used by PROMETHEE I, II and III methods (see \link{promethee1},
#'  \link{promethee2} and \link{promethee3}).
#'
#' We start this computation with measuring differences in performance between
#'  the alternatives in criteria: \mjseqn{D_k(a,b) > PM_k(a) - PM_k(b)}.
#'
#' Where D ... defference in performance in criterion k, PM ... performance of
#'  alternative a or b in criterium k.
#'
#' Based on this difference preference degree (PD) is computed.PROMETHEE
#'  methods support variety of functions describing the nature of this
#'  transformation. Method supports as default binary transformation to PD = 0
#'  if \mjseqn{D_k \le 0} and PD = 1 otherwise. Other types of transformation
#'  are also available.
#'
#' U-shape compares the performance difference to indifference threshold.
#'  PD = 0 if \mjseqn{D_k \le q_k}, PD = 1 otherwise.
#'
#' V-shape transforms PD = 0, for \mjseqn{D_k \le 0}, for positive values of D
#'  up to \mjseqn{D_k \le p_k}, the \mjseqn{PD  = \frac{D_k}{p_k}} and PD = 1
#'  otherwise.
#'
#' Level function transforms PD = 0, for \mjseqn{D_k \le q_k}, for
#'  \mjseqn{q_k < D_k <le p_k} PD = 0.5 and PD = 1 otherwise.
#'
#' Similarly linear function transforms PD = 0, for \mjseqn{D_k \le q_k}, for
#'  \mjseqn{q_k < D_k \le p_k} \mjseqn{PD = \frac{D_k - q_k}{p_k - q_k}},
#'  otherwise PD = 1.
#'
#' Finaly in gaussian function PD = 0 for \mjseqn{D_k \le 0} otherwise
#'  \mjseqn{PD = 1 - e^{-\frac{D_k^2}{2s_k^2}}}.
#'
#' Where PD ... preference degree, q ... is indifference threshold, p ... is
#'  preference threshold, s ... intermediate threshold, k ... criterium to be
#'  evaluated.
#'
#' Positive and negative flows are derived directly from PD. PD above was
#'  describes as a derivate from \mjseqn{D_k}. Formally both are matrixes
#'  describing how one alternative is better then the other.
#'
#' The flow can be expressed
#'
#' \mjsdeqn{P^{+} = w_k \cdot \frac{\sum PD_i}{n - 1}}
#'
#' \mjsdeqn{P^{-} = w_k \cdot \frac{\sum PD_j}{n - 1}}
#'
#' Where P ... is the flow (+ positive, - negative), i ... are rows, j ... are
#'  columns, n ... number of alternatives, w ... weight of criterium k. In
#'  other words both flows represent average of how much is alternative a
#'  better (positive flow) or inferior (negative flow) when compared to
#'  alternative b. The sum is over the rows of PD for positive and over the
#'  columns of PD for negative flow respectively.
#'
#' Method also provides net flow, computed as
#'
#' \mjsdeqn{P_{net} = P^+ - P^-}
#'
#' @param PM Matrix or data frame containing the performance table. Each row
#'  corresponds to an alternative, and each column to a criterion. only numeric
#'  values expercted. Rows and columns are expected to be named.
#' @param preferenceFunction vector, specifies type of function used to compute
#'  preferences. Need to be set for each criterion. Possible values are:
#'  'default', 'U-shape', 'V-shape', 'level', 'linear', 'Gaussian'. Choice of
#'  function type will decide on what type of threshold (if any) is required
#'  for computation. Each criterion can use different preference function.
#' @param w vector containing the weights of the criteria. Values need to
#'  \mjseqn{0 \le w_i \le 1, \sum w_i = 1}
#' @param indifferenceTreshold vector containing indifference threshods for
#'  criteria. Not all types of performance functions require it. The parameter
#'  must be used if there is at least one criterion, for which it is required.
#'  Values for all other criteria should be 0 (and will not be used during
#'  computations). Only 'U-shape', 'level', 'linear' functions need this
#'  threshold.
#' @param prefferenceThreshold vector containing prefference threshods for
#'  criteria. Not all types of performance functions require it. The parameter
#'  must be used if there is at least one criterion, for which it is required.
#'  Values for all other criteria should be 0 (and will not be used during
#'  computations). Only 'V-shape', 'level', 'linear' functions need this
#'  threshold.
#' @param intermediateThreshold vector containing intermetiate thresholds for
#'  criteria. only Gaussian type performance functions rewuire this type of
#'  threshold. If prefference and indifference thresholds are present, the
#'  PROMETHEE function will try to 'gues' intermediate threshold as value
#'  right in the middle between these thresholds.
#'
#' @return
#' The function returns a list structured as follows:
#' \itemize{
#'   \item positiveFlowCriteria - matrix of size no. alternatives x no. of
#'  criteria representing how the alternative is preffered in criterium
#'  compared to other alternatives
#'   \item negativeFlowCriteria - matrix of size no. alternatives x no. of
#'  criteria representing how the alternative is outranked in criterium
#'  compared to other alternatives
#'   \item netFlowCriteria - matrix of size no. alternatives x no. of
#'  criteria representing overal evaluation of the flows
#'   \item weightedPositiveFlowCriteria - matrix of size no. alternatives x no.
#'  of criteria representing how the alternative is preffered in criterium
#'  compared to other alternatives with weights applied to them
#'   \item weightedNegativeFlowCriteria - matrix of size no. alternatives x no.
#'  of criteria representing how the alternative is outranked in criterium
#'  compared to other alternatives with weights applied to them
#'   \item weightedNetFlowCriteria - matrix of size no. alternatives x no. of
#'  criteria representing overal evaluation of the flows (with weights applied
#'  to them)
#'   \item positiveFlow - vector representing how the alternative is preffered
#'  to other alternatives
#'   \item negativeFlow vector representing how altenative is outranked by
#'  other alternatives
#'   \item netFlow - vector representing diferences between positive and
#'  negative flows for the alternative across criteria
#'   \item preferenceDegreeUnw - list of matrixes with unweighted preferences,
#'  separate matrix for each criterion
#'   \item pairweiseComparison - list of matrixes measuring nominal differences
#'  in alternatives performance in criterions. Separate matrixes are
#'  constructed for each criterion.
#' }
#'
#' @references
#' ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca
#'  Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords PROMETHEE promethee1 promethee2 promethee3 positive negative net
#' @keywords flow
#'
#' @examples
#' # Example from Fuzzy TOPSIS book (see references)
#' # ammended error in tab. 4.9, the computation presumes maximization of all
#' # criteria
#' PM <- cbind(
#'    c(80, 65, 83, 40, 52, 94),
#'    c(90, 58, 60, 80, 72, 96),
#'    c(600, 200, 400, 1000, 600, 700),
#'    c(54, 97, 72, 75, 20, 36),
#'    c(8, 1, 4, 7, 3, 5),
#'    c(5, 1, 7, 10, 8, 6)
#' )
#' colnames(PM) <- c('C1', 'C2', 'C3', 'C4', 'C5', 'C6')
#' rownames(PM) <- c('A1', 'A2', 'A3', 'A4', 'A5', 'A6')
#' minmax <- 'max'
#' shape <- c('U-shape', 'V-shape', 'linear', 'level', 'default', 'Gaussian')
#' p <- c(10, 0, 450, 50, 0, 0) #indifference threshold
#' q <- c(0, 30, 50, 10, 0, 0) #prefference threshold
#' s <- c(0,0,0,0,0,5) #intermediate threshold
#' w <- c(0.1667, 0.1667, 0.1667, 0.1667, 0.1667, 0.1665)
#' result <- PROMETHEE(PM, shape, w, q, p, s)
PROMETHEE <- function(PM, preferenceFunction, w, indifferenceTreshold = NULL, prefferenceThreshold = NULL,
                      intermediateThreshold = NULL) {
  ## check validity of the objects manipulated by the current function
  # with < 2 criteria or 2 alternatives, there is no MCDA problem
  t <- promethee_param_check(PM, preferenceFunction, w, indifferenceTreshold, prefferenceThreshold,
                             intermediateThreshold)
  ## End of checking the validity of the "inputs"

  nalt <- nrow(PM)  #no. of alternatives
  ncri <- ncol(PM)
  alt  <- rownames(PM) #list of alternatives
  cri  <- colnames(PM) #list of criteria
  qj   <- indifferenceTreshold
  pj   <- prefferenceThreshold
  sj   <- t
  
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
