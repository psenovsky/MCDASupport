#' Pivot Pairwise Relative Criteria Importance Assessment
#'
#' @description
#' New spin on \link{swara} method. PIPRECIA is believed to be more effective
#'  in complex decision-making situations.
#'
#' First five steps follow SWARA method. The 6-th step allows for aggregation
#'  of the weights derived from preferences of individual decision-makers. 
#' 
#' \mjsdeqn{w_j^* = (\prod_{m = 1}^M w_j^m)^{1/M}}
#' 
#' \mjsdeqn{w_j = \frac{w_j^*}{\sum_{j = 1}^m w_j^*}}
#'
#' @param relative_weights dataframe of relative weights of the criteria to
#'  first criterium. Each column has a preference of one decision-maker.
#' @param criteria dataframe of criteria names ordered in descending manner.
#'  Each decision maker in separate column
#' @return vector of normalized weights for criteria
#'
#' @references
#' Chakrakborty, S., Chatterjee, P. Das, P. P. Multi-Criteria Decision Making
#'  Methods in Manufacturing Environments: Models and Applications. CRC Press,
#'  Boca Raton, 2024, 450 p., ISBN: 978-1-00337-703-0
#'
#' @examples
#' criteria <- data.frame(
#'   DM1 = c("C1", "C2", "C3", "C4"),
#'   DM2 = c("C1", "C3", "C2", "C4"),
#'   DM3 = c("C1", "C3", "C4", "C2")
#' )
#' rel_weights <- data.frame(
#'   DM1 = c(NA, 0.48, 0.35, 0.22),
#'   DM2 = c(NA, 0.5, 0.4, 0.3),
#'   DM3 = c(NA, 0.4, 0.3, 0.2)
#' )
#' t <- piprecia(rel_weights, criteria)
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords SWARA weights PIPRECIA
piprecia <- function(relative_weights, criteria) {
  # consistency check
  nDM <- ncol(relative_weights)
  ncri <- nrow(relative_weights)
  if (ncol(relative_weights) != ncol(criteria)) {
    stop("All decision makers need to state both criteria and relative weights")
  }
  # end of consistency check
  w <- relative_weights
  wt <- rep(0, times = ncri)
  for (j in 1:nDM) {
    wt <- swara(relative_weights[, j], criteria[, j])
    if (j == 1) {
      w[, 1] <- wt
    } else {
      reorder_idx <- match(criteria[, 1], criteria[, j])
      w[, j] <- wt[reorder_idx]
    }
  }
  w2 <- rep(1, times = ncri)
  for (j in 1:nDM) {
    w2 <- w2 * w[, j]^j
  }
  w2 <- w2^(1/nDM)
  w3 <- w2 / sum(w2)
  names(w3) <- criteria[, 1]
  return(w3)
}