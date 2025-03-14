#' Stepwise Weight Assessment Ratio Analysis
#'
#' @description
#' Introduce by Kersuliene et al in 2010 as a way to derive the weights for
#'  criteria when intitial importance ratio for the criteria is known.
#'
#' The method starts with establishing initial weights (S) in means of relative
#'  importance to first criterium.
#'
#' Next the coefficient K is established as 1 for first criterium and S + 1 for
#'  all other criteria.
#'
#' In step 3 initial weight (q) is established as 1 for first criterium and
#'
#' \mjsdeqn{q_j = \frac{q_{j-1}}{K_j}}
#'
#' for all other criteria.
#'
#' Final relative weight is established by normalizing q:
#'
#' \mjsdeqn{w_j = \frac{q_j}{\sum_{j = 1}^n q_j}}
#'
#' @param relative_weights relative weights of the criteria to first criterium.
#' @param criteria vector of criteria names
#' @return vector of normalized weights for criteria
#'
#' @references
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' @examples
#' criteria <- c("C1", "C2", "C3", "C4")
#' rel_weights <- c(NA, 0.48, 0.35, 0.22)
#' t <- swara(rel_weights, criteria)
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords SWARA weights
swara <- function(relative_weights, criteria) {
  # consistency check
  ncri <- length(criteria)
  if (ncri < 3) stop("It does not make sense to derive weights for < 3 criteria.")
  validation$validate_no_elements_vs_cri(relative_weights, ncri, "relative weights", TRUE)
  # end of consistency check

  # coefficient K
  k <- relative_weights + 1
  k[1] <- 1
  
  # initial weight q
  q <- rep(1, times = ncri)
  for (i in 2:ncri) q[i] <- q[i - 1] / k[i]
  
  # relative weights w
  w <- q / sum(q)
  names(w) <- criteria
  return(w)
}