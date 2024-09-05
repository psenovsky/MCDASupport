#' Identify alternatives, which clearly dominate others
#'
#' @description
#' Takes normalized performance matrix (criteria in columns, alternatives in
#'  rows) and identifies the alternatives, that are being dominated by other
#'  alternatives. Dominated alternatives are prime candidates for deletion and
#'  thus simplification of decision problem.
#'
#' Function returns matrix m to m with 1 identifying alternative being
#'  dominated.
#'
#' @param m Normalized performance matrix - criteria in columns and
#'  alternatives in rows.
#' @param minmaxcriteria either value (min or max) or vector providing guidance
#'  for orientation of the criteria in preference matrix. If parameter provides
#'  only single max or min value, the function will presume usige of this
#'  orientation for all criteria.
#'
#' @return
#' list:
#'
#' \item{dominationMatrix}{Returns matrix m to m with 1 identifying alternative
#'  being dominated. 1 in position ij if alternative i dominates alternative j.}
#' \item{dominatedAlternatives}{vector of alternatives names, which have been
#'  identified as dominated}
#'
#' @references
#' De Brouwer, Philippe J. S.: "The The Big R-Book: From Data Science to
#'  Learning Machines and Big Data ", Wiley, 2020, 928 p.,
#'  ISBN 978-1119632726.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @examples
#' alternatives <- c('BLR', 'BOM', 'DEL', 'MNL', 'HYD', 'GRU', 'DUB', 'KRK',
#'  'MAA', 'EZE')
#' criteria <- c('tlnt', 'stab', 'cost', 'infl', 'tm-zn', 'infr', "life")
#' M <- rbind(
#'   c(0.8181818, 0.1814159, 1.0000000, 0.1198582, 0, 0.6, 0.750),
#'   c(1.0000000, 0.1814159, 0.6666667, 0.1198582, 0, 0.6, 0.375),
#'   c(1.0000000, 0.1814159, 0.8333333, 0.1198582, 0, 0.6, 0.125),
#'   c(0.8181818, 0.0000000, 1.0000000, 0.3482143, 0, 0.6, 0.375),
#'   c(0.1818182, 0.1814159, 1.0000000, 0.1198582, 0, 0.2, 0.375),
#'   c(0.1818182, 0.1814159, 0.5000000, 0.1198582, 0, 0.2, 0.125),
#'   c(0.0000000, 1.0000000, 0.0000000, 0.5741667, 1, 1.0, 1.000),
#'   c(0.3636364, 0.7787611, 0.6666667, 1.0000000, 1, 0.0, 0.500),
#'   c(0.4545455, 0.1814159, 0.9166667, 0.1198582, 0, 0.4, 0.000),
#'   c(0.1818182, 0.6283186, 0.5833333, 0.0000000, 0, 0.4, 0.125)
#' )
#' rownames(M) <- alternatives
#' colnames(M) <- criteria
#' dominated <- mcda_get_dominated(M)
#'
#' @keywords MCDA domination
mcda_get_dominated <- function(m, minmaxcriteria = "max") {
  #check validity of inputs
  ncrit <- ncol(m)
  if (nrow(m) < 2 || ncrit < 2) {
    stop("Performance matrix must have at minimum 2 alteratives and criteria")
  }
  if (!(is.matrix(m) || (is.data.frame(m)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }

  pm <- util_pm_minmax(m, minmaxcriteria)
  nalt  <- nrow(pm)

  dom <- matrix(data = 0, nrow = nalt, nalt)
  colnames(dom) <- rownames(dom) <- rownames(m)
  dominated_ones <- rep(0, nalt)
  names(dominated_ones) <- rownames(m)

  for (i in 1:nalt) {
    for (j in 1:nalt) {
      if (i == j) next
      if (all(pm[i, ] >= pm[j, ])) {
        dom[j, i] <- 1
        dominated_ones[j] <- 1
      }
    }
  }
  dominated_alternatives <- names(dominated_ones[dominated_ones == 1])

  out <- list(
    dominationMatrix = t(dom),
    dominatedAlternatives = dominated_alternatives
  )
  return(out)
}
