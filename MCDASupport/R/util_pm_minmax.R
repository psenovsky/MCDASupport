#' Process performance metrix - reverts minimalized criteria in the matrix
#'
#' @description
#' Internal function the package uses to prepare performace matrix for
#'  computations. It processes the performance matrix and transfers scales of
#'  minimized criteria to maximized ones.
#'
#' @param PM performance matrix - criteria are expected in the columns,
#'  alternatives in rows, only numeric values allowed.
#' @param minmaxcriteria provides either 'min' or 'max' (default) values if all
#'  criteria should be minimalized or maximalized. If it is not case provide
#'  vector specifying either 'max' or 'min' for each criterion.
#'
#' @returns
#' processed performance matrix with all criteria to be maximized
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
#'   c(0.4545455, 0.1814159, 0.9166667, 0.1198582, 0, 0.4, 0.000),
#'   c(0.1818182, 0.6283186, 0.5833333, 0.0000000, 0, 0.4, 0.125)
#' )
#' rownames(M) <- alternatives
#' colnames(M) <- criteria
#' minmaxcriteria <- c('max', 'max', 'min', 'min', 'max', 'max', 'max')
#' maxed <- util_pm_minmax(M, minmaxcriteria)
util_pm_minmax <- function(PM, minmaxcriteria = "max") {
  minmax <- util_prepare_minmaxcriteria(ncol(PM), minmaxcriteria)
  if (length(minmax[minmax %in% c("min")]) > 0) {
    # some criteria are minimalized - need to invert them
    i <- 0
    for (r in minmax) {
      i <- i + 1
      if (r == "min") { #invert scale
        valmax <- max(PM[, i])
        PM[, i] <- valmax - PM[, i]
      }
    }
  }
  PM <- as.data.frame(PM)
  return(PM)
}
