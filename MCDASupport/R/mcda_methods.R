#' Compare ranks using MCDA methods
#' 
#' @description
#' helper function, which allows to compare results achieved using MCDA
#'  methods provided as vector in methods parameter. 
#' 
#' #' @param pm performance matrix criteria in columns, alternatives in rows,
#'  numeric values expressed
#' @param w weights vector
#' @param minmax 'min' or 'max' to specify cost or benefit criterion, max is
#'  default value
#' @param methods specify MCDA methods, see list of supported methods in
#'  \link{mcda_method}
#' 
#' @return dataframe with methods in rows and ranked alternatives in columns
#' 
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @examples
#' alternatives <- c('BLR', 'BOM', 'DEL', 'MNL', 'HYD', 'GRU', 'DUB',
#'  'KRK', 'MAA', 'EZE')
#' criteria <- c('tlnt', 'stab', 'cost', 'infl', 'tm-zn', 'infr', 'life')
#' pm <- rbind(
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
#' rownames(pm) <- alternatives
#' colnames(pm) <- criteria
#' w <- c(0.125, 0.2, 0.2, 0.2, 0.175, 0.05, 0.05)
#' methods <- c("wsm", "aras", "borda", "copeland", "copras", "cradis", "edas", "fuca", "mabac",
#'             "mairca", "marcos", "maut", "moora", "moosra", "msim", "ocra", "piv", "probid",
#'             "psi", "ram", "rawec", "regime", "rov", "saw", "sprobid", "todim", "topsis", "wpm")
#' t <- mcda_methods(pm = pm, w = w, minmax = "max", methods = methods)
mcda_methods <- function(pm, w, minmax = "max", methods = "wsm") {
  # validation
  nmethods <- c("wsm", "aras", "borda", "copeland", "copras", "cradis", "edas",
    "fuca", "mabac", "mairca", "marcos", "maut", "moora", "moosra", "msim",
    "ocra", "piv", "probid", "psi", "ram", "rawec", "regime", "rov", "saw",
    "sprobid", "todim", "topsis", "wpm")
  validation$validate_invalid_val(methods, nmethods, "MCDA method")
  # end of validation
  n <- length(methods)
  result <- as.data.frame(matrix(0, ncol = 10, nrow = n))
  for (i in 1:n) {
    t <- mcda_method(pm = pm, w = w, minmax = "max", method = methods[i])
    result[i, ] <- t$result$rank
  }
  colnames(result) <- alternatives
  rownames(result) <- methods
  return(result)
}