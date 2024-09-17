#' function for creating a sequence +/- 10\% of valaue
#'
#' @description
#' Function returns sequence +/- 10 \% of given number by 1 \%. The function is
#'  used by sensitivity analysis functions in version 0.30 of the package.
#'
#' For newer versions of the package the function is depreciated and will be
#'  eventualy removed.
#'
#' @param x a number for which the sequence should be generated
#'
#' @return vector of numbers +/- 10 \% of x by 1 \%.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords sensitivity
tenPercent <- function(x) {
  if (!is.numeric(x)) {
    stop("Numeric value as base for percentage computation expected")
  }
  if (x != 0) {
    t <- seq(from = x - x / 10, to = x + x / 10, by = x / 100)
  } else {
    t <- rep(0, times = 21)
  }
  return(t)
}
