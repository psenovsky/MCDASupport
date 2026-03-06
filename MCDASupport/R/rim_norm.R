#' Reference Ideal Model Normalization
#'
#' @description
#' Normalization of values used in multi-criteria decision analysis method RIM.
#'
#' The normalization is based on distance from reference ideal. Consider
#'  choosing of new emploee with one criterium being age. Supose that we
#'  consider possible age range [A, B] to [23, 65] years, with ideal reference
#'  [C, D] in [30, 35].
#'
#' Considering the above we can say that A < C and D < B (reference interval is
#'  inside of possible range of values).
#'
#' The normalization of x is done by computing:
#'
#' \mjsdeqn{d_{min}(X, [C, D]) = min( |x - C|, |x - D|)}
#'
#' and three possible scenarios. In first the scenario the value to normalize
#'  is in reference ideal inerval [C, D]:
#'
#' \mjsdeqn{f(x, [A, B], [C, D]) = 1}
#'
#' In second scenario, the to normalize value is in interval [A, C]:
#'
#' \mjsdeqn{f(x, [A, B], [C, D]) = 1 - \frac{d_{min}(x, [C, D])}{|A - C|}}
#'
#' Finally the value to normalize can be in the interval [D, B]:
#'
#' \mjsdeqn{f(x, [A, B], [C, D]) = 1- \frac{d_{min}(x, [C, D])}{|D - B|}}
#'
#' In other words the normalization approach focusses on establishing how far
#'  is the value from reference ideal.
#'
#' @param tonorm numbers vector to normalize
#' @param A lower limit of the range
#' @param B upper limit of the range
#' @param C lower limit of reference ideal
#' @param D upper limit of reference ideal
#'
#' @return normaluzed vector of numbers
#'
#' @examples
#' tonorm <- c(25, 30, 35, 40, 50)
#' A <- 23
#' B <- 60
#' C <- 30
#' D <- 35
#' t <- rim_norm(tonorm, A, B, C, D)
#' 
#' @references
#' CABLES, E., LAMATA, M. T., VERDEGAY, J. L. RIM-reference ideal method in
#'  multicriteria decision making. Information Sciences. 2016, Vol. 337–338,
#'  pp. 1–10, available from: https://doi.org/10.1016/j.ins.2015.12.011,
#'  ISSN 0020-0255.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords RIM
rim_norm <- function(tonorm, A, B, C, D) {
  ## validate
  if (!is.vector(tonorm, mode = "numeric")) {
    stop("tonorm paramerer expected to be numeric vector.")
  }
  t <- c(A, B, C, D)
  if (!is.vector(t, mode = "numeric")) {
    stop("ranges [A, B] and reference ideals [C, D] need to be numbers.")
  }
  if (A > B || A > C) {
    stop("Value of lower range limit needs to be lower then its upper limit and
     lower limit of reference ideal.")
  }
  if (D < C || D > B) {
    stop(
      "Value of upper reference ideal needs to be higher then its lower limit
       and upper limit of range."
    )
  }
  # end of validation

  norm <- rep(0, times = length(tonorm))
  for (i in 1:length(tonorm)) {
    f1 <- min(abs(tonorm[i] - C), abs(tonorm[i] - D))
    if (tonorm[i] <= C) {
      norm[i] <- 1 - f1 / abs(A - C)
    } else if (tonorm[i] > C && tonorm[i] <= D) {
      norm[i] <- 1
    } else {
      norm[i] <- 1 - f1 / abs(B - D)
    }
  }

  return(norm)
}