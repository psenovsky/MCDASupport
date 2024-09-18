#' validates and prepares minmax vector for processing
#'
#' @description
#' Internal function package uses to validate vector containing information on
#'  criteria optimization direction (minimizaing or maximizing). Since this
#'  operation is required for almost all decision analysis functions, it has
#'  been refactored into separate function.
#'
#' If provided with direction to min or max - it will create the minmaxcriteria
#'  vector minimizing or maximizing all the criteria.
#'
#' @param ncrit number of criteria used in decision problem (must be > 2)
#' @param minmaxcriteria provides either 'min' or 'max' (default) values if all
#'  criteria should be minimalized or maximalized. If it is not case provide
#'  vector specifying either 'max' or 'min' for each criterion.
#'
#' @return
#' Returns validated and completed vector with information on minimizing or
#'  maximizing criteria.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @examples
#' # 5 maximizzed criteria
#' minmax <- util_prepare_minmaxcriteria(5)
#' # 5 minimized criteria
#' minmax <- util_prepare_minmaxcriteria(5, minmaxcriteria = 'min')
#' # mix
#' minmax <- util_prepare_minmaxcriteria(5, c('min', 'min', 'max', 'max',
#'  'min'))
util_prepare_minmaxcriteria <- function(ncrit, minmaxcriteria = "max") {
  # validate parameters
  if (length(minmaxcriteria) == 1) {
    if (!is.numeric(ncrit)) stop("expected number of criteria to be a number")
    if (ncrit < 2) stop("at minimum 2 criteria need to be considered")
  }
  minmax <- tolower(minmaxcriteria)
  if (any(!(minmax %in% c("min", "max")))) {
    stop("Vector minmaxcriteria must contain max or min")
  }
  if (is.vector(minmaxcriteria) && length(minmaxcriteria) > 1) {
    if (ncrit != length(minmax)) {
      stop("length of minmaxcriteria does not correspont with number of
           criteria in performance matrix")
    }
  } else {
    minmax <- rep(minmaxcriteria, times = ncrit)
  }
  return(minmax)
}
