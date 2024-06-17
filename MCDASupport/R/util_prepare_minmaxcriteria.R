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
