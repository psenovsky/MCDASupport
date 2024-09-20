#' parameter check for WSM and WSM-like methods (internal)
#'
#' @description
#' The function checks parameters for \link{wsm} function.
#'
#' Package has implemented some other functions which share these checks, such
#'  as \link{saw} and the function helps to keep the code DRY.
#'
#' Function is intended for internal use only.
#'
#' @keywords SAW WSM
param_check_wsm <- function(pm, w, minmax = "max") {
  ncrit <- ncol(pm)
  if (!(is.matrix(pm) || (is.data.frame(pm)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (ncrit < 2 || nrow(pm) < 2) {
    stop("less than 2 criteria or 2 alternatives")
  }
  if (!is.numeric(unlist(pm))) {
    stop("Only numeric values in performance matrix expected")
  }
  if (!is.vector(w, mode = "numeric")) {
    stop("weights should be a numeric vector")
  }
  if (length(w) != ncrit) {
    stop("number of weights does not correspond to number of criteria being
         used in preference matrix")
  }
  pm2 <- util_pm_minmax(pm, minmax)
  return(pm2)
}