#' Summary method for r_method objects
#'
#' @param obj r_method object to process summary for
#'
#' @export
summary.r_method <- function(obj) {
  obj$summary()
}