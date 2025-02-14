#' Summary method for smart objects
#'
#' @param obj smart object to process summary for
#'
#' @export
summary.smart <- function(obj) {
  obj$summary()
}