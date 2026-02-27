#' Summary method for borda objects
#'
#' @param obj borda object to process summary for
#'
#' @export
summary.borda <- function(obj) {
  obj$summary()
}