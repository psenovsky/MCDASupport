#' Summary method for moora objects
#'
#' @param obj moora object to process summary for
#'
#' @export
summary.moora <- function(obj) {
  obj$summary()
}