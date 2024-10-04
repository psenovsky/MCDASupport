#' parameter checking form MARCOS function (internal function)
#'
#' @description
#' function perform validation of the provided parameters. The validation is
#'  used by both \link{marcos} and \link{cradis} functions.
#'
#' If all validations yield no error, the function returns adjusted minmax
#'  vector. If the error is detected then the run of the process is stopped
#'  with appropriate error message.
#'
#' Function is intended for internal use only.
#'
#' @param pm original (not normalized) performance matrix
#' @param w weights of the criteria
#' @param minmax vector of criteria direction to be used during
#'  performance matrix normalization. Can be replaced with single max or
#'  min if all criteria are to be maximized or minimized. Implicitly set
#'  to max.
#'
#' @return minmax vector (for situations when the minmax parameters is set as
#'  single min or max value)
#'
#' @keywords MARCOS CRADIS
param_check_marcos <- function(pm, w, minmax = "max") {
  # utility functions
  # @description
  # checks if the param is string or vector of strings
  #
  # @param param string or vector of strings to check
  # @return TRUE if the param is single stringm otherwise returns FALSE
  #
  # @examples
  # check_string("hello")  # Returns TRUE
  # check_string(c("hello", "world"))  # Returns FALSE
  # check_string(123)  # Returns FALSE
  check_string <- function(param) {
    if (is.character(param) && length(param) == 1) {
      return(TRUE)  # It's a single string
    } else {
      return(FALSE)  # It's a vector of strings or not a string at all
    }
  }

  # validation of the parameters
  if (is.null(dim(pm))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(pm) || (is.data.frame(pm)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (!is.numeric(unlist(pm))) {
    stop("Only numeric values in performance matrix expected")
  }
  if (!is.vector(w, mode = "numeric")) {
    stop("criteria weights should be a vector")
  }
  ncri <- length(w)
  if (ncri != ncol(pm)) {
    stop("Number of criteria in weight vector and performance matrix must
         be same")
  }
  if (any(!minmax %in% c("min", "max"))) {
    stop("Minmax parameter supports only min/max values")
  }
  # check for single min or max value
  if (check_string(minmax)) {
    mm <- rep(minmax, times = ncri)
  } else {
    mm <- minmax
  }
  return(mm)
  # end of validation
}