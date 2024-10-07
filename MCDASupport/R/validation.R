validation_env <- new.env()

#' Validate min-max vector (internal use only)
#'
#' @description
#' Minmax parameter should use only the "min" or "max" values. Appart of
#'  testing this the function also deals with the parameter set as single min
#'  or max (all criteria in same optimization direction) and creates full
#'  vector from this information.
#'
#' Also test if number of criteria in minmax is the same as no. of criteria in
#'  ncri.
#'
#' @name validation$validate_minmax
#' @param minmax vector of min or max values signifying optimization direction
#'  of the criteria
#' @param ncri number of criteria in decision problem
#'
#' @return validated vector of min/max for criteria
validation_env$validate_minmax <- function(minmax, ncri) {
  if (any(!minmax %in% c("min", "max"))) {
    stop("Minmax parameter supports only min/max values")
  }
  if (!is.numeric(ncri)) {
    stop("ncri parameter must be a number (number of criteria)")
  }
  if (minmax == "min" || minmax == "max") {
    return(rep(minmax, times = ncri))
  } else if (length(minmax) != ncri) {
    stop("No. of criteria in ncri does not correspont to no. of it in minmax.")
  }
  return(minmax)
}

#' Validate performance matrix (internal use only)
#'
#' @description
#' Function for validation of the performance matrix. Numeric values are
#'  expected. There must be at least two alternatives and criteria for the
#'  problem do fall under decision making.
#'
#' If validation rules are violated, the run of function will stop with
#'  error message.
#'
#' @name validation$validate_pm
#' @param pm performance matrix
validation_env$validate_pm <- function(pm) {
  ncrit <- ncol(pm)
  if (is.null(dim(pm))) stop("Less than 2 criteria or 2 alternatives")
  if (!(is.matrix(pm) || is.data.frame(pm))) {
    stop("Wrong performance matrix, should be a matrix or a data frame")
  }
  if (ncrit < 2 || nrow(pm) < 2) {
    stop("less than 2 criteria or 2 alternatives")
  }
  if (!is.numeric(unlist(pm))) {
    stop("Only numeric values in performance matrix expected")
  }
}

#' validate weight vector
#'
#' @description
#' Validates that the weights vector is numeric and number of elements in it
#'  is equal to number of criteria.
#'
#' @name validation$validate_w
#' @param w weight vector
#' @param ncri number of criteria
validation_env$validate_w <- function(w, ncri) {
  if (!is.numeric(ncri)) {
    stop("ncri parameter must be a number (number of criteria)")
  }
  if (!is.numeric(w)) {
    stop("Numeric values expected in the weight vector")
  }
  if (length(w) != ncri) {
    stop("No. of elements in weight vector is expected to be same as no. of
         criteria.")
  }
}

#' Validate that sum of weights is equal to 1
#'
#' @name validation$validate_w_sum_eq_1
#' @param w weight vector
validation_env$validate_w_sum_eq_1 <- function(w) {
  if (round(sum(w), 4) != 1) {
    stop("Sum of weights must be equal to 1. If you do not want to use this
         constrain use wsm method instead.")
  }
}



# Export the environment
validation <- as.list(validation_env)