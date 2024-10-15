validation_env <- new.env()

#' Validate consistency of thresholds
#'
#' @name validation$validate_electre_pqv
#' @param p preference threshold
#' @param q indifference threshold
#' @param v veto threshold
#' @param cri vector of names for criteria
validation_env$validate_electre_pqv <- function(p, q, v, cri) {
  ncri <- length(cri)
  validation_env$validate_no_elements_vs_cri(p, ncri, "Preference threshold")
  validation_env$validate_no_elements_vs_cri(q, ncri, "Indifference threshold")
  validation_env$validate_no_elements_vs_cri(v, ncri, "Veto threshold")
  if (any(q < 0 | q > p | p >= v)) {
    print("Problem with consistency of thresholds:")
    print(paste0("  - Q < 0 for criteria:", cri[q < 0]))
    print(paste0("  - Q > P for criteria:", cri[q > p]))
    print(paste0("  - P >= V for criteria:", cri[p >= v]))
    stop("correct the thresholds please")
  }
}


#' function to check consistency of matrix againts dictionary of values
#'
#' @name validation$validate_fuzzy_consistency
#' @param m matrix to be checked
#' @param dict dictionary to check matrix against
#' @param fn type of fuzzy number (i.e. 3 for triangular, 4 for trapeziodal)
#' @param msg message to guide user on what is being examined i.e. PM,
#'   weight matrix, etc.
validation_env$validate_fuzzy_consistency = function(m, dict, fn = 3, msg = NULL) {
  if (!is.numeric(fn)) {
    stop("Fn param needs to be specified as number of numbers formung the fuzzy
         number (i.e. 3 for triangular or 4 for trapeziodal fuzzy number.)")
  }
  if (ncol(dict) != fn) {
    mes <- paste("dictionary ", msg,
                 " needs to be defined as stringle fuzzy number (", fn,
                 " columns)")
    stop(mes)
  }
  if (!is.numeric(unlist(dict))) {
    stop(paste("Only numeric values expected in dictionary of ", msg))
  }
  for (i in 1:nrow(dict)) {
    validation_env$validate_vector_progression(dict[i, ])
  }
}

#' Validate that the matrix or dataframe has only valid values
#'
#' @description
#' Vector of valid values must contain full set of valid values to check
#'  against. If other value is detected the error will be generated and
#'  computation run will be stoped.
#'
#' @name validation$validate_invalid_val
#' @param m matrix or dataframe to check
#' @param valid_val vector of valid values to check
#' @param msg Identification of what are we checking (i.e. performance matrix)
validation_env$validate_invalid_val <- function(m, valid_val, msg) {
  val1 <- paste(valid_val, collapse = ", ")
  msg2 <- paste(msg, "has some invalid values, expected only: {", val1, "}")
  if (any(!m %in% valid_val)) stop(msg2)
}

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
  l_minmax <- length(minmax)
  if (l_minmax == 1 && (minmax == "min" || minmax == "max")) {
    return(rep(minmax, times = ncri))
  } else if (l_minmax != ncri) {
    stop("No. of criteria in ncri does not correspont to no. of it in minmax.")
  }
  return(minmax)
}

#' Validates that the number of elements in the vector is same as number of
#'  criteria
#'
#' @name validation$validate_no_elements_vs_cri
#' @param vect vector to check
#' @param ncri number of criteria
#' @param msg identification of what are we checking to use in error message
#' @param test_numeric set to TRUE to test vect as numeric vector
validation_env$validate_no_elements_vs_cri <- function(vect, ncri, msg,
                                                       test_numeric = FALSE) {
  if (test_numeric) {
    if (!is.vector(vect, mode = "numeric")) {
      m <- paste(msg, " expected to be numeric vector")
      stop(m)
    }
  } else if (!is.vector(vect)) {
    m <- paste(msg, " expected to be vector")
    stop(m)
  }
  if (!is.numeric(ncri)) {
    stop("number of criteria must be set as number")
  }
  if (length(vect) != ncri) {
    m <- paste("Number of elements in ", msg, " does not correspont to number of
               criteria")
    stop(m)
  }
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

#' Validate consistency of performance matrix specified using fuzzy numbers
#'
#' @name validation$validate_pm_fuzzy
#' @param pm performance matrix
#' @param dict_pm dictionary for performance matrix
#' @param w weight matrix
#' @param dict_w dictionary for weight matrix
#' @param fn type of fuzzy number (i.e. 3 for triangular, 4 for trapeziodal)
#' @param alt vector of alternatives names
validation_env$validate_pm_fuzzy <- function(pm, dict_pm, w, dict_w, fn, alt) {
  n <- ncol(pm) #no. of decision makers
  ncri <- nrow(w)
  validation_env$validate_invalid_val(pm, rownames(dict_pm),
                                      "Performance matrix")
  validation_env$validate_invalid_val(w, rownames(dict_w),
                                      "Weight matrix")
  validation_env$validate_scalar_same(ncol(w), n, "No. of decision makers in performance matrix and weights matrix")
  nalt <- trunc(nrow(pm) / ncri)
  validation_env$validate_scalar_same(nalt, length(alt), "No. of alternatives in alt. vector and performance matrix")
  validation_env$validate_fuzzy_consistency(pm, dict_pm, fn,
                                            "performance matrix")
  validation_env$validate_fuzzy_consistency(w, dict_w, fn,
                                            "weight matrix")
}

#' Validates that to 1 in rows there is symetrical 0 in columns (and vice
#'  versa)
#'
#' @description
#' The preference matrix for binary preference comparison is symetrical if
#'  a is found more important then b (represented by 1) then b must be less
#'  important than a (represented by 0). This function checks that this is the
#'  case
#'
#' @name validation$validate_pm_01_symetry
#' @param pm performance matrix
validation_env$validate_pm_01_symetry <- function(pm) {
  n <- nrow(pm)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if ((pm[i, j] == 1 && pm[j, i] != 0) ||
            (pm[i, j] == 0 && pm[j, i] != 1)) {
        stop("detected inconsistency in stated preferences. If [i,j] == 1 then
             [j,i] == 0 and vice-versa.")
      }
    }
  }
}


#' Validate that the number of rows and columns is same
#'
#' @name validation$validate_pm_rows_columns_same
#' @param pm performance matrix
validation_env$validate_pm_rows_columns_same <- function(pm) {
  if (ncol(pm) != nrow(pm)) {
    stop("number of criteria in rows and colums of preference matrix must be
         same.")
  }
}

#' Validate that scalar is number
#'
#' @name validation$validate_scalar_numeric
#' @param s the value to be checked
#' @param msg identification of what failed to check (i.e. "Discrimination
#'  threshold") must be a number.
validation_env$validate_scalar_numeric <- function(s, msg) {
  if (!is.numeric(s)) {
    m <- paste(msg, " must be a number")
    stop(m)
  }
}

#' Validates that provided params val1 and 2 are same
#'
#' @name validation$validate_scalar_same
#' @param val1 first value to check
#' @param val2 second value to check
#' @param msg identification of what we are checking
validation_env$validate_scalar_same <- function(val1, val2, msg) {
  if (val1 != val2) {
    m <- paste(msg, " expected to be the same")
    stop(m)
  }
}

#' Validates that provided val is in <from;to> interval.
#'
#' @name validation$validate_value_in_interval
#' @param val numeric value to check
#' @param from lower bound of the interval to check
#' @param to upper bound of interval to check
#' @param msg identification of what we are checking
validation_env$validate_value_in_interval <- function(val, from, to, msg) {
  if (!is.numeric(val) || !is.numeric(from) || !is.numeric(to)) {
    m <- paste("Provided ", msg, " and bounds of the interval need to be
               numbers")
    stop(m)
  }
  if (val < from || val > to) {
    m <- paste("Provided ", msg, " is outside of supported interval")
    stop(m)
  }
}

#' Validates tahl all values in the vactor are in specified bounds
#'
#' @name validation$validation_vector_in_interval
#' @param vect numeric vector to check
#' @param from lower bound of the checking interval
#' @param to upper bound for the checking interval
#' @param msg identification of object we are checking (i.e. indifference
#'  threshold)
validation_env$validation_vector_in_interval <- function(vect, from, to, msg) {
  if (!is.numeric(from) || !is.numeric(to)) {
    m <- paste("Provided ", msg, " and bounds of the interval need to be
               numbers")
    stop(m)
  }
  if (!is.vector(vect, mode = "numeric")) {
    m <- paste("For", msg, " numeric vector expected.")
    stop(m)
  }
  if (any(vect < from) || any(vect > to)) {
    m <- paste(msg, " expected to be in interval {", from, "; ", to, "}")
    stop(m)
  }
}

#' Validate the numeric progresion in provided vector
#'
#' @description
#' Presumes that the elements in the vector are ordered ascending. This is
#'  usefull for various threshold checks.
#'
#' @name validation$validate_vector_progression
#' @param vect numeric vector with values to check
validation_env$validate_vector_progression <- function(vect) {
  if (!is.vector(vect, mode = "numeric")) {
    stop("Expected provided parameter to be vector of numbers.")
  }
  if (!all(vect == sort(vect))) {
    stop("Values of the provided vector are not sorted ascending.")
  }
}

#' Validate that vect1 < vect2 (element wise)
#'
#' @description
#' Perform check by comparint elements in vect1 with vect2 to ensure that
#'  every vect1_i < vect2_i, where i are the elements in the vector.
#'
#' This function is a variant of validate_vector_progression() function but it
#'  works with 2 vectors instead of progression of scalars.
#'
#' The function is intended for validation consistency of various shresholds.
#'
#' @name validation$validate_vector_progression2
#' @param vect1 first vector to compare (smaller)
#' @param vect2 second vector to comapare (larger)
#' @param msg string specifying what are we comparing
validation_env$validate_vector_progression2 <- function(vect1, vect2, msg) {
  if (!is.vector(vect1, mode = "numeric") ||
        !is.vector(vect2, mode = "numeric")) {
    m <- paste(msg, " expectes vectors to be numeric")
    stop(m)
  }
  if (!all(vect1 <= vect2)) {
    m <- paste("Not all elements of the ", msg, " are consistent.")
    stop(m)
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