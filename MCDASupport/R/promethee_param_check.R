#' internal function to check parameters for PROMETHEE and SIR functions
#'
#' @description
#' Performs parameter checking for \link{PROMETHEE} and \link{sir} functions.
#'  It especially focuses on preference, indifference and intermediate
#'  thresholds as these various preference functions deal with these in
#'  different way.
#'
#' Function tries to derive intermediate threshold for Gausian preference
#'  function (if it is missing) from prefererence and indifference thresholds.
#'
#' Function then returns ammended vector of intermediate thresholds.
#'
#' This function is intended for package's internal use only.
#'
#' @param pm Matrix or data frame containing the performance table. Each row
#'  corresponds to an alternative, and each column to a criterion. only numeric
#'  values expercted. Rows and columns are expected to be named.
#' @param preference_function vector, specifies type of function used to
#'  compute preferences. Need to be set for each criterion. Possible values
#'  are: 'default', 'U-shape', 'V-shape', 'level', 'linear', 'Gaussian'. Choice
#'  of function type will decide on what type of threshold (if any) is required
#'  for computation. Each criterion can use different preference function.
#' @param w vector containing the weights of the criteria. Values need to
#'  0 <= wi <= 1, sum(wi) = 1
#' @param indifference_treshold vector containing indifference threshods for
#'  criteria. Not all types of performance functions require it. The parameter
#'  must be used if there is at least one criterion, for which it is required.
#'  Values for all other criteria should be 0 (and will not be used during
#'  computations). Only 'U-shape', 'level', 'linear' functions need this
#'  type of threshold.
#' @param prefference_threshold vector containing prefference threshods for
#'  criteria. Not all types of performance functions require it. The parameter
#'  must be used if there is at least one criterion, for which it is required.
#'  Values for all other criteria should be 0 (and will not be used during
#'  computations). Only 'V-shape', 'level', 'linear' functions need this
#'  threshold.
#' @param intermediate_threshold vector containing intermetiate thresholds for
#'  criteria. only Gaussian type performance functions rewuire this type of
#'  threshold. If prefference and indifference thresholds are present, the
#'  PROMETHEE function will try to 'gues' intermediate threshold as value right
#'  in the middle between these thresholds.
#'
#' @return The function returns recomputed value of intermediate threshold if
#'  no error in parameter checking logic is detected.
#'
#' @references
#' ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca
#'  Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords PROMETHEE SIR
promethee_param_check <- function(pm, preference_function, w,
                                  indifference_treshold = NULL,
                                  prefference_threshold = NULL,
                                  intermediate_threshold = NULL) {
  ## check validity of the objects manipulated by the current function
  # with < 2 criteria or 2 alternatives, there is no MCDA problem
  if (is.null(dim(pm))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(pm) || (is.data.frame(pm)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (!is.numeric(unlist(pm))) {
    stop("Only numeric values in performance matrix expected")
  }
  ncri <- ncol(pm)  #no. of criteria
  qj   <- indifference_treshold
  pj   <- prefference_threshold
  sj   <- intermediate_threshold
  f_types <- c("default", "U-shape", "V-shape", "level", "linear", "Gaussian")
  indif_types <- c("U-shape", "level", "linear")
  pref_types <- c("V-shape", "level", "linear")
  if (!all(preference_function %in% f_types)) { #fail unknown preference func.
    stop("preferenceFunction must contain only supported types of functions: 
         default, U-shape, V-shape, level, linear, Gaussian")
  }
  if (!is.numeric(w) || length(w) != ncri) {
    stop("weights either non-numeric or not set for all the criteria.")
  }
  if (any(w) < 0) {
    stop("weight vector can contain only positive numbers.")
  }
  if (round(sum(w), 4) != 1) {
    stop("sum of weights must be equal to 1")
  }
  # check validity of the indifference and prefference thresholds
  for (i in 1:ncri) {
    # indifference threshold
    if (preference_function[i] %in% indif_types &&
          (!is.numeric(qj[i]) || qj[i] < 0)) {
      msg <- paste0("Nonnumeric value found in criterium ", i, ". ",
                    preference_function[i], " must have set numeric  positive
                     indifference threshold")
      stop(msg)
    }
    # preference threshold
    if (prefference_threshold[i] %in% pref_types &&
          (!is.numeric(pj[i]) || pj[i] < 0)) {
      msg <- paste0("Nonnumeric value found in criterium ", i, ". ",
                    preference_function[i], " must have set numeric positive
                     prefference threshold")
    }
    # indifference threshold vs preference threshold
    if (preference_function[i] %in% c("linear", "level") && qj[i] > pj[i]) {
      msg <- paste0("For level/linear preference function indifference
                     threshold must be lower then prefference threshold.
                      Problem detectied in criterium ", i, ".")
      stop(msg)
    }

    if (preference_function[i] == "Gaussian") {
      # Gaussian type of prefference function check intermediate threshold
      # for consistency
      if (is.null(sj[i]) || !is.numeric(sj[i]) || sj[i] < 0) {
        # itermediate threshold is missing, we try to derive it from preference
        # and indifference thresholds
        if (is.null(pj[i]) || !is.numeric(pj[i]) || is.null(qj[i]) ||
              !is.numeric(qj[i]) || pj[i] < 0 || qj[i] < 0) {
          #fail nothing to derive it from
          msg <- paste0("Criterium ", i, " is Gaussian, but the it has not ",
                        " set intermediate threshold for it and this ",
                        " threshold is not deriveable from preference and ",
                        "indefference thresholds, as you did not provide them.")
          stop(msg)
        }
      } else if (qj[i] > pj[i]) { #fail thresholds are faulty
        msg <- paste0("Failed to derive intermediate threshold for criterium ",
                      i, " from preference and indifference thresholds as the",
                      " indifference threshold is larger then preference ",
                      "threshold.")
        stop(msg)
      } else { #succ - derive it
        sj[i] <- (pj[i] + qj[i]) / 2
      }
    }
  }
  #clean-up (remove variables used only for consistency check)
  f_types     <- NULL
  indif_types <- NULL
  pref_types  <- NULL

  return(sj)
}