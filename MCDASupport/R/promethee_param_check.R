# Function checks parametrs of PROMETHEE and SIR functions
#
# parameters
#   PM - performance matrix
#   preferenceFunction - vector of preference functions types to derive
#                        preference when comparing PM
#   w - weights
#   indifferenceTreshold list of indifference thresholds
#   prefferenceThreshold list of prefference thresholds
#   intermediateThreshold list of intermediate thresholds for Gaussian
#                         function type
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
  indif_types_vector <- rep(FALSE, times = ncri)
  pref_types <- c("V-shape", "level", "linear")
  pref_types_vector <- rep(FALSE, times = ncri)
  if (!all(preference_function %in% f_types)) {
    stop("preferenceFunction must contain only supported types of functions: 
         default, U-shape, V-shape, level, linear, Gaussian")
  }
  indif_types_vector <- indif_types_vector |
    (preference_function %in% indif_types)
  pref_types_vector <- pref_types_vector | (preference_function %in% pref_types)
  if (TRUE %in% indif_types_vector) {
    # indifference threshold required, check its consistency
    if (!is.vector(qj, mode = "numeric")) {
      stop("indifferenceTreshold must be a numeric vector")
    }
    if (length(qj) != ncri) {
      stop("number of elements in indifferenceTreshold must be equal to number
           of criteria (set value to 0 for criteria with preference function
           that does not require this parameter).")
    }
    if (any(qj < 0)) {
      stop("indif. threshold for criterion only non negative values accepted.")
    }
  }
  if (TRUE %in% pref_types_vector) {
    # prefference threshold required, check its consistency
    if (!is.vector(pj, mode = "numeric")) {
      stop("prefferenceThreshold must be a numeric vector")
    }
    if (length(pj) != ncri) {
      stop("number of elements in prefference threshold must be equal to number
           of criteria (set value to 0 for criteria with preference function
           that does not require this parameter).")
    }
    if (any(pj < 0)) {
      stop("pref. threshold for criterion only non negative values accepted.")
    }
  }
  if ("Gaussian" %in% preference_function) {
    # Gaussian type of prefference function check intermediate threshold
    # for consistency
    if (is.null(sj) && is.null(pj) && is.null(qj)) {
      stop("intermediate threshold required (and not possible to derive from
           indiference and prefference thresholds).")
    }
    if (is.null(sj)) { # try to derive sj from other thresholds
      if (!is.numeric(pj) || !is.numeric(qj) || length(pj) != ncri ||
            length(qj) != ncri) {
        stop("intermediate threshold required (failed to derive it form other
             thresholds due to its inconsistencies)")
      }
      sj <- (pj + qj) / 2
      if (any(sj < 0)) {
        stop("intermediate threshold required (failed to derive it form other
             thresholds due to its inconsistencies)")
      }
    }
  }
  if (!is.numeric(w) || length(w) != ncri) {
    stop("weights not set for all criteria.")
  }

  #clean-up (remove variables used only for consistency check)
  f_types     <- NULL
  indif_types <- NULL
  pref_types  <- NULL
  indif_types_vector <- NULL
  pref_types_vector  <- NULL

  return(sj)
}