# implementation of PROMETHEE III function. Function provides preordering
# by intervals.
#
# parameters
#   PM - performance matrix
#   preferenceFunction - vector of preference functions types to derive
#                        preference when comparing PM
#   w - weights
#   minmax - min/max value or vector of mixed values for criteria orientation
#   indifferenceTreshold list of indifference thresholds
#   prefferenceThreshold list of prefference thresholds
#   intermediateThreshold list of intermediate thresholds for Gaussian
#                         function type
PROMETHEE_III <- function(PM, preferenceFunction, w, minmax = "max",
                          indifferenceTreshold = NULL, prefferenceThreshold = NULL,
                          intermediateThreshold = NULL) {
  #params consistency check centralized in generalized PROMETHEE function
  nalt <- nrow(PM)  #no. of alternatives
  alt  <- rownames(PM) #list of alternatives
  #here is only minmax evaluation
  PM <- util_pm_minmax(PM, minmax) #validate minmax and invert scales if neccessary
  #end of parameter consistency check

  flow <- PROMETHEE(PM, preferenceFunction, w, indifferenceTreshold,
                    prefferenceThreshold, intermediateThreshold)
  pf <- flow$positiveFlow
  nf <- flow$negativeFlow
  nof <- flow$netFlow
  names(nof) <- alt

  stand_error <- round((sd(nof) / sqrt(nalt)), 3)
  x_limit <- round(nof - stand_error, 3)
  y_limit <- round(nof + stand_error, 3)

  #establish preference system
  pref <- matrix(data = "-", nrow = nalt, ncol = nalt)
  P_plus <- outer(x_limit, y_limit, ">")
  I <- outer(x_limit, y_limit, "<=") & outer(y_limit, x_limit, ">=")
  P_minus <- !(P_plus | I)
  diag(P_plus) <- diag(P_minus) <- diag(I) <- FALSE
  pref[P_plus] <- "P+"
  pref[P_minus] <- "P-"
  pref[I] <- "I"
  rownames(pref) <- alt
  colnames(pref) <- alt

  out <- list(
    positiveFlow = pf,
    negativeFlow = nf,
    netFlow = nof,
    preferenceDegree = flow$preferenceDegree,
    preferenceDegreeUnw = flow$preferenceDegreeUnw,
    pairweiseComparison = flow$pairweiseComparison,
    preferenceMatrix = pref
  )
  return(out)
}
