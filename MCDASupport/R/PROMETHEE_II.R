# PROMETHEE IU function
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
#   VERBOSE - TRUE to dump outputs to console
PROMETHEE_II <- function(PM, preferenceFunction, w, minmax = "max",
                         indifferenceTreshold = NULL, prefferenceThreshold = NULL,
                         intermediateThreshold = NULL, VERBOSE = FALSE) {
  #params consistency check centralized in generalized PROMETHEE function
  alt  <- rownames(PM) #list of alternatives
  #here is only minmax
  PM <- util_pm_minmax(PM, minmax) #validate minmax and invert scales if neccessary
  #end of parameter consistency check

  flow <- PROMETHEE(PM, preferenceFunction, w, indifferenceTreshold,
                    prefferenceThreshold, intermediateThreshold)
  pf <- flow$positiveFlow
  nf <- flow$negativeFlow
  nof <- flow$netFlow #net outranking flow (nof)
  names(nof) <- alt
  ordering <- sort(nof, decreasing = TRUE)
  groups <- split(names(ordering), ordering)
  rank <- sapply(groups, function(g) paste(g, collapse = ", "))
  r <- data.frame(rank, as.numeric(names(rank)))
  rownames(r) <- NULL
  colnames(r) <- c("alternative", "netFlow")
  r <- r[order(r$netFlow, decreasing = TRUE), ]
  r$rank <- seq_along(rank)
  #establish preference system
  pref <- outer(nof, nof, function(x, y) as.integer(x > y))
  rownames(pref) <- alt
  colnames(pref) <- alt
  class(pref) <- "prefM"

  if (VERBOSE) {
    plot(pref)
    print("Preference matrix")
    print(pref)
    print("Final order as per net outranking flow")
    print(ordering)
    print("Ranked list")
    print(r)
  }

  out <- list(
    positiveFlow = pf,
    negativeFlow = nf,
    netFlow = nof,
    preferenceDegree = flow$preferenceDegree,
    preferenceDegreeUnw = flow$preferenceDegreeUnw,
    pairweiseComparison = flow$pairweiseComparison,
    rankedList = r,
    rankedListNOF = ordering,
    preferenceMatrix = pref
  )
  return(out)
}
