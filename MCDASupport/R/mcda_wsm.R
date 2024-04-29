# mcda_wsm
#
# Finds the alternatives that are dominated by others using weighted sum method
#
# Arguments:
#   M - normalized decision matrix with alternatives in rows, criteria 
#       in columns and higher numbers are better
#   w - numeric vector of weights for the criteria
#   minmaxcriteria - either 'min' or 'max' value or a vector of these values
#                    specifying whether the criterium is to be maximized or
#                    minimized
#
# Returns
# weighted performance matrix, summary of weighted values for alternatives
# in raw form and as percentages
mcda_wsm <- function(M, w, minmaxcriteria = "max", VERBOSE = FALSE) {

  #parameter validity check
  ncrit <- ncol(M)
  if (!(is.matrix(M) || (is.data.frame(M)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (ncrit < 2 || nrow(M) < 2) stop("less than 2 criteria or 2 alternatives")
  if (!is.numeric(unlist(M))) {
    stop("Only numeric values in performance matrix expected")
  }
  if (!is.vector(w, mode = "numeric")) {
    stop("weights should be a numeric vector")
  }
  if (length(w) != ncrit) {
    stop("number of weights does not correspond to number of criteria being
         used in preference matrix")
  }
  # end of parameter check

  PM <- util_pm_minmax(M, minmaxcriteria)
  weightedPM <- as.data.frame(sweep(PM, 2, w, "*"))
  weightedSum <- rowSums(weightedPM)
  weightedSumPrc <- weightedSum / (max(weightedSum)/100)
  ResultTable <- weightedPM
  ResultTable$weightedSum <- weightedSum
  ResultTable$weightedSumPrc <- weightedSumPrc
  weightedSumPrc <- sort(weightedSumPrc, decreasing = TRUE)
  scoreM <- plot.scoreM(weightedPM)

  if (VERBOSE) {
    print("performance matrix")
    print(PM)
    print("weighted performance matrix")
    print(weightedPM)
    print("percentages of weighted sums")
    print(weightedSumPrc)
    print("result table")
    print(ResultTable)
    scoreM
  }

  out <- list(
    "performanceMatrix" = PM,
    "weightedPM" = weightedPM,
    "weightedSum" = weightedSum,
    "weightedSumPrc" = weightedSumPrc,
    "resultTable" = ResultTable,
    "scoreM" = scoreM)
  return(out)
}
