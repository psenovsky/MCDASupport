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

  pm <- util_pm_minmax(M, minmaxcriteria)
  weighted_pm <- as.data.frame(sweep(pm, 2, w, "*"))
  weighted_sum <- rowSums(weighted_pm)
  weighted_sum_prc <- weighted_sum / (max(weighted_sum) / 100)
  result_table <- weighted_pm
  result_table$weighted_sum <- weighted_sum
  result_table$weighted_sum_prc <- weighted_sum_prc
  weighted_sum_prc <- sort(weighted_sum_prc, decreasing = TRUE)
  scoreM <- plot.scoreM(weighted_pm)

  if (VERBOSE) {
    print("performance matrix")
    print(pm)
    print("weighted performance matrix")
    print(weighted_pm)
    print("percentages of weighted sums")
    print(weighted_sum_prc)
    print("result table")
    print(result_table)
    scoreM
  }

  out <- list("performanceMatrix" = pm,
              "weightedPM" = weighted_pm,
              "weightedSum" = weighted_sum,
              "weightedSumPrc" = weighted_sum_prc,
              "resultTable" = result_table,
              "scoreM" = scoreM)
  return(out)
}
