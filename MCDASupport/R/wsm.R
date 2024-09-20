#' WSM (Weighted Sum Method)
#'
#' @description
#' Weighted sum method (WSM) is one of simpliest and at the same time one of
#'  most used methods for evaluation of alternatives using different criteria.
#'  For the method to work it is neccessary to prepare performance matrix with
#'  alternatives in the rows and criteria in the columns.
#'
#' The decision problem is expressed by means of numeric values characterizing
#'  various alternatives in criteria relevant for decision making. The method
#'  expects the values to be normalized.
#'
#' The methods takes normalized performance matrix, applies weights for the
#'  criteria on it and sums the results accros the criteria to get score for
#'  the alternatives. Such score is usable to rank the alternatives. Score
#'  is provided in both raw (just sum of values) and percentage forms.
#'
#' @references
#' Šenovský, P: "Modeling of Decision Processes (in czech)", 4th edition, VŠB
#'  - Technical University of Ostrava, 2012, 113 p.
#'
#' @keywords WSM MCDA Weighted Sum Method
wsm <- R6Class("wsm",
  public = list(

    #' @field pm_orig original (not normalized) performance matrix
    pm_orig = NULL,

    #' @field pm normalized performance matrix
    pm = NULL,

    #' @field w vector of weights (there are no other limitations on weights)
    w = NULL,

    #' @field minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    minmax = NULL,

    #' @field result_table has weighted performance matrix with added columns
    #'  to summ performance and extress this sum as a percentage of the best
    #'  alternative
    result_table = NULL,

    #' @field weighted_sum_prc vector specifying how close the alternatives are
    #'  to the best aleternative (expresed as the percentage of best) sorted
    #'  descending (from best to worst)
    weighted_sum_prc = NULL,

    #' @field scoreM results of the WSM presented as stacked bar chart. The
    #'  graph clearly shows contribution of the criteria to overall performance
    #'  of the alternative.
    scoreM = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm normalized performance matrix
    #' @param w vector of weights
    #' @param minmax minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    #'
    #' @examples
    #' alternatives <- c('BLR', 'BOM', 'DEL', 'MNL', 'HYD', 'GRU', 'DUB',
    #'   'KRK', 'MAA', 'EZE')
    #' criteria <- c('tlnt', 'stab', 'cost', 'infl', 'tm-zn', 'infr', "life")
    #' M <- rbind(
    #'   c(0.8181818, 0.1814159, 1.0000000, 0.1198582, 0, 0.6, 0.750),
    #'   c(1.0000000, 0.1814159, 0.6666667, 0.1198582, 0, 0.6, 0.375),
    #'   c(1.0000000, 0.1814159, 0.8333333, 0.1198582, 0, 0.6, 0.125),
    #'   c(0.8181818, 0.0000000, 1.0000000, 0.3482143, 0, 0.6, 0.375),
    #'   c(0.1818182, 0.1814159, 1.0000000, 0.1198582, 0, 0.2, 0.375),
    #'   c(0.1818182, 0.1814159, 0.5000000, 0.1198582, 0, 0.2, 0.125),
    #'   c(0.0000000, 1.0000000, 0.0000000, 0.5741667, 1, 1.0, 1.000),
    #'   c(0.3636364, 0.7787611, 0.6666667, 1.0000000, 1, 0.0, 0.500),
    #'   c(0.4545455, 0.1814159, 0.9166667, 0.1198582, 0, 0.4, 0.000),
    #'   c(0.1818182, 0.6283186, 0.5833333, 0.0000000, 0, 0.4, 0.125)
    #' )
    #' rownames(M) <- alternatives
    #' colnames(M) <- criteria
    #' w = c(0.125, 0.2, 0.2, 0.2, 0.175, 0.05, 0.05)
    #' t <- wsm$new(M, w)
    initialize = function(pm, w, minmax = "max") {
      self$pm_orig <- pm
      self$pm <- param_check_wsm(pm, w, minmax)
      self$w <- w
      self$minmax <- minmax
      self$compute()
      self
    },

    #' @description
    #' performs computation of WSM model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      weighted_pm <- as.data.frame(sweep(self$pm, 2, self$w, "*"))
      weighted_sum <- rowSums(weighted_pm)
      weighted_sum_prc <- weighted_sum / (max(weighted_sum) / 100)
      result_table <- weighted_pm
      result_table$weighted_sum <- weighted_sum
      result_table$weighted_sum_prc <- weighted_sum_prc
      weighted_sum_prc <- sort(weighted_sum_prc, decreasing = TRUE)
      scoreM <- plot.scoreM(weighted_pm)

      # save results
      self$result_table <- result_table
      self$weighted_sum_prc <- weighted_sum_prc
      self$scoreM <- scoreM
    },

    #' @description
    #' prepares summary of the WSM method resutls and outputs them
    #'  to the console.
    summary = function() {
      cat(paste("WSM method results:\n"))
      print(self$result_table, pretty = TRUE)
      cat(paste("\nClosenes to best alternative as % of best\n"))
      print(self$weighted_sum_prc, pretty = TRUE)
      print(self$scoreM)
    }
  )
)