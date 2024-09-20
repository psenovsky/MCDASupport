#' SAW (Simple Additive Weight) method
#'
#' @description
#' SAW is a accronym for Simple Additive Weighting method. From
#'  implementation point of wiew it is basically \link{wsm} method, with some
#'  presumptions such as that the sum of weithts is equal to 1 and usage of
#'  specific normalization method.
#'
#' WSM as implemented in the MCDA package presumes the performance matrix has
#'  been normalized, but it is up to the user to do that manually by whatever
#'  method (s-)he chooses. Also weights do not need to be normalized, though
#'  from point of view of the result interpretation it makes sense if it is
#'  normalized.
#'
#' SAW processes decision problem in 3 basix steps:
#'
#' Step 1: prepare initial performance matrix (nust be numeric)
#'
#' We are working with n criteria (i-index) and m alternatives (j-index)
#'  forming matrix R of performance of the altenratives in criteria.
#'
#' We presume that:
#'
#' \mjsdeqn{\sum_{i=1}^n w_i= 1}
#'
#' We can force this requirement by normalizing numeric vector of weights by:
#'
#' \mjsdeqn{w_i' = \frac{w_i}{\sum w_i}}
#'
#' step 2: normalize performance matrix for cost criteria
#'
#' \mjsdeqn{r_{ij}' = \frac{min_j r_{ij}}{r_{ij}}}
#'
#' and for benefit criteria
#'
#' \mjsdeqn{r_{ij}' = \frac{r_{ij}}{max_j r_{ij}}}
#'
#' step 3: integrating values of the criteria and weights
#'
#' \mjsdeqn{S_j = \sum_{i = 1}^n w_i r_{ij}'}
#'
#' Considering the above, the implementation works as special case of
#'  \link{wsm} and extensively uses that function for computation
#'  purposes.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords SAW
saw <- R6Class("saw",
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
    #' @param w vector of weights, summ of weights = 1
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
    #' t <- saw$new(M, w)
    initialize = function(pm, w, minmax = "max") {
      # validity check
      self$pm_orig <- pm
      self$pm <- param_check_wsm(pm, w, minmax)
      if (round(sum(w), 4) != 1) {
        stop("Sum of weights must be equal to 1. If you do not want to use this
             constrain use wsm method instead.")
      }
      # end of validaty check

      self$w <- w
      self$minmax <- minmax
      self$compute()
      self
    },

    #' @description
    #' performs computation of SAW model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      t <- wsm$new(self$pm_orig, self$w, self$minmax)
      self$result_table <- t$result_table
      self$weighted_sum_prc <- t$weighted_sum_prc
      self$scoreM <- t$scoreM
    },

    #' @description
    #' prepares summary of the SAW method resutls and outputs them
    #'  to the console.
    summary = function() {
      cat(paste("SAW method results:\n"))
      print(self$result_table, pretty = TRUE)
      cat(paste("\nClosenes to best alternative as % of best\n"))
      print(self$weighted_sum_prc, pretty = TRUE)
      print(self$scoreM)
    }
  )
)