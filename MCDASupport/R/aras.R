#' Additive Ratio Assessment (ARAS) method for multiple criteria decision
#'  making
#'
#' @description
#' Another method from \link{wsm} family of methods. In its requirements it is
#'  perhaps little bit closer to \link{saw} as sum of provided weights needs to
#'  be equal to 1.
#'
#' As all methods in this family of methods, it requires performance matrix to
#'  be normalized.
#'
#' ARAS differs from other methods in the family in optimality estimation step,
#'  in which it computes ration between utility of the alternative and utility
#'  of the ideal variant.
#'
#' The computation process:
#'
#' step 1) Formulate DMM (Decision Making Matrix)
#'
#' Package usually uses different name for it PM (Performance Matrix). The idea
#'  is same, we work with m alternatives in n criteria describing  performance
#'  of these alternatives.
#'
#' Performance of i-th alternative in criterium j is denoted as \mjseqn{x_{ij}}
#'  and \mjseqn{x_{0j}} represents optimal value of j-th criterium.
#'
#' Generally speaking if optimal value of j-th criterium is not known then for
#'  benefit criteria we can compute it as:
#'
#' \mjsdeqn{x_{0j} = max_i x_{ij}}
#'
#' and for cost criteria:
#'
#' \mjsdeqn{x_{0j} = min_i x_{ij}}
#'
#' Actual implementation in this package is slightly different as it transforms
#'  scales of cost criteria to benefit scale, simplifying the computation
#'  process.
#'
#' step 2) Normalize DMM
#'
#' for benefit criteria
#'
#' \mjsdeqn{x_{ij}' = \frac{x_{ij}}{\sum_{i=1}^m x_{ij}}}
#'
#' step 3) normalized weight martix
#'
#' For weights followin constraint must hold:
#'
#' \mjsdeqn{\sum_{j=1}^n w_j = 1}
#'
#' then we can compute weighted performance as
#'
#' \mjsdeqn{\hat{x}_{ij} = x_{ij}' w_j}
#'
#' step 4) optimality estimation
#'
#' \mjsdeqn{S_i = \sum_{j=1}^n \hat{x}_{ij}}
#'
#' Alternatives with larger \mjseqn{S_i} are considered better then those with
#'  lower value of it.
#'
#' Up to this point, the procedure is basically same as for \link{wsm} or
#'  \link{saw}. ARAS adds comparison of the ratio between value of \mjseqn{S_i}
#'  and ideal variant \mjseqn{S_0} as
#'
#' \mjsdeqn{K_i = \frac{S_i}{S_0}}
#'
#' It can be argued that the ranking of alternatives based on sum \mjseqn{S_i}
#'  and value of ratio \mjseqn{K_i} will be same in both cases. So the
#'  computation of \mjseqn{K_i} allows us to see the optimality through optics
#'  of distance to ideal variant.
#'
#' @references
#' Zavadskas, E. K., Turskis, Z.: A New Additive Ratio Assessment (ARAS) method
#'  in multicriteria decision-making. Technological and Economic Deverlopment
#'  of Econony, 16(2), pp. 159-172, DOI:
#'  \url{https://dx.doi.org/10.3846/tede.2010.10}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords WSM ARAS SAW
aras <- R6Class("aras",
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

    #' @field assessment_ratio is ration between overal performance of the
    #'  alternative to the performance of best alternative
    assessment_ratio = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm normalized performance matrix
    #' @param w vector of weights, its sum must be equal to 1
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
      # validity check
      ncri <- ncol(pm)
      self$pm_orig <- pm
      validation$validate_pm(pm)
      validation$validate_w(w, ncri)
      validation$validate_w_sum_eq_1(w)
      validation$validate_minmax(minmax, ncri)
      self$pm <- util_pm_minmax(pm, minmax)
      # end of validaty check

      self$w <- w
      self$minmax <- minmax
      self$compute()
      self
    },

    #' @description
    #' performs computation of ARAS model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      t <- wsm$new(self$pm_orig, self$w, self$minmax)
      t$result_table$assesment_ratio <-
        t$result_table$weighted_sum / max(t$result_table$weighted_sum)
      ar <- t$result_table$assesment_ratio
      names(ar) <- rownames(t$result_table)

      self$result_table <- t$result_table
      self$weighted_sum_prc <- t$weighted_sum_prc
      self$assessment_ratio <- ar
      self$scoreM <- t$scoreM
    },

    #' @description
    #' prepares summary of the SAW method resutls and outputs them
    #'  to the console.
    summary = function() {
      cat(paste("ARAS method results:\n"))
      print(self$result_table, pretty = TRUE)
      cat(paste("\nAssessment ration (ration of sum of alternative to best
                alternative\n"))
      print(self$assessment_ratio, pretty = TRUE)
      print(self$scoreM)
    }
  )
)