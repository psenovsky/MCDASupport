#' Weighted Aggregated Sum Product Assessment
#'
#' @description
#' Method developed by Zavadkas as a compromise approach to \link{wsm} and
#'  \link{wpm} methods. The idea is that using both methods together will
#'  minimize negative properties of the methods providing bettor solution to
#'  the decision problem.
#'
#' For example WSM is compensatory method, meaning that worse or even
#'  insufficient performance in one criterium can be compensated by better
#'  performance in other criteria. This may lead to bad solution if nature of
#'  the decision problem is not fully compensatory.
#'
#' The computation is following. First performance matrix is normalized to
#'  maximum for benefit criteria
#'
#' \mjsdeqn{\overline{x_{ij}} = \frac{x_{ij}}{max_i x_{ij}}}
#'
#' and for cost criteria
#'
#' \mjsdeqn{\overline{x_{ij}} = \frac{min_i x_{ij}}{x_{ij}}}
#'
#' Then the performance of alternatives is computed
#'
#' \mjsdeqn{Q_i = \lambda \sum_{j=1}^n \overline{x_{ij}}w_j + (1 - \lambda) \prod_{j = 1}^n x_{ij}^{w_j}}
#'
#' Q value is then directly interpretable for purposes of sorting (max = best).
#'
#' Lambda value in original Zavadkas article was 0.5 - leading to same
#'  proportion of WSM and WPM influence to problem solution.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @references
#' Zavadskas, E.K., Turskis, Z., Antucheviciene, J., Zakarevicius, A. (2012),
#'  Optimization of Weighted Aggregated Sum Product Assessment. Elektronika
#' ir Elektrotechnika (6), 3–6;
#'
#' Zavadskas, E. K., Antucheviciene, J., Saparauskas, J., Turskis, Z.
#'  (2013a), Multi-criteria Assessment of Facades’ Alternatives: Peculiarities
#'  of Ranking Methodology. Procedia Engineering 57, 107–112;
#'
#' @keywords WSM WPM WASPAS
waspas <- R6Class("waspas",
  public = list(
    #' @field pm_orig original percormance matrix (alternatives in rows,
    #'  criteria in columns)
    pm_orig = NULL,

    #' @field pm normalized performance matrix
    pm = NULL,

    #' @field minmax vector of optimization direction for criteria (min/max)
    minmax = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field lambda proportion of the WSM
    lambda = NULL,

    #' @field q performance of alternatives (results of method)
    q = NULL,

    #' @description
    #' validates parameters and computes the results.
    #'
    #' @param pm percormance matrix (alternatives in rows, criteria in columns)
    #' @param w weight vector (sum w = 1)
    #' @param minmax vector of optimization direction for criteria (min/max),
    #'  can be substitued for single min/max if all directions are same
    #' @param lambda proportion of WSM, rest to 1 will be WPM proportion
    #'  (default 0.5)
    #'
    #' @return instance of the WASPAS class
    initialize = function(pm, w, minmax = "max", lambda = 0.5) {
      # check params
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      validation$validate_w_sum_eq_1(w)
      minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_value_in_interval(lambda, 0, 1, "lambda")
      # end check

      self$pm_orig <- pm
      self$minmax <- minmax
      self$w <- w
      self$lambda <- lambda
      self$compute()
      self
    },

    #' @description
    #' Computed the WASPAS model based on params provided in fields of the
    #'  class. Usually it is not required to call this method manualy as it is
    #'  called automatically in the constructor.
    compute = function() {
      # normalize pm
      ncri <- ncol(self$pm_orig)
      pm <- self$pm_orig
      for (i in 1:ncri) {
        if (self$minmax[i] == "max") {
          pm[, i] <- self$pm_orig[, i] / max(self$pm_orig[, i])
        } else {
          pm[, i] <- min(self$pm_orig[, i]) / self$pm_orig[, i]
        }
      }
      self$pm <- pm
      t_wpm <- wpm$new(self$pm, self$w)
      t_wsm <- wsm$new(self$pm, self$w)
      self$q <- self$lambda * t_wsm$result_table$weighted_sum +
        (1 - self$lambda) * t_wpm$p
    },

    #' @description
    #' prepares summary of the WPM method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("WASPAS method:\nComputed ", nalt, " alternatives in ", ncri,
                " criteria\n\nResults:\n"))
      print(self$q)
    }
  )
)