#' Multi-Objective Optimization on the basis of Ratio Analysis
#'
#' @description
#' Another method in family of \ling{wsm}-based methods.
#'
#' This time around the method uses vector normalization for benefit criteria.
#'  This form of normalization is used on both benefit and cost criteria.
#'
#' \mjsdeqn{\overline{x_{ij}} = \frac{x_{ij}}{\sqrt{\sum_{j=1}^m x_{ij}^2}}}
#'
#' Then it computes weighted assessment of the alternatives by computing
#'  difference between g weighted benefit criteria and n cost criteria.
#'
#' \mjsdeqn{y_j = \sum_{i=1}^g \overline{x_{ij}} - \sum_{i = 1}^n \overline{x_{ij}}}
#'
#' @references
#' Brauers, W.K.M., Zavadskas, E.K. The MOORA method and its application to
#'  privatization in a transition economy. Control and Cybernetics. Vol. 32
#'  (2006), no. 2, 445-469
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords WSM MOORA
moora <- R6Class("moora",
  public = list(
    #' @field pm_orig original (not normalized) performance matrix
    pm_orig = NULL,

    #' @field pm normalized performance matrix
    pm = NULL,

    #' @field w vector of weights (sum of weights = 1)
    w = NULL,

    #' @field y normalized assessment of alternatives with respect to all
    #'  objectives
    y = NULL,

    #' @field y_sorted sorted normalized assessment of alternatives with
    #'  respect to all objectives from best to worst.
    y_sorted = NULL,

    #' @field minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    minmax = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm performance matrix (alternatives in rows and criteria in
    #'  columns)
    #' @param w vector of weights, its sum must be equal to 1
    #' @param minmax minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    #'
    #' @examples
    #' # from: https://www.youtube.com/watch?v=wQD8df_jVF0
    #' alternatives <- c("car 1", "car 2", "car 3", "car 4", "car 5", "car 6",
    #'  "car 7", "car 8", "car 9", "car 10")
    #' criteria <- c("quality", "condition", "security", "delivery days",
    #'  "fuel consumption", "price")
    #' M <- rbind(
    #'   c(3, 6, 4, 20, 2, 30000),
    #'   c(4, 4, 6, 15, 2.2, 32000),
    #'   c(6, 5, 9, 18, 3, 32100),
    #'   c(5, 6, 3, 23, 2.8, 28000),
    #'   c(4, 8, 7, 30, 1.5, 29000),
    #'   c(8, 3, 6, 35, 1.9, 27000),
    #'   c(7, 2, 5, 33, 1.7, 28500),
    #'   c(3, 8, 3, 34, 1.6, 30500),
    #'   c(8, 4, 8, 40, 2.5, 33000),
    #'   c(9, 3, 7, 34, 2, 29800)
    #' )
    #' rownames(M) <- alternatives
    #' colnames(M) <- criteria
    #' w = c(0.1, 0.2, 0.1, 0.2, 0.1, 0.3)
    #' minmax <- c("max", "max", "max", "min", "min", "min")
    #' t <- moora$new(M, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      # validate parameters
      ncri <- ncol(pm)
      self$pm_orig <- pm
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      validation$validate_w_sum_eq_1(w)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of validation

      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' performs computation of MOORA model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      # normalization
      ncri <- ncol(self$pm_orig)
      self$pm <- self$pm_orig
      for (i in 1:ncri) {
        self$pm[, i] <- mcda_norm(self$pm_orig[, i], "max", "vector")
      }
      weighted_pm <- as.data.frame(sweep(self$pm, 2, self$w, "*"))
      min_indices <- which(self$minmax == "min")
      max_indices <- which(self$minmax == "max")
      min_sums <- rowSums(weighted_pm[, min_indices])
      max_sums <- rowSums(weighted_pm[, max_indices])
      self$y <- max_sums - min_sums
      self$y_sorted <- sort(self$y, decreasing = TRUE)
    },

    #' @description
    #' prepares summary of the MOORA method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("MOORA method results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n"))
      print(self$y_sorted, pretty = TRUE)
    }
  )

)