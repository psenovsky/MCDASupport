#' Multi-Objective Optimization on the basis of Simple Ratio Analysis
#'
#' @description
#' The method is very similar to \link{moora}. It uses same approach to
#'  normalization
#'
#' \mjsdeqn{\overline{x_{ij}} = \frac{x_{ij}}{\sqrt{\sum_{j=1}^m x_{ij}^2}}}
#'
#' Again all criteria regardless of whether beneficial or cost are normalized
#'  using same equation.
#'
#' MOORA solved this by computing difference betweeen sums of beneficial and
#'  cost criteria. MOOSRA on the other hand computes the ratios of these.
#'
#' \mjsdeqn{v_i = \frac{\sum_{j in bc} w_j r_{ij}}{\sum_{j in cc} w_j r_{ij}}}
#'
#' Where bc are benefit and cc are cost criteria.
#'
#' @references
#' ULUTAŞ, Alptekin et al. Developing of a Novel Integrated MCDM MULTIMOOSRAL
#'  Approach for Supplier Selection. Informatica. 2021, vol. 32, no. 1,
#'  pp. 145–161, available from: https://doi.org/10.15388/21-INFOR445,
#'  ISSN 0868-4952, 1822-8844.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords WSM MOORA MOOSRA
moosra <- R6Class("moosra",
  public = list(
    #' @field pm_orig original (not normalized) performance matrix
    pm_orig = NULL,

    #' @field pm normalized performance matrix
    pm = NULL,

    #' @field w vector of weights (sum of weights = 1)
    w = NULL,

    #' @field v normalized assessment of alternatives with respect to all
    #'  objectives
    v = NULL,

    #' @field v_sorted sorted normalized assessment of alternatives with
    #'  respect to all objectives from best to worst.
    v_sorted = NULL,

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
    #' t <- moosra$new(M, w, minmax)
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
    #' performs computation of MOOSRA model based on class properties. Usually
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
      self$v <- max_sums / min_sums
      self$v_sorted <- sort(self$v, decreasing = TRUE)
    },

    #' @description
    #' prepares summary of the MOORA method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("MOOSRA method results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n"))
      print(self$v_sorted, pretty = TRUE)
    }
  )
)