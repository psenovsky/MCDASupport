#' Combined Compromise solution
#'
#' #@description
#' MCDA analysis method based on finding of compomise solution for \link{wsm}
#'  and \link{wpm} models. The compromise is found in following manner.
#'
#' Analysis starts with formulation of performance matrix of alternatives
#'  (rows) in criteria (columns). Values of performance matrix are then
#'  normalized using min-max method.
#'
#' In step 3 and 4 of analysis weighted sum (Si) and weighted products (Pi) are
#'  computed.
#'
#' \mjsdeqn{S_i = \sum_{j=1}^n w_j r_{ij}}
#'
#' \mjsdeqn{P_i = \sum_{j=1}^n r_{ij}^{w_j}}
#'
#' Method proposes three measures for establishing ranking order called ki a)
#'  to c). Each measure can be used separately to establish ranking of the
#'  alternatives.
#'
#' \mjsdeqn{k_{ia} = \frac{P_i + S_i}{\sum_{i=1}^m (P_i + S_i)}}
#'
#' \mjsdeqn{k_{ib} = \frac{S_i}{min_i S_i} + \frac{P_i}{min_i P_i}}
#'
#' \mjsdeqn{k_{ic} = \frac{\lambda S_i + (1 - \lambda) P_i}{\lambda max_i S_i + (1 - \lambda) max_i P_i}}
#'
#' with
#'
#' \mjsdeqn{0 \le \lambda \le 1}
#'
#' Lambda in measure c serves as a weight for summary and product part of the
#'  measure. When lambda = 1, then these two componets will have same impact on
#'  result.
#'
#' Lastly final ranking is established
#'
#' \mjsdeqn{k_i = (k_{ia} k_{ib} k_{ic})^{1/3} + (k_{ia} k_{ib} k_{ic}) / 3}
#'
#' @references
#' A Combined Compromise Solution. 2022. Available from
#'  \url{https://www.youtube.com/watch?v=izbd0h0-fSs}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords CoCoSo WSM WPM
cocoso <- R6Class("cocoso",
  public = list(
    #' @field pm_orig original (unnormalized) performance matrix of
    #'  alternatives (in rows) in criteria (columns)
    pm_orig = NULL,

    #' @field pm normalized performance matrix
    pm = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria.
    minmax = NULL,

    #' @field lambda a constant in interval <0;1> to establish influence of
    #'  weighted sum and weighted product on the result
    lambda = NULL,

    #' @field results provides dataframe with ki a to c partial rankings and ki
    #'  representing final (compromise) solution. (largest value best)
    results = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix (alternatives in rows, criteria in columns).
    #' @param w weight vector (sum of weights = 1)
    #' @param minmax vector of optimization direction for criteria (min/max).
    #'  Can use single min/max if optimization direction of all criteria is
    #'  same.
    #' @param lambda a constant in interval <0;1> to establish influence of
    #'  weighted sum and weighted product on the result. Implicitly set to 0.5
    #'  to represent equalt influence of both components
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' # from: https://www.youtube.com/watch?v=izbd0h0-fSs
    #' alternatives <- c("AI 2024-T6", "AI 5052-O", "SS 301 FH", "SS 310-3AH",
    #'  "Ti-6AI-4V", "Inconel 718", "70Cu-30Zn")
    #' criteria <- c("Toughness index", "Yield Strength", "Young's Modulus",
    #'  "Density", "Thermal expansion", "Thermal conductivity", "Specific Heat")
    #' pm <- cbind(
    #'  c(75.5, 95, 770, 187, 179, 239, 273),
    #'  c(420, 91, 1365, 1120, 875, 1190, 200),
    #'  c(74.2, 70, 189, 210, 112, 217, 112),
    #'  c(2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53),
    #'  c(21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9),
    #'  c(0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29),
    #'  c(0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06)
    #' )
    #' rownames(pm) <- alternatives
    #' colnames(pm) <- criteria
    #' w <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
    #' minmax <- c("max", "max", "max", "min", "min", "min", "min")
    #' t <- cocoso$new(pm, w, minmax)
    initialize = function(pm, w, minmax = "max", lambda = 0.5) {
      # validation of the parameters
      ncri <- ncol(pm)
      self$pm_orig <- pm
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_w_sum_eq_1(w)
      validation$validate_value_in_interval(lambda, 0, 1, "lambda")
      # end of validation

      self$w <- w
      self$lambda <- lambda
      self$compute()
      self
    },

    #' @description
    #' computes CoCoSo model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm_orig)
      self$pm <- self$pm_orig
      for (i in 1:ncri) {
        self$pm[, i] <- mcda_norm(self$pm_orig[, i], self$minmax[i],
                                  "minmax")
      }
      si <- rowSums(sweep(self$pm, 2, self$w, "*"))
      w_pm <- self$pm
      for (i in 1:ncri) {
        w_pm[, i] <- self$pm[, i]^self$w[i]
      }
      p_i <- rowSums(w_pm)
      min_si <- min(si)
      min_pi <- min(p_i)
      max_si <- max(si)
      max_pi <- max(p_i)
      kia <- (p_i + si) / sum(p_i + si)
      kib <- si / min_si + p_i / min_pi
      kic <- (self$lambda * si + (1 - self$lambda) * p_i) /
        (self$lambda * max_si + (1 - self$lambda) * max_pi)
      ki <- (kia * kib * kic)^(1 / 3) + (kia + kib + kic) / 3

      self$results <- data.frame(kia, kib, kic, ki)
    },

    #' @description
    #' prepares summary of the CoCoSo method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("CoCoSo results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n"))
      print(self$results, pretty = TRUE)
    }
  )
)