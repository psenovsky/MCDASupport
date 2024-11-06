#' Simple weight sum weighting method
#'
#' @description
#' Simple methos for alternatives rank establishment considering weighted sum
#'  producs.
#'
#' The inputs of method are formulated as usual for this type of decision
#'  problem by creading matrix of alternatives (in rows) performance in
#'  selected criteria (columns).
#'
#' The values of the performance need to be normalized using:
#'
#' \mjsdeqn{r_{ij} = \frac{x_{ij}}{max_i x_{ij}}}
#'
#' The normalization is performed using same equation for both benefit and
#'  cost criteria as the method is based on analyzing the difference of these.
#'
#' Then we need to estables four utility measures:
#'
#' 1) weighted sum
#'
#' \mjsdeqn{u_i^{wsd} = \sum_{j \in \Omega_{max}}r_{ij}w_j - \sum_{j \in \Omega_{min}} r_{ij}w_j}
#'
#' 2) weighted product
#'
#' \mjsdeqn{u_i^{wpd} = \prod_{j \in \Omega_{max}} r_{ij}w_j - \prod_{j \in \Omega_{min}} r_{ij}w_j}
#'
#' 3) weighted sum ratio
#'
#' \mjsdeqn{u_i^{wsr} = \frac{\sum_{j \in \Omega_{max}}r_{ij}w_j}{\sum_{j \in \Omega_{min}} r_{ij}w_j}}
#'
#' 4) weighted product ratio
#'
#' \mjsdeqn{u_i^{wpr} = \frac{\prod_{j \in \Omega_{max}} r_{ij}w_j}{\prod_{j \in \Omega_{min}} r_{ij}w_j}}
#'
#' From these values four utility measures need to be derived. For weighted sum
#'
#' \mjsdeqn{\overline{u_i^{wsd}} = \frac{1 + u_i^{wsd}}{1 + max_i u_i^{wsd}}}
#'
#' weighted products
#'
#' \mjsdeqn{\overline{u_i^{wpd}} = \frac{1 + u_i^{wpd}}{1 + max_i u_i^{wpd}}}
#'
#' weighted sum ratio
#'
#' \mjsdeqn{\overline{u_i^{wsr}} = \frac{1 + u_i^{wsr}}{1 + max_i u_i^{wsr}}}
#'
#' weighted product ratio
#'
#' \mjsdeqn{\overline{u_i^{wpr}} = \frac{1 + u_i^{wpr}}{1 + max_i u_i^{wpr}}}
#'
#' Overall metric for performance is derived from these four measures by
#'  averaging them.
#'
#' \mjsdeqn{u_i = (\overline{u_i^{wsd}} + \overline{u_i^{wpd}} + \overline{u_i^{wsr}} + \overline{u_i^{wpr}}) / 4}
#'
#' Using this metric we can sort the alternatives from best to worst.
#'
#' The class also supports simple wariant of WIPS (S-WIPS), which simplifies
#'  the model by using only wsd and wpr utility measures.
#'
#' @references
#' Stanujkic, D., Popovic, G., Karabasevic, D., Meidute-Kavaliauskiene, I., &
#'  Ulutaş, A. (2021). An integrated simple weighted sum product method—WISP.
#'  IEEE Transactions on Engineering Management.
#'
#' Ulutaş, A., Stanujkic, D., Karabasevic, D., Popovic, G., & Novaković, S.
#'  (2022). Pallet truck selection with MEREC and WISP-S methods. Strategic
#'  Management-International Journal of Strategic Management and Decision
#'  Support Systems in Strategic Management.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords WSM WISP
wisp <- R6Class("wisp",
  public = list(
    #' @field pm_orig original (not normalized) performance matrix.
    #'  Alternatives in rows and criteria in columns
    pm_orig = NULL,

    #' @field pm normalized performance matrix
    pm = NULL,

    #' @field w weight vector (sum of weights = 1)
    w = NULL,

    #' @field u overal utility of the alternatives
    u = NULL,

    #' @field us overal utility of the alternatives for S-WIPS
    us = NULL,

    #' @field u_sorted overal sorted utility of the alternatives
    u_sorted = NULL,

    #' @field us_sorted overal sorted utility of the alternatives for S-WIPS
    us_sorted = NULL,

    #' @field minmax vector of optimization directions (min/max). Can be
    #'  single min or max value  if all optimization directions are same.
    minmax = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix (alternatives in rows, criteria in columns).
    #' @param w weight vector (sum of weights = 1)
    #' @param minmax vector of optimization direction for criteria (min/max).
    #'  Can use single min/max if optimization direction of all criteria is
    #'  same.
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' # from: https://www.youtube.com/watch?v=gfnZNUsWBeg
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
    #' t <- wisp$new(pm, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      # validation of the parameters
      ncri <- ncol(pm)
      self$pm_orig <- pm
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_w_sum_eq_1(w)
      # end of validation

      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' computes WISP model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      c_max <- apply(self$pm_orig, 2, max)
      self$pm <- sweep(self$pm_orig, 2, c_max, FUN = "/")
      weighted_pm <- as.data.frame(sweep(self$pm, 2, self$w, "*"))
      min_indices <- which(self$minmax == "min")
      max_indices <- which(self$minmax == "max")
      min_sums <- rowSums(weighted_pm[, min_indices])
      max_sums <- rowSums(weighted_pm[, max_indices])
      min_products <- apply(weighted_pm[, min_indices], 1, prod)
      max_products <- apply(weighted_pm[, max_indices], 1, prod)
      u_wsd <- max_sums - min_sums
      u_wpd <- max_products - min_products
      u_wsr <- max_sums / min_sums
      u_wpr <- max_products / min_products
      u_wsd2 <- private$util(u_wsd)
      u_wpd2 <- private$util(u_wsd)
      u_wsr2 <- private$util(u_wsr)
      u_wpr2 <- private$util(u_wpr)
      self$u <- (u_wsd2 + u_wpd2 + u_wsr2 + u_wpr2) / 4
      self$u_sorted <- sort(self$u, decreasing = TRUE)
      self$us <- (u_wsd2 + u_wpr2) / 2
      self$us_sorted <- sort(self$us, decreasing = TRUE)
    },

    #' @description
    #' prepares summary of the WISP method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("WISP method results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n"))
      print(self$u_sorted, pretty = TRUE)
      cat("\n\nS-WISP method results:\n")
      print(self$us_sorted, pretty = TRUE)
    }
  ),
  private = list(
    # @description
    # computes
    #
    # \mjsdeqn{\overline{u_i} = \frac{1 + u}{1 + max_i u}}
    #
    # @param vect u vector (u_wsd, u_wpd, u_wsr or u_wpr)
    # @return recalculated u value
    util = function(vect) {
      return((1 + vect) / (1 + max(vect)))
    }
  )
)