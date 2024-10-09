#' Complex Proportional Assessment
#'
#' @description
#' COPRAS is a approach pholosophically comming from methods such as
#'  \link{saw}, but it evaluates separately minimizing and maximizing criteria.
#'  In case only maximized criteria are present, the method provides exactly
#'  same results as SAW.
#'
#' The procedure starts with performance matrix PM of the alternatives (rows)
#'  in criteria (columns). Then the PM needs to be normalized using
#'
#' \mjsdeqn{r_{ij} = \frac{x_{ij}}{\sum_{i=1}^m x_{ij}}}
#'
#' Normalized matrix R is then multiplied by weight vector. Then the sum of
#'  normalized weighted matrix is computed. We create S+ and S- separately for
#'  benefit (S+) and cost (S-) criteria.
#'
#' \mjsdeqn{S_{+i} = \sum_{j \in k} y_{ij}}
#'
#' \mjsdeqn{S_{-i} = \sum_{j \in l} y_{ij}}
#'
#' Where k is a set of benefical and l is set of cost criteria. Y is normalized
#'  weighted value.
#'
#' Then we determine relative significance of the criteria using equation
#'
#' \mjsdeqn{Q_i = S_{+i} + \frac{S_{-min}\sum_{i=1}^m S_{-i}}{S_{-i}\sum_{i=1}^m (S_{-min}/S_{-i})}}
#'
#' Value of Q is directly usable to derive ranking of the alternatives. We can
#'  improve readability of the result by normalizing it and multiplying by 100
#'  to get the result in percents.
#'
#' \mjsdeqn{U_i = 100 \cdot \frac{Q_i}{Q_{max}}}
#'
#' @references
#' Taherdoost, Hamed; Mohebi, Atefeh. A Comprehensive Guide to the COPRAS
#'  method for Multi-Criteria Decision Making. Journal of Management Science
#'  & Engineering Research, Vol. 7, no. 2,
#'  https://doi.org/10.30564/jmser.v7i2.6280
#'
#' DEHGHAN-MANSHADI, B., MAHMUDI, H., ABEDIAN, A., MAHMUDI, R. A novel method
#'  for materials selection in mechanical design: Combination of non-linear
#'  normalization and a modified digital logic method. Materials & Design.
#'  2007, Vol. 28, no. 1, pp. 8–15, available from:
#'  https://doi.org/10.1016/j.matdes.2005.06.023, ISSN 0261-3069.
#'
#' @keywords COPRAS SAW
copras <- R6Class("copras",
  public = list(
    #' @field pm performance matrix (alternatives in rows and criteria in
    #'  columns)
    pm = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field minmax vector of optimization direction for criteria (min/max)
    minmax = NULL,

    #' @field q relative significance of the alternative
    q = NULL,

    #' @field u quantitative utility in percents
    u = NULL,

    #' @description
    #' public constructor of the function. Validates inputs and computes the
    #'  model.
    #'
    #' @param pm performance matrix (alternatives in rows and criteria in
    #'  columns)
    #' @param w weights vector
    #' @param minmax vector of optimization direction (min/max, max is default)
    #' @return inicialized and computed model
    #'
    #' @examples
    #' # example from EHGHAN-MANSHADI, see references or
    #' # https://www.youtube.com/watch?v=364ghfEWz_k
    #' PM <- rbind(
    #'   c(75.5, 420, 74.2, 2.8, 21.4, 0.37, 0.16),
    #'   c(95, 91, 70, 2.68, 22.1, 0.33, 0.16),
    #'   c(770, 1365, 189, 7.9, 16.9, 0.04, 0.06),
    #'   c(187, 1124, 210, 7.9, 14.4, 0.03, 0.06),
    #'   c(179, 875, 112, 4.43, 9.4, 0.016, 0.09),
    #'   c(239, 1190, 217, 8.51, 11.5, 0.31, 0.07),
    #'   c(273, 200, 112, 8.53, 19.9, 0.29, 0.06)
    #' )
    #' cri <- c("Toughness index (TI)", "Yield strenght (YS)",
    #'   "Young's modulus (YM)", "Density (D)", "Thermal expansion coef. (TE)",
    #'   "Thermal conductivity (TC)", "Specific heat (SH)")
    #' alt <- c("AI 2024-T6", "AI 5052-0", "SS 301 FH", "SS 310 FH",
    #'   "Ti-6AI-4V", "Inconel 718", "70Cu-30Zn")
    #' colnames(PM) <- cri
    #' rownames(PM) <- alt
    #' minmax <- c("max", "max", "max", "min", "min", "min", "min")
    #' w <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
    #' t <- copras$new(PM, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      # parameters validation
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of parameter validation

      self$pm <- as.data.frame(pm)
      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' computes the model bas of class properties. Usually we do not run the
    #'  computation manually (it is run from class' constructor).
    compute = function() {
      c_max <- colSums(self$pm)
      pm_w <- self$pm %>%
        sweep(2, c_max, FUN = "/") %>%
        sweep(2, self$w, FUN = "*")
      min_indices <- which(self$minmax == "min")
      max_indices <- which(self$minmax == "max")
      s_plus <- rowSums(pm_w[, max_indices])
      s_minus <- rowSums(pm_w[, min_indices])
      q <- s_plus + (min(s_minus) * sum(s_minus)) /
        (s_minus * sum(min(s_minus) / s_minus))
      u <- 100 * q / max(q)
      self$q <- q
      self$u <- u
    },

    #' @description
    #' summary of the COPRAS method resutls.
    #' @return basic information on the model including ranking.
    summary = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0("\nCOPRAS:\n", "processed ", nalt,
                 " alternatives in ", ncri,
                 " criteria\n\nUtility (%):\n"))
      print(sort(self$u, decreasing = TRUE), pretty = TRUE)
    }
  )
)