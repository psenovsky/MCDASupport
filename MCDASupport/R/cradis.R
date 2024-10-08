#' compromise ranking of alternatives from distance to ideal solution
#'
#' @description
#' Approach to ranking problem integrating parts of \link{aras}, \link{marcos}
#'  and \link{topsis} methods.
#'
#' Similarly to ARAS method it starts with decision problem formulation using
#'  performance metrix with n criteria in columns and m alternatives in rows
#'  (step 1). In step 2) The performance in criteria is then normalized using:
#'
#' \mjsdeqn{r_{ij} = \frac{x_{ij}}{x_{max_j}}}
#'
#' for benefit criteria and for cost criteria
#'
#' \mjsdeqn{r_{ij} = \frac{x_{min_j}}{x_{ij}}}
#'
#' In step 3) the weights are applied on normalized matrix.
#'
#' \mjsdeqn{v_{ij} = w_{j} r_{ij}}
#'
#' In step 4) determining ideal and antiideal solution of the problem as
#'  miximal (ideal) and minimal (anti-ideal) values of criteria.
#'
#' \mjsdeqn{t_{i} = max(v_{ij})}
#'
#' \mjsdeqn{t_{ai} = min(v_{ij})}
#'
#' In step 5) deviations from these is computed as difference between the
#'  normalized weighted value and the ideal or antiideal solution:
#'
#' \mjsdeqn{d^+ = t_i - v_{ij}}
#'
#' \mjsdeqn{d^- = v_{ij} - t_ai}
#'
#' Then the grades s are computed by summing up these differences (step 6).
#'
#' \mjsdeqn{s_i^+ = \sum_{j=1}^n d^+}
#'
#' \mjsdeqn{s_i^- = \sum_{j=1}^n d^-}
#'
#' Then the utility function is constructed as a ratio between the optimum and
#'  actual value (step 7).
#'
#' \mjsdeqn{K_i^+ = \frac{s_0^+}{s_i^+}}
#'
#' \mjsdeqn{K_i^- = \frac{s_i^-}{s_0^-}}
#'
#' Finaly (step 8) the ranking of the alternatives ise derived from the average
#'  of utility functions.
#'
#' \mjsdeqn{Q_i = \frac{K_i^+ + K_i^-}{2}}
#'
#' First 4 steps of the method are closely mimicking ARAS approach, remainder
#'  is philosophically closer to MARCOS or TOPSIS methods. Mathematically the
#'  approach to the problem is closer to MARCOS.
#'
#' @references
#' Puška, A., Stević, Ž., Panučar, D.: Evaluation and selection of healtcare
#'  waste incinerators using extended sustainability criteria and multicriteria
#'  analysis methods. Environment, Development and Sustainability, vol. 24, pp.
#'  11195-11225 (2022), DOI: 10.1007/s10668-021-01902-2
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords MARCOS CRADIS ARAS TOPSIS
cradis <- R6Class("cradis",
  public = list(
    #' @field pm_orig original state of the performance matrix (before
    #'  transforming criteria)
    pm_orig = NULL,

    #' @field pm transformed performance matrix, all criteria are maximized
    pm = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field minmax directio of optimization, can be replaced by single min
    #'  or max if all criteria have same optimization direction.
    minmax = NULL,

    #' @field q sorted average utility order descending from best to worst
    q = NULL,

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
    #' PM <- rbind(
    #'    c(0.446, 1.785, 6.643, 6.843, 50),
    #'    c(1.113, 2.425, 10.525, 2.902, 250),
    #'    c(1.246, 3.321, 14.224, 3.885, 600),
    #'    c(1.935, 3.678, 17.852, 4.406, 1000),
    #'    c(0.446, 3.062, 5.238, 3.112, 200),
    #'    c(1.064, 3.814, 14.558, 4.121, 250),
    #'    c(1.654, 4.581, 17.888, 4.886, 1600),
    #'    c(1.924, 5.226, 22.224, 5.702, 1500),
    #'    c(0.337, 4.444, 24.708, 4.123, 450),
    #'    c(0.998, 5.12, 18.012, 5.206, 1500),
    #'    c(1.622, 5.886, 22.226, 6.226, 600),
    #'    c(1.844, 6.234, 26.128, 6.786, 1500),
    #'    c(0.531, 5.6, 18.883, 5.405, 800),
    #'    c(1.023, 6.123, 21.987, 6.501, 1500),
    #'    c(1.664, 7.244, 27.012, 7.421, 1600),
    #'    c(2.012, 7.345, 28.021, 7.923, 1000)
    #' )
    #' rownames(PM) <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9",
    #'    "A10", "A11", "A12", "A13", "A14", "A15", "A16")
    #' colnames(PM) <- c("Ra", "Ax", "Ay", "Az", "Q")
    #' w <- rep(0.2, times = 5) #equal weights
    #' minmax <- c("min", "min", "min", "min", "max")
    #' t <- cradis$new(PM, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      # parameters validation
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      validation$validate_w(w, ncri)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of parameter validation

      self$pm_orig <- pm
      self$pm <- as.data.frame(pm)
      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' computes the model bas of class properties. Usually we do not run the
    #'  computation manually (it is run from class' constructor).
    compute = function() {
      ncri <- ncol(self$pm)
      x_max <- self$pm  %>%
        summarize_all(max) %>%
        as.numeric()
      x_min <- self$pm  %>%
        summarize_all(min) %>%
        as.numeric()
      # step 2) normalization and 3) weights application
      pm_norm <- self$pm
      for (i in 1:ncri) {
        if (self$minmax[i] == "max") {
          pm_norm[, i] <- self$pm[, i] / x_max[i]
        } else if (self$minmax[i] == "min") {
          pm_norm[, i] <- x_min[i] / self$pm[, i]
        }
      }
      pm_w <- sweep(pm_norm, 2, self$w, FUN = "*")
      # step 4) ideal and anti-ideal solution
      t_i <- apply(pm_w, 2, max)
      t_ai <- apply(pm_w, 2, min)
      # step 5) compute d+ and d-
      # d- = t_i - pm_w
      d_plus <- sweep(-pm_w, 2, t_i, FUN = "+")
      #d+ = pm_w - t_ai
      d_minus <- sweep(pm_w, 2, t_ai, FUN = "-")
      # step 6) compute s+ and s-
      s_plus <- rowSums(d_plus)
      s_minus <- rowSums(d_minus)
      # stap 7) compute k+ and k-
      k_plus <- t_i / s_plus
      k_minus <- s_minus / t_ai
      # step 8) compute q
      q <- (k_plus + k_minus) / 2
      self$q <- sort(q, decreasing = TRUE)
    },

    #' @description
    #' summary of the CRADIS method resutls.
    #' @return basic information on the model including ranking.
    summary = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0("\nCRADIS:\n", "processed ", nalt,
                 " alternatives in ", ncri,
                 " criteria\n\nFinal order (Q-based):\n"))
      print(self$q, pretty = TRUE)
    }
  )
)