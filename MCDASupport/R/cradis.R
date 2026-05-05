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
#' Ha, Le Dang. SELECTION OF SUITABLE DATA NORMALIZATION METHOD TO COMBINE WITH
#'  THE CRADIS METHOD FOR MAKING MULTI - CRITERIA DECISION. Applied Engineering
#'  Letters, Vol. 8, No. 1, pp. 24-35 (2023), DOI: 10.18485/aeletters.2023.8.1.4
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

    #' @field result data frame with average utility and ranking
    result = NULL,

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
    #' # https://doi.org/10.18485/aeletters.2023.8.1.4
    #' w <- c(0.257, 0.129, 0.214, 0.196, 0.204)
    #' pm <- rbind(
    #'   c(2, 110, 3, 2, 3),
    #'   c(5, 100, 5, 3, 3),
    #'   c(3, 90, 4, 5, 2),
    #'   c(10, 80, 3, 4, 4),
    #'   c(4, 85, 2, 4, 5),
    #'   c(8, 80, 3, 4, 4),
    #'   c(5, 95, 2, 4, 3)
    #' )
    #' colnames(pm) <- c(paste0("C", 1:5))
    #' rownames(pm) <- c(paste0("S", 1:7))
    #' t <- cradis$new(pm = pm, w = w, minmax = "max")
    #' summary(t)
    initialize = function(pm, w, minmax = "max") {
      # parameters validation
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
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
      # step 7) compute k+ and k-
      k_plus <- max(s_plus) / s_plus
      k_minus <- s_minus / min(s_minus)
      # step 8) compute q
      q <- (k_plus + k_minus) / 2
      self$result <- data.frame(
        q,
        rank(-q)
      )
      colnames(self$result) <- c("avg. util.", "rank")
      rownames(self$result) <- rownames(self$pm)
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
                 " criteria\n\nResults:\n"))
      print(self$result, pretty = TRUE)
    }
  )
)