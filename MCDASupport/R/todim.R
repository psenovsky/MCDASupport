#' TOmada de Decisao Interativa Multicriterio
#'
#' @description
#' Multicriteria decision analysis method based on diminance measure.
#'  Computation starts with normalizing the performance matrix (aleternatives
#'  in rows and criteria in columns) using linear normalization. Then relative
#'  weights are computed.
#'
#' \mjsdeqn{w_{cr} = \frac{w_c}{max w}}
#'
#' Then dominance degree of  the alternatives is computed using
#'
#' \mjsdeqn{\delta (A_i, A_j) = \sum_{c=1}^m \Theta_c(A_i, A_j) \quad \forall(i,j)}
#'
#' \mjsdeqn{\Theta (A_i, A_j) = \left\lbrace\begin{array}{ll} \sqrt{\frac{w_{cr}(P_{ic} - P_{jc})}{\sum_{c=1}^m w_{cr}}} & \text{ if }(P_{ic} - P_{jc}) > 0 \cr 0 & \text{ if }(P_{ic} - P_{jc}) = 0 \cr \frac{-1}{\theta}\sqrt{\frac{(\sum_{c=1}^m w_{cr})(P_{ic} - P_{jc})}{w_{cr}}} & \text{ if }(P_{ic} - P_{jc}) < 0\end{array}\right.}
#'
#' Theta symbol is loss damping factor. Different values lead to different
#'  shapes of the prospect theoretical value function.
#'
#' Finally overall dominance degree for alternatives is computed. This measure
#'  is directly usable to establish ranking of the alternatives.
#'
#' \mjsdeqn{\xi_i = \frac{\sum_{j=1}^m \delta(A_i, A_j) - min \sum_{j=1}^m \delta(A_i, A_j)}{max \sum_{j=1}^m \delta(A_i, A_j) - min \sum_{j=1}^m \delta(A_i, A_j)}}
#'
#' @references
#' SOLÍS, Irvin David Bonilla, PÉREZ-DOMÍNGUEZ, Luis Asunción, DELGADO, Rosa
#'  Patricia Ramírez, DÍAZ, Marling Carolina Cordero. Web Application
#'  Development for TODIM Method Automation and Alternatives Evaluation. Data
#'  and Metadata. 2025, roč. 4, s. 492–492, dostupné z:
#'  https://doi.org/10.56294/dm2025492, ISSN 2953-4917.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords TODIM
todim <- R6Class("todim",
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

    #' @field results overall dominance degree altarnative (best = max)
    results = NULL,

    #' @field results_sorted by dominance degree altarnative (best = max)
    results_sorted = NULL,

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
    #' # from: https://www.youtube.com/watch?v=VetfNIHLyn0
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
    #' t <- todim$new(pm, w, minmax)
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
    #' computes TODIM model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm_orig)
      nalt <- nrow(self$pm_orig)
      alt <- rownames(self$pm_orig)
      self$pm <- self$pm_orig
      for (i in 1:ncri) {
        self$pm[, i] <- mcda_norm(self$pm_orig[, i], self$minmax[i],
                                  "linear aggregation")
      }
      wcr <- self$w / max(self$w)
      theta_c <- list()
      theta_temp <- matrix(0, nrow = nalt, ncol = nalt)
      colnames(theta_temp) <- rownames(theta_temp) <- alt
      sum_w <- sum(wcr)
      t <- 1 # decay constant TODO maybe explore behavior
      for (c in 1:ncri) {
        theta <- theta_temp
        for (i in 1:nalt) {
          for (j in 1:nalt) {
            delta_p <- self$pm[i, c] - self$pm[j, c]
            if (delta_p > 0) {
              theta[i, j] <- sqrt((wcr[c] * delta_p) / sum_w)
            } else if (delta_p == 0) {
              theta[i, j] <- 0
            } else {
              # delta_p < 0
              theta[i, j] <- (-1 / t) * sqrt(abs((sum_w * delta_p)) / wcr[c])
            }
          }
        }
        theta_c[[c]] <- theta
      }
      delta <- Reduce("+", theta_c)
      d_sum <- rowSums(delta)
      d_sum_min <- min(d_sum)
      d_sum_max <- max(d_sum)
      self$results <- (d_sum - d_sum_min) / (d_sum_max - d_sum_min)
      self$results_sorted <- sort(self$results, decreasing = TRUE)
    },

    #' @description
    #' prepares summary of the TODIM method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("WISP TODIM results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n"))
      print(self$results_sorted, pretty = TRUE)
    }
  )
)