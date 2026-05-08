#' Multi-Attribute Ideal-Real Comparative Analysis
#'
#' @description
#' MAIRCA evaluations is argued in sense of gaps between these ideal
#'  solutions and the real preformance of the alternatives. The better the
#'  alternative is, the lower the gap should be.
#'
#' We start the process by formulating performance matrix PM as usual (criteria
#'  in colums and alternatives in rows). We set the preferences for decision
#'  maker PA to 1/m, where m in number of alternatives.
#'
#' Next perform theoretical evaluation by applying weights to PA. To establish
#'  real evaluation matrix we compare performance of the alternative
#'  in the criterium with ideal (or antiideal solution) and divide by the
#'  difference between ideal and anti-ideal solution.
#'
#' For benefit criteria we do that by:
#'
#' \mjsdeqn{t_{rij} = t_{pij}(\frac{x_{ij} - x_i^-}{x_i^+ - x_i^-})}
#'
#' For cost criteria:
#'
#' \mjsdeqn{t_{rij} = t_{pij}(\frac{x_{ij} - x_i^+}{x_i^- - x_i^+})}
#'
#' where xij ... elements of performance matrix, xi+ ... ideal solution, xi-
#'  ... andtiideal solution and tpij are weighted preferences.
#'
#' We compute gap matrix G by comparing weighted preferences and results of
#'  real evaluation (tpij - trij). If tpij == trij then gij = 0, if tpij > trij
#'  then gij = tpij - trij.
#'
#' We aggregate gap matrix across the criteria to establish final values of
#'  criteria functions:
#'
#' \mjsdeqn{Q_i = \sum_{j=1}^n g_{ij}}
#'
#' This measure is directly usable for ranking purposes. Since we are
#'  minimizing gaps we are also minimizing Qi.
#'
#' @references
#' GIGOVIĆ, Ljubomir, PAMUČAR, Dragan, BAJIĆ, Zoran, MILIĆEVIĆ, Milić. The
#'  Combination of Expert Judgment and GIS-MAIRCA Analysis for the Selection of
#'  Sites for Ammunition Depots. Sustainability. 2016, vol. 8, no. 4, available
#'  from: https://doi.org/10.3390/su8040372, ISSN 2071-1050.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords MAICRA
mairca <- R6Class(
  "mairca",
  public = list(
    #' @field pm performance matrix (alternatives in rows and criteria in
    #'  columns)
    pm = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field minmax vector of optimization directions. Use only values of min
    #'  or max. Single min/max will apply chosen direction to all criteria
    minmax = NULL,

    #' @field gap gap matrix describing gap between the preference a real
    #'  performance of the alternative
    gap = NULL,

    #' @field Q vector of final values of criteria functions for alternatives
    Q = NULL,

    #'@field rank vector of ranks based on Q
    rank = NULL,

    #' @field result data frame with gap, Q and rank
    result = NULL,

    #' @description
    #' public constructor allowing the user to construct Borta count decision
    #'  analysis problem and compute it.
    #'
    #' @param pm Matrix or data frame containing the performance table. Each
    #'  row corresponds to an alternative, and each column to a criterion. only
    #'  numeric values expercted. Rows and columns are expected to be named.
    #' @param w weight vector
    #' @param minmax value or vector of values 'min' or 'max' specifying
    #'  optimization direction for the criterium
    #'
    #' @return initialized R6 class with computed results for MAIRCA
    #'
    #' @examples
    #' cri <- c("C1", "C2", "C3")
    #' alt <- c("A1", "A2", "A3", "A4", "A5")
    #' pm <- cbind(
    #'   c(5, 10, 4, 8, 1),
    #'   c(4, 8, 2, 16, 4),
    #'   c(7, 7, 6, 8, 5)
    #' )
    #' rownames(pm) <- alt
    #' colnames(pm) <- cri
    #' minmax <- c("max", "max", "min")
    #' w <- c(0.25, 0.33, 0.42)
    #' result <- mairca$new(pm, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      ## Validation
      ncri <- ncol(pm)
      self$pm <- pm
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncol(pm))
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_w_sum_eq_1(w)
      # end of validation

      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' Computes MAIRCA model based on parametrs specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation si required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)
      cri <- colnames(self$pm)
      TP <- matrix((1 / nalt) * self$w, nrow = nalt, ncol = ncri, byrow = TRUE)
      TR <- matrix(0, nrow = nalt, ncol = ncri)
      rownames(TR) <- alt
      colnames(TR) <- cri
      G <- TR
      x_ideal <- rep(0, times = ncri)
      names(x_ideal) <- cri
      x_anti <- x_ideal
      for (j in 1:ncri) {
        if (self$minmax[j] == "max") {
          # compute ideal and anti-ideal solutions
          x_ideal[j] <- max(self$pm[, j])
          x_anti[j] <- min(self$pm[, j])
        } else {
          x_anti[j] <- max(self$pm[, j])
          x_ideal[j] <- min(self$pm[, j])
        }
        for (i in 1:nalt) {
          # perform real evaluation
          if (self$minmax[j] == "max") {
            TR[i, j] <- TP[i, j] *
              ((self$pm[i, j] - x_anti[j]) / (x_ideal[j] - x_anti[j]))
          } else {
            TR[i, j] <- TP[i, j] *
              ((self$pm[i, j] - x_ideal[j]) / (x_anti[j] - x_ideal[j]))
          }
          # perform gap analysis
          if (TP[i, j] > TR[i, j]) G[i, j] <- TP[i, j] - TR[i, j]
        }
      }
      # final values
      Q <- rowSums(G)

      # formulate results
      self$Q <- Q
      self$gap <- G
      self$rank <- rank(Q, ties = "min")
      self$result <- data.frame(Q, self$rank)
      colnames(self$result) <- c("Q", "rank")
      rownames(self$result) <- alt
    },

    #' @description
    #' summary of the MAIRCA count method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "MAIRCA:\n",
        "processed ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria\n\nResults:\n"
      ))
      print(self$result, pretty = TRUE)
    }
  )
)
