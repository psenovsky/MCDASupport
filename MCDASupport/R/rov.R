#' Range of Value Method
#'
#' @description
#' Developed by Yakowitz in 1993. Method is based on evaluation of separate
#'  utility functions for benefit (ui+) and cost (ui-) criteria.
#'
#' Evaluation is performed on normalized performance matrix (using min-max
#'  normalization).
#'
#' \mjsdeqn{u_i^+ =\sum_{j \in benefit} w_j \cdot r_{ij}}
#'
#' \mjsdeqn{u_i^- =\sum_{j \in cost} w_j \cdot r_{ij}}
#'
#' To establish rank, we compute uverage of the ui+ and ui-, with higher, the
#'  better values:
#'
#' \mjsdeqn{u_i =\frac{u_i^+ + u_i^-}{2}}
#'
#' @references
#' Chakrakborty, S., Chatterjee, P. Das, P. P. Multi-Criteria Decision Making
#'  Methods in Manufacturing Environments: Models and Applications. CRC Press,
#'  Boca Raton, 2024, 450 p., ISBN: 978-1-00337-703-0
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ROV
rov <- R6Class(
  "rov",
  public = list(
    #' @field pm performance matrix (alternatives in rows, criteria in columns)
    pm = NULL,

    #' @field w numeric vector of weight
    w = NULL,

    #' @field minmax vector with optimization direction (min/max) for the
    #'  criteria
    minmax = NULL,

    #' @field result data frame with computational result: utility for benefit
    #'  (ui_plus), cost (ui_minus), average utility (ui) and rank based on ui
    result = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm normalized performance matrix
    #' @param w vector of weights, its sum must be equal to 1
    #' @param minmax minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    #'
    #' @examples
    #' alternatives <- c('BLR', 'BOM', 'DEL', 'MNL', 'HYD', 'GRU', 'DUB',
    #'   'KRK', 'MAA', 'EZE')
    #' criteria <- c('tlnt', 'stab', 'cost', 'infl', 'tm-zn', 'infr', "life")
    #' M <- rbind(
    #'   c(0.8181818, 0.1814159, 1.0000000, 0.1198582, 0, 0.6, 0.750),
    #'   c(1.0000000, 0.1814159, 0.6666667, 0.1198582, 0, 0.6, 0.375),
    #'   c(1.0000000, 0.1814159, 0.8333333, 0.1198582, 0, 0.6, 0.125),
    #'   c(0.8181818, 0.0000000, 1.0000000, 0.3482143, 0, 0.6, 0.375),
    #'   c(0.1818182, 0.1814159, 1.0000000, 0.1198582, 0, 0.2, 0.375),
    #'   c(0.1818182, 0.1814159, 0.5000000, 0.1198582, 0, 0.2, 0.125),
    #'   c(0.0000000, 1.0000000, 0.0000000, 0.5741667, 1, 1.0, 1.000),
    #'   c(0.3636364, 0.7787611, 0.6666667, 1.0000000, 1, 0.0, 0.500),
    #'   c(0.4545455, 0.1814159, 0.9166667, 0.1198582, 0, 0.4, 0.000),
    #'   c(0.1818182, 0.6283186, 0.5833333, 0.0000000, 0, 0.4, 0.125)
    #' )
    #' rownames(M) <- alternatives
    #' colnames(M) <- criteria
    #' w <- c(0.125, 0.2, 0.2, 0.2, 0.175, 0.05, 0.05)
    #' minmax <- "max"
    #' t <- rov$new(M, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      # validity check
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      validation$validate_w_sum_eq_1(w)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      self$pm <- pm
      # end of validaty check

      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' Computes ROV based on parametrs specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation si required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)
      normPM <- self$pm
      # normalize using min-max method
      for (j in 1:ncri) {
        if (self$minmax[j] == "max") {
          normPM[, j] <- mcda_norm(self$pm[, j], minmax = "max", method = "minmax")
        } else {
          normPM[, j] <- mcda_norm(self$pm[, j], minmax = "min", method = "minmax")
        }
      }
      # compute utility functions
      benefit <- which(self$minmax == "max")
      ui_plus <- rep(0, times = nalt)
      cost <- which(self$minmax == "min")
      ui_minus <- rep(0, times = nalt)
      rij <- sweep(normPM, 2, self$w, "*")
      for (j in benefit) {
        ui_plus <- ui_plus + rij[, j]
      }
      for (j in cost) {
        ui_minus <- ui_minus + rij[, j]
      }
      ui <- (ui_plus + ui_minus) / 2
      rank <- rank(-ui, ties.method = "min")

      result <- data.frame(ui_plus, ui_minus, ui, rank)
      rownames(result) <- alt
      self$result <- result
    },

    #' @description
    #' summary of the ROV method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "ROV:\n",
        "processed ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria\n"
      ))
      print(self$result, pretty = TRUE)
    }
    # TODO implement
  )
)