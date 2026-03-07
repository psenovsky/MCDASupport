#' Modeified Similarity Index
#'
#' @description
#' uses modeified \link{topsis}. The modification start in later steps of the
#'  procedure, namely step 6, when degree of conflict bertween each alternative
#'  and ideal and antiideal se coumputed.
#'
#' \mjsdeqn{cos e_i^+ = \frac{\sum_{j=1}^m y_{ij}A_j^+}{\sqrt{\sum_{j=1}^m y_{ij}^2}\sqrt{\sum_{j=1}^m A_j^{+2}}}}
#'
#' \mjsdeqn{cos e_i^- = \frac{\sum_{j=1}^m y_{ij}A_j^-}{\sqrt{\sum_{j=1}^m y_{ij}^2}\sqrt{\sum_{j=1}^m A_j^{-2}}}}
#'
#' Determine degree of similarity between each pair of alternatives and ideal
#'  and antiideal variant.
#'
#' \mjsdeqn{S_i^+ = \frac{cos e_i^+ \sqrt{\sum_{j=1}^m y_{ij}^2}}{|A_j^+|}}
#'
#' \mjsdeqn{S_i^- = \frac{|A_j^+|}{cos e_i^- \sqrt{\sum_{j=1}^m y_{ij}^2}}}
#'
#' Finally overall performance score is computed:
#'
#'  \mjsdeqn{P_i = \frac{S_i^+}{S_i^+ + S_i^-}}
#'
#' @references
#' Chakrakborty, S., Chatterjee, P. Das, P. P. Multi-Criteria Decision Making
#'  Methods in Manufacturing Environments: Models and Applications. CRC Press,
#'  Boca Raton, 2024, 450 p., ISBN: 978-1-00337-703-0
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
msim <- R6Class(
  "msim",
  public = list(
    #' @field pm performance matrix (alternatives in rows, criteria in columns)
    pm = NULL,

    #' @field w numeric weight vector
    w = NULL,

    #' @field minmax vector of optimization directions - min/max
    minmax = NULL,

    #' @field result dataframe with computational results: degree of conflict
    #'  with ideal (cos_e_plus) and antiideal (cos_e_minus), similarity to ideal
    #'  (s_plus), similarity to antiideal (s_minus), performance (Ri), rank
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
    #' t <- msim$new(M, w, minmax)
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
      
      A_plus <- A_minus <- rep(0, times = ncri)
      names(A_plus) <- names(A_minus) <- cri
      for (j in 1:ncri) {
        normPM[, j] <- mcda_norm(self$pm[, j], minmax = "max", method = "vector")
      }
      # compute utility functions
      rij <- sweep(normPM, 2, self$w, "*")
      for (j in 1:ncri) {
        if (self$minmax[j] == "max") {
          A_plus[j] <- max(rij[, j])
          A_minus[j] <- min(rij[, j])
        } else {
          A_plus[j] <- min(rij[, j])
          A_minus[j] <- max(rij[, j])
        }
      }
      # ideal and antiideal variants
      Ap <- Am <- 0
      # degree of conflict
      y2 <- y_ideal <- y_anti <- rep(0, times = nalt)
      for (j in 1:ncri) {
        y_ideal <- y_ideal + rij[, j] * A_plus[j]
        y_anti <- y_anti + rij[, j] * A_minus[j]
        y2 <- y2 + rij[, j]^2
        Ap <- Ap + A_plus[j]^2
        Am <- Am + A_minus[j]^2
      }
      y2 <- sqrt(y2)
      Ap <- sqrt(Ap)
      Am <- sqrt(Am)
      cos_e_plus <- y_ideal / (y2 * Ap)
      cos_e_minus <- y_anti / (y2 * Am)
      # degree of similarity
      s_plus <- s_minus <- rep(0, times = nalt)
      for (i in 1:nalt) {
        s_plus[i] <- (cos_e_plus[i] * y2[i]) / Ap
        s_minus[i] <- Am / (cos_e_minus[i] * y2[i])
      }
      
      # overal performance
      Ri <- s_plus / (s_plus + s_minus)
      rank <- rank(-Ri, ties.method = "min")

      # results
      result <- data.frame(
        cos_e_plus,
        cos_e_minus,
        s_plus,
        s_minus,
        Ri,
        rank
      )
      rownames(result) <- alt
      self$result <- result
    },

    #' @description
    #' summary of the MSIM method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "Modified Similarity Index:\n",
        "processed ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria\n"
      ))
      print(self$result, pretty = TRUE)
    }
  )
)