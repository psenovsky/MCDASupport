#' Election Based on Relatived Value Distance
#'
#' @description
#' A evolution of TOPSIS method to provide more stable results. The method
#'  introduces concept of reference points for the criteria the values below
#'  this point are considered losses, values over it gains.
#'
#' The procedure starts with vertor normalization of the performance matrix
#'  (max). To the same scale the reference points are transformed:
#'
#' \mjsdeqn{\varphi_j = \frac{\mu_{ij}}{\sum_{j=1}^m d_{ij}}}
#'
#' Then the value af the alternative to criteria. For benefit function:
#'
#' \mjsdeqn{v_{ij} = (r_{ij} - \varphi_j)^\alpha}
#'
#' for \mjseqn{r_{ij} > \varphi_j} otherwise
#'
#' \mjsdeqn{v_{ij} = -\lambda(\varphi_j - r_{ij})^\alpha}
#'
#' for cost criteria
#'
#' \mjsdeqn{v_{ij} = (\varphi_j - r_{ij})^\alpha}
#'
#' for \mjseqn{r_{ij} < \varphi_j}, otherwise
#'
#' \mjsdeqn{v_{ij} = -\lambda(r_{ij} - \varphi_j)^\alpha}
#'
#' This concept was introduced by Tvertsky and Kahneman, with
#'  \mjseqn{\lambda = 2.25} and \mjseqn{\alpha = 0.88} from the empirical
#'  studies. Generally \mjseqn{\lambda \in <2; 2.5>}.
#'
#' Then ideal (A+ ... PIS - positive ideal solution) and antiideal (A- ... NIS
#'  - negative ideal solutions) are identified as max or min in the vij and
#'  separation measures for PIS and NIS are compited:
#'
#' \mjsdeqn{S_i^+ = \sum_{j=1}^n w_j \cdot |v_{ij} - v_j^+|}
#'
#' \mjsdeqn{S_i^- = \sum_{j=1}^n w_j \cdot |v_{ij} - v_j^-|}
#'
#' Finaly relative closeness index for each cleseness of each alternative to
#'  ideal solution. Last two steps are same as in TOPSIS:
#'
#' \mjsdeqn{\phi_i = \frac{S_i^-}{S_i^+ + S_i^-}}
ervd <- R6Class(
  "ervd",
  public = list(
    #' @field pm performance matrix
    pm = NULL,

    #' @field w weights vector
    w = NULL,

    #' @field ref_points reference points for the criteria
    ref_points = NULL,

    #' @field minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    minmax = NULL,

    #' @field lambda alternation factor of the losses, 2.25 by default, should be in <2;2.5>
    lambda = 2.25,

    #' @field alpha diminishing sensitivity parameter, 0.88 by default
    alpha = 0.88,

    #' @field result data frame with results
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
    #' @param ref_points reference points for the criteria
    #' @param lambda alternation factor of the losses, 2.25 by default, should be in <2;2.5>
    #' @param alpha diminishing sensitivity parameter, 0.88 by default
    initialize = function(
      pm,
      w,
      minmax = "max",
      ref_points,
      lambda = 2.25,
      alpha = 0.88
    ) {
      # validity check
      ncri <- ncol(pm)
      self$pm <- pm
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      validation$validate_no_elements_vs_cri(ref_points, ncri, "weights")
      validation$validate_w_sum_eq_1(w)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_scalar_numeric(
        lambda,
        "lambda (alternation factor of losses)"
      )
      validation$validate_scalar_numeric(
        alpha,
        "alpha (diminishing sensitivity parameter"
      )
      if (lambda < 2 || lambda > 2.5) {
        warning("lambda value is usually in <2;2.5> interval")
      }
      # end of validaty check

      self$w <- w
      self$alpha <- alpha
      self$lambda <- lambda
      self$ref_points <- ref_points
      self$compute()
      self
    },

    #' @description
    #' computes ERVD model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      S1 <- S2 <- V <- pm_norm <- self$pm
      rp_norm <- self$ref_points # normalized reference points

      # normalization of PM and reference points
      for (i in 1:ncri) {
        sum_cri <- sum(self$pm[, i])
        pm_norm[, i] <- self$pm[, i] / sum_cri
        rp_norm[i] <- self$ref_points[i] / sum_cri
      }
      # value of alternatives in criteria
      for (i in 1:ncri) {
        for (j in 1:nalt) {
          if (self$minmax[i] == "max") {
            if (pm_norm[j, i] > rp_norm[i]) {
              V[j, i] <- (pm_norm[j, i] - rp_norm[i])^self$alpha
            } else {
              V[j, i] <- -self$lambda * (rp_norm[i] - pm_norm[j, i])^self$alpha
            }
          } else {
            if (pm_norm[j, i] < rp_norm[i]) {
              V[j, i] <- (rp_norm[i] - pm_norm[j, i])^self$alpha
            } else {
              V[j, i] <- -self$lambda * (pm_norm[j, i] - rp_norm[i])^self$alpha
            }
          }
        }
      }
      # ideal (PIS) and negative ideal solution (NIS)
      PIS <- apply(V, 2, max)
      NIS <- apply(V, 2, min)
      # separation measures S_plus, S_minus
      S_plus <- apply(V, 1, function(row) sum(w * abs(row - PIS)))
      S_minus <- apply(V, 1, function(row) sum(w * abs(row - NIS)))
      # relative closeness to ideal solution
      Q <- S_minus / (S_plus + S_minus)

      result <- data.frame(S_plus, S_minus, Q, rank(-Q))
      colnames(result) <- c("S+", "S-", "phi", "rank")
      self$result <- result
    },

    #' @description
    #' Creates summary for ERVD model and sends it to console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste0(
        "ERVD\nprocessed ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria\n\nResults:\n"
      ))
      print(self$result, pretty = TRUE)
    }
  )
)