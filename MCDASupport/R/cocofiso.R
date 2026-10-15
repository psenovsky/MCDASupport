#' Combined Compromise for Ideal Solution
#'
#' @description
#' Method takes two approaches (strategies) to weighted utility. The process
#'  starts with normalization of performance matrix:
#'
#' \mjsdeqn{r_{ij} é \frac{x_{ij}}{\sqrt{\sum_{i=1}^m}x_{ij}^2}}
#'
#' Then the weights are applied in two different ways:
#'
#' \mjsdeqn{S_i = \sum_{j=1}^n w_j r_{ij}}
#'
#' \mjsdeqn{P_i = \sum_{j=1}^n r_{ij}^{w_j}}
#'
#' Three appraisal score strategies are then used to compute relative weights of
#'  the alternatives. Information from these measures is then used to formulate
#'  compromise solution.
#'
#' \mjsdeqn{k_{ia} = \frac{P_i + S_i}{\sum_{i=1}^m P_i + S_i}}
#'
#' \mjsdeqn{k_{ib} = \frac{P_i + S_i}{1 + \frac{S_i}{1+S_i} + \frac{P_i}{1+P_i}}}
#'
#' \mjsdeqn{k_{ic} = \frac{\lambda S_i + (1 - \lambda)P_i}{\lambda \max_i S_i + (1-\lambda) \max_i P_i}; 0 \le \lambda \le 1}
#'
#' \mjsdeqn{k_i = (k_{ia} k_{ib} k_{ic})^{1/3} + \frac{1}{3}(k_{ia} k_{ib} k_{ic})}
#'
#' Ki measure is directly usable for rank derivation (highest value is best).
#'
#' @references
#' SAH, S., Sardar, S., Das, D. Optimization of W-EDM process for superalloy by
#'  different objective weight integrated MCDM - a comparative study of CoCoSo
#'  and CoCoFISo methods. International Journal on Interactive Design and
#'  Manufacturing (IJIDeM) (2026) 20: pp. 381–406,
#'  https://doi.org/10.1007/s12008-025-02397-1
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords CoCoFISo
cocofiso <- R6Class(
  "cocofiso",
  public = list(
    #' @field pm performance matrix
    pm = NULL,

    #' @field w vector of weights (there are no other limitations on weights)
    w = NULL,

    #' @field minmax vector specifying optimization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    minmax = NULL,

    #' @field result data frame with wighted sum, utility and rank
    result = NULL,

    #' @field lambda 0.5 by default for balancing aggregation strategies
    lambda = 0.5,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm normalized performance matrix
    #' @param w vector of weights
    #' @param minmax minmax vector specifying optimization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    #' @param lambda 0.5 by default for balancing aggregation strategies
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
    #' w = c(0.125, 0.2, 0.2, 0.2, 0.175, 0.05, 0.05)
    #' t <- cocofiso$new(M, w)
    initialize = function(pm, w, minmax = "max", lambda = 0.5) {
      # validation
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_scalar_numeric(lambda, "balancing coefficient")
      if (lambda < 0 || lambda > 1) stop("Value of lambda must be in interval <0; 1>")
      # end of validation

      self$pm <- pm
      self$w <- w
      self$lambda <- lambda
      self$compute()
      self
    },

    #' @description
    #' performs computation of cocofiso model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$pm)
      pnorm <- S <- P <- self$pm
      for (i in 1:ncri) { #normalization
        pnorm[, i] <- self$pm[, i] / sqrt(sum(self$pm[, i]^2))
        S[, i] <- self$w[i] * pnorm[, i]
        P[, i] <- pnorm[, i]^self$w[i]
      }
      # comparability sequence weighting
      Si <- rowSums(S)
      Pi <- rowSums(P)

      # aggregation strategies
      kia <- (Pi + Si) / sum(Pi + Si)
      kib <- (Pi + Si) / (1 + Si / (1 + Si) + Pi / (1 + Pi))
      kic <- (self$lambda * Si + (1 - self$lambda) * Pi) / (self$lambda * max(Si) + (1 - self$lambda) * max(Pi))
      ki <- (kia * kib * kic)^(1/3) + (kia + kib + kic) / 3

      rank <- rank(-ki, ties.method = "min")
      result <- data.frame(
        kia,
        kib,
        kic,
        ki,
        rank
      )
      colnames(result) <- c("aggr. A","aggr. B", "aggr. C", "compromise", "rank")
      rownames(result) <- rownames(self$pm)
      self$result <- result
    },

    #' @description
    #' prepares summary of the CoCoFISo method results and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0("CoCoFISo method\nprocessed ", nalt, " alternatives in ", ncri, " criteria\n\nResults:\n"))
      print(self$result, pretty = TRUE)
    }
  )
)
