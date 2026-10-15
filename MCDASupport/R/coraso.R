#' COmpromise Ranking from Alternative Solutions
#'
#' @description
#' CORASO method is in a sense similar to WSM method. The CORASO method uses
#'  weighted aggregated performance as computed by WSM method and uses it to
#'  compute two types of deviations. These are then used to calculate final
#'  score, where higher value represents better result.
#'
#' \mjsdeqn{\psi_i = \frac{\xi_i}{\xi_{i(max)}}}
#'
#' \mjsdeqn{\psi_i^* = \frac{\xi_{i(min)}}{\xi_i}}
#'
#' \mjsdeqn{U_i = \frac{\xi_i - \xi_i^*}{\xi_i + \xi_i^*}}
#'
#' @references
#' Dua, T. V., Bao, N. C., Duc, D. V., Thinh, H. X. (2026). Improving the data
#'  normalization method in the CORASO method. EUREKA: Physics and Engineering,
#'  no. 1, pp. 217–228. Available from:
#'  http://doi.org/10.21303/2461-4262.2026.003950
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ARTASI
coraso <- R6Class("coraso",
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
    #' t <- coraso$new(M, w)
    initialize = function(pm, w, minmax = "max") {
      # validation
      t <- wsm$new(pm, w = w, minmax = minmax)
      # end of validation

      self$pm <- pm
      self$minmax <- t$minmax
      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' performs computation of coraso model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$pm)
      pnorm <- wpnorm <- self$pm
      for (i in 1:ncri) {
        if (self$minmax[i] == "max") { # normalization
          pnorm[, i] <- self$pm[, i] / max(self$pm[, i])
        } else {
          pnorm[, i] <- min(self$pm[, i]) / self$pm[, i]
        }
        # apply weights
        wpnorm[, i] <- pnorm[, i] * self$w[i]
      }
      weighted_sum <- rowSums(wpnorm)
      d1 <- weighted_sum / max(weighted_sum)
      d2 <- min(weighted_sum) / weighted_sum
      U <- (d1 - d2) / (d1 + d2)
      rank <- rank(-U, ties.method = "min")
      result <- data.frame(
        weighted_sum,
        U,
        rank
      )
      colnames(result) <- c("weighted sum", "utility", "rank")
      rownames(result) <- rownames(self$pm)
      self$result <- result
    },

    #' @description
    #' prepares summary of the CORASO method results and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0("CORASO method\nprocessed ", nalt, " alternatives in ", ncri, " criteria\n\nResults:\n"))
      print(self$result, pretty = TRUE)
    }
  )

)
