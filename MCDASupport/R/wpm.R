#' Weighted Product Model
#'
#' @description
#' One of original (simpliest) models in MCDA. Works similarly to \link{wsm},
#'  but instead of summing weighted performance of the alternatives it is based
#'  on product of performances of alternatives to power of weights.
#'
#' Mathematically this is expressed as:
#'
#' \mjsdeqn{P(A_K) = \prod_{j=1}^{n} a_{Kj}^{w_j}}
#'
#' Where a is performance of alternative K in sriterium j, n is number of
#'  criteria and w are weights. Computed value of P is directly comparable.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords WSM WPM
wpm <- R6Class("wpm",
  public = list(

    #' @field pm normalized performance matrix alternatives in rows, criteria
    #'  in columns
    pm = NULL,

    #' @field w weights, sum of weights = 1
    w = NULL,

    #' @field p final weighted product of the alternative
    p = NULL,

    #' @description
    #' validates input parameter, runs the computation of the WPM model and
    #'  returns WPM object with the imputs and resutls of the model.
    #'
    #' WPM does not have any means of normalization, so it is up to the
    #'  analytic to provide the normalized performance matrix. This also
    #'  presumes that the cost/benefit criteria have already been resolved
    #'  outside the model.
    #'
    #' @param pm normalized performance matrix alternatives in rows, criteria
    #'  in columns
    #' @param w weights, sum of weights = 1
    #'
    #' @return instance of WPM class with results
    #'
    #' @examples
    #' PM <- rbind(
    #'  c(25, 20, 15, 30),
    #'  c(10, 30, 20, 30),
    #'  c(30, 10, 30, 10)
    #' )
    #' rownames(PM) <- c("A1", "A2", "A3")
    #' colnames(PM) <- c("C1", "C2", "C3", "C4")
    #' w <- c(0.2, 0.15, 0.4, 0.25)
    #' t <- wpm$new(PM, w)
    initialize = function(pm, w) {
      # check param
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      validation$validate_w_sum_eq_1(w)
      # end of check

      self$pm <- pm
      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' Computes the WPM model based on the properties of the class. Usually it
    #'  does not need to be run manually as this function is run by the class's
    #'  constructor.
    compute = function() {
      ncri <- ncol(self$pm)
      w_pm <- self$pm
      for(i in 1:ncri) {
        w_pm[, i] <- self$pm[, i]^self$w[i]
      }
      self$p <- apply(w_pm, 1, prod)
    },

    #' @description
    #' prepares summary of the WPM method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("WPM method:\nComputed ", nalt, " alternatives in ", ncri,
                " criteria\n\nResults:\n"))
      print(self$p)
    }
  )
)