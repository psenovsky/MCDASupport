#' Alternative Ranking Order Method Accounting for Two-Step Normalization
#'
#' @description
#' Extension of \link{aroman} method. Starts with same steps as AROMAN method
#'  up to computation if Li and Ai and continues with raising these to lambda:
#' 
#' \mjsdeqn{L_i^{'} = L_i^\lambda}
#' 
#' and
#' 
#' \mjsdeqn{A_i^{'} = A_i^\lambda}
#' 
#' And finally we compute Ri:
#' 
#' \mjsdeqn{R_i = e^{A_i^{'} - L_i^{'}}}
#' 
#' @references
#' Bošković, S., Švadlenka, L., Dobrodolac, M., Jovčić, S., & Zanne, M. (2023).
#'  An Extended AROMAN Method for Cargo Bike Delivery Concept Selection.
#'  Decision Making Advances, 1(1), 1–9. https://doi.org/10.31181/v120231
#' 
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords extendedAROMAN
extended_aroman <- R6Class(
  "extended_aroman",
  public = list(
    #' @field pm performance matrix of alternatives in criteria
    pm = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field beta value 0-1 representing weighting factor between 2 types of
    #'  normalization (default = 0.5)
    beta = NULL,

    #' @field lambda values 0-1, coefficient degree of the criteria type (0.5
    #'  by default)
    lambda = NULL,

    #' @field result dataframe with cost (Li) and Bbenefit (Ai) normalized
    #'  aggregated value, final result (Ri) and rank of alternatives.
    result = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm performance matrix
    #' @param w vector of weights, its sum must be equal to 1
    #' @param minmax minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    #' @param beta value 0-1 representing weighting factor between 2 types of
    #'  normalization (default = 0.5)
    #' @param lambda values 0-1, coefficient degree of the criteria type (0.5
    #'  by default)
    #'
    #' @examples
    #' pm <- data.frame(
    #'   C1 = c(0.185, 0.317, 0.555, 0.731, 0.948),
    #'   C2 = c(2.33, 1.08, 6.45, 8.88, 7.39),
    #'   C3 = c(454, 298, 174, 849, 517)
    #' )
    #' rownames(pm) <- c("A1", "A2", "A3", "A4", "A5")
    #' w <- c(0.25, 0.4, 0.35)
    #' minmax <- c("max", "min", "max")
    #' res <- extended_aroman$new(pm, w = w, minmax = minmax)
    initialize = function(pm, w, minmax = "max", beta = 0.5, lambda = 0.5) {
      # validity check ... nothing here compute AROMAN and continue from there
      t <- aroman$new(pm, w, minmax, beta, lambda)
      # end of validaty check

      self$w <- t$w
      self$pm <- t$pm
      self$minmax <- t$minmax
      self$beta <- beta
      self$lambda <- lambda
      self$result <- t$result
      self$compute()
      self
    },

    #' @description
    #' performs computation of extended AROMAN model based on class properties.
    #'  Usually is not run manually as constructor calls this method
    #'  automatically.
    compute = function() {
      Li <- self$result$Li^self$lambda
      Ai <- self$result$Ai^self$lambda
      Ri <- exp(Ai - Li)
      rank <- rank(-Ri, ties.method = "min")
      # result
      self$result$Li <- Li
      self$result$Ai <- Ai
      self$result$Ri <- Ri
      self$result$rank <- rank
    },

    #' @description
    #' summary of the extended AROMAN method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "Extended AROMAN:\n",
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