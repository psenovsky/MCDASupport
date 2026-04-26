#' R-method
#'
#' @description
#' Simple decision support method based on evaluation of rands assigned to the
#'  criteria (ranking importance of criteria) and performance of alternatives
#'  in the criteria.
#'
#' For both weights are computed:
#'
#' \mjsdeqn{w_j = \frac{1/\sum_{k=1}^j(1/r_k)}{\sum_{j=1}^n(1/r_k)}}
#'
#' where wj is the weight of the j-th alternative or criterium, rk is the rank
#'  assigned to k-th alternative or criterium.
#'
#' The resulting two sets of weights are aggregated basicaly using \link{wsm}
#'  method, where criteria weights serve as weights and performance weights
#'  serve as pervormance measure.
#'
#' Rasulting score can be used for ranking purposes, with highest value being
#'  the best.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @references
#' Shankar Chakraborty, Prasenjit Chatterjee, Partha Protim Das. R METHOD. In
#'  Multi-Criteria Decision-Making Methods in Manufacturing Environments. Apple
#'  Academic Press: New York, 2023, pp. 337-342, ISBN 9781003377030.
r_method <- R6Class(
  "r_method",
  public = list(
    #' @field rm ranking matrix consisting of performance ranks evaluation of
    #'  alternatives in criteria
    rm = NULL,

    #' @field criteria_ranks ranks of criteria describing their importance to the
    #'  decision
    criteria_ranks = NULL,

    #' @field w weights vector (based on criteria_ranks)
    w = NULL,

    #' @field wrm weights data frame derived form ranking matrix
    wrm = NULL,

    #' @field result data frame with composite score and assigned ranks
    result = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param rm normalized performance matrix
    #' @param criteria_ranks anks of criteria describing their importance to the
    #'  decision
    initialize = function(rm, criteria_ranks) {
      # validity check
      ncri <- ncol(rm)
      nalt <- nrow(rm)
      validation$validate_matrix_numeric(rm)
      validation$validate_no_elements_vs_cri(
        criteria_ranks,
        ncri,
        "ranks of the criteria",
        TRUE
      )
      if (max(criteria_ranks) > ncri || min(criteria_ranks) < 1) {
        stop("some ranks in criteria_ranks are out of bounds")
      }
      if (max(rm) > nalt || min(rm) < 1) {
        stop("some values in ranking matrix are out of bounds")
      }
      # end of validaty check

      self$criteria_ranks <- criteria_ranks
      self$rm <- rm
      self$compute()
      self
    },

    #' @description
    #' performs computation of SECA model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$rm)
      nalt <- nrow(self$rm)
      cri <- colnames(self$rm)
      alt <- rownames(self$rm)
      result <- self$rm

      for (k in 1:ncri) {
        result[, k] <- private$norm_weights(self$rm[, k], alt)
      }
      w <- private$norm_weights(self$criteria_ranks, cri)
      names(w) <- cri
      self$w <- w
      self$wrm <- result
      t <- wsm$new(result, w)
      t$result_table$ranks <- rank(-t$result_table$weighted_sum)
      self$result <- t$result_table
      #weighted_pm <- as.data.frame(sweep(result, 2, w, "*"))
      #self$result <- rowSums(weighted_pm)
    },

    #' @description
    #' prepares summary of the R method resutls and outputs them
    #'  to the console.
    summary = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      cat(paste(
        "R method:\nProcessed, ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria.\n\nComputed wegiths:\n"
      ))
      print(self$w, pretty = TRUE)
      cat("\nComputed score:\n")
      print(self$result, pretty = TRUE)
    }
  ),

  private = list(
    # compute weights for vector
    #
    # @param r_vector rank vector
    # @param alt names for the vector
    # @return weights
    norm_weights = function(r_vector, alt) {
      crit <- r_vector
      names(crit) <- alt
      ur <- sort(unique(crit)) # unique ranks sorte ascending
      wrm1 <- sort(crit)
      cum_inv <- cumsum(1 / ur)
      level_weights <- 1 / cum_inv
      names(level_weights) <- ur
      wrm <- level_weights[as.character(crit)]
      return(wrm / sum(wrm))
    }
  )
)