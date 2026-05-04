#' Copeland's Method AKA Llull method
#'
#' @description
#' ranked-choice voting system based on counting each candidate's pairwise wins
#'  and losses. System can be applied to general decision situations. First we
#'  rand the alternatives in criteria based on their performance.
#'
#' Then we compute result matrix r.
#'
#' For each comparison we compare respective ranks with rij = 1 if alternative
#'  i is ranked better than j, rij = 0 if alternative j is ranked better than i
#'  and finaly rij = 0.5 if alternatives i and j have same rank. rii = 0
#'  (although technical any value will do not have any impact on ranking).
#'
#' We sum r per j (criteria) to compupe Copland score. Which will is directly
#'  translable into ranking (higher values are better).
#'
#' @references
#' Copeland's method. Wikipedia. available from \url{https://en.wikipedia.org/wiki/Copeland's_method}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords Copeland
copeland <- R6Class(
  "copeland",
  public = list(
    #' @field pm performance matrix of alternatives in criteria
    pm = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field CopelandScore summed results per criteria
    CopelandScore = NULL,

    #' @field result matrix with points based on pairwise comparison and 
    #'  Copeland Score with its accompanying ranking
    result = NULL,

    #' @field rank vector of asigned ranks based on Copeland's score
    rank = NULL,

    #' @description
    #' public constructor allowing the user to construct Borta count decision
    #'  analysis problem and compute it.
    #'
    #' @param pm Matrix or data frame containing the performance table. Each
    #'  row corresponds to an alternative, and each column to a criterion. only
    #'  numeric values expercted. Rows and columns are expected to be named.
    #' @param minmax value or vector of values 'min' or 'max' specifying
    #'  optimization direction for the criterium
    #'
    #' @return initialized R6 class with computed results for Borda count
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
    #' result <- copeland$new(pm, minmax)
    initialize = function(pm, minmax = "max") {
      ## Validation
      self$pm <- pm
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncol(pm))
      # end of validation

      self$compute()
      self
    },

    #' @description
    #' Computes Borda model based on parametrs specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation si required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)
      points <- self$pm
      r <- matrix(0, ncol = nalt, nrow = nalt)
      for (i in 1:ncri) {
        # assign ranks
        if (self$minmax[i] == "max") {
          points[, i] <- rank(self$pm[, i], ties.method = "min")
        } else {
          points[, i] <- rank(-self$pm[, i], ties.method = "min")
        }
      }
      for (j in 1:ncri) {
        # across criteria
        for (i in 1:nalt) {
          for (k in 1:nalt) {
            if (i == k) {
              r[i, k] <- 0
            } else if (points[i, j] > points[k, j]) {
              r[i, k] <- r[i, k] + 1
            } else if (points[i, j] == points[k, j]) {
              r[i, k] <- r[i, k] + 0.5
            } 
          }
        }
      }
      rownames(r) <- colnames(r) <- alt
      r <- as.data.frame(r)
      self$CopelandScore <- rowSums(r)
      self$rank <- rank(-self$CopelandScore, ties.method = "min")
      r$`Copeland Score` <- self$CopelandScore
      r$rank <- self$rank
      self$result <- r
    },

    #' @description
    #' summary of the Copeland method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "Copeland's Method:\n",
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