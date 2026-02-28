#' Borda voting system (AKA Borda count/order of merit)
#'
#' @description
#' Decision making method derived from voting system developed by
#'  Jean-Charles de Borda in 1770. The method assigns the points based on the
#'  rank.
#'
#' On ballot last candidate gets 1 point, 2nd worse 2 points, ... Points for
#'  candidates are then aggregated across all balots, which gives us results
#'  for the elections.
#'
#' In context of multi-criteria decision making the approach can be applied by
#'  assigning points based on ranking performance of the alternatives in the
#'  criteria and then suming the points for alternative across the criteria
#'  leading to measure directly usable to rank the alternatives.
#'
#' @references
#' Borda Count. Wikipedia. available from \url{https://en.wikipedia.org/wiki/Borda_count}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords Borta
borda <- R6Class(
  "borda",
  public = list(
    #' @field pm performance matrix of alternatives in criteria 
    pm = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field points points based on rank of the criteria in the alternatives
    points = NULL,

    #' @field result matrix with points and assigned ranks for the alternatives
    result = NULL,

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
    #' result <- borda$new(pm, minmax)
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
      for (i in 1:ncri) {
        if (self$minmax[i] == "max") {
          points[, i] <- rank(self$pm[, i], ties.method = "min")
        } else {
          points[, i] <- rank(-self$pm[, i], ties.method = "min")
        }
      }
      self$points <- points
      point <- rowSums(points)
      rank <- rank(-point, ties.method = "min")
      result <- rbind(
        point,
        rank
      )
      colnames(result) <- c(rownames(self$pm))
      self$result <- result
    },

    #' @description
    #' summary of the Borda count method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "Borda count:\n",
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