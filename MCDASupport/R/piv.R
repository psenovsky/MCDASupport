#' Proximity Indexed Value (PIV)
#'
#' @description
#' Introduced by Mufazzel and Muzakkin (2018) to mitigate rank reversal
#'  problem. The procedure takes ideal solution and evaluates distance of the
#'  alternatives to it.
#'
#' In comparison TOPSIS evaluates both ideal and intiideal solution.
#'
#' This simplification introduces some limitations for the results (compared
#'  to TOPSIS). Ignoring closeness to antiideal solution is not advised in
#'  decision situations with risk considerations. Succesible to inflation of
#'  extreme values to name few.
#'
#' The procedure starts with 1) normalization of performance matrix using vector
#'  normalization. 2) Apply weights. 3) determine ideal solution A* and
#'  4) calculate overall proximity value of Di:
#'
#' \mjsdeqn{D_i = \sum_{j=1}^n |v_{ij} - v_j^+ |}
#'
#' and use it to rank the alternatives.
#' 
#' Note: since PIV is so close to TOPSIS this class just computes TOPSIS and
#'  publishes parts relevant to PIV.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords PIV
piv <- R6Class(
  "piv",
  public = list(
    #' @field pm_orig original (not transformed) performance matrix of
    #'  alternatives in criteria
    pm_orig = NULL,

    #' @field pm performance matrix of alternatives in criteria (all criteria
    #'  are maximized)
    pm = NULL,

    #' @field w vector of weights
    w = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field normPM normalized performance matrix
    normPM = NULL,

    #' @field weightPM weighted normalized performance matrix
    weightPM = NULL,

    #' @field A_ideal positive ideal solution
    A_ideal = NULL,

    #' @field D_ideal alternative closeness to ideal variant
    D_ideal = NULL,

    #' @field ranks ranks derived from distance to ideal solution (D_ideal) measure
    ranks = NULL,

    #' @description
    #' Public constructor for TOPSIS object. Performas validation of input
    #'  parameters and uses them to compute the model.
    #'
    #' @param pm Matrix or data frame containing the performance table. Each
    #'  row corresponds to an alternative, and each column to a criterion.
    #'  Only numeric values expercted. Rows and columns are expected to be
    #'  named.
    #' @param w vector containing the weights of the criteria.
    #' @param minmax criteria MinMax Vector containing the preference direction
    #'  on each of the criteria. "min" (resp."max") indicates that the
    #'  criterion has to be minimized (maximized). Default value: max
    #'
    #' @return
    #' Instance of topsis class uncluding inputs and model results in class's
    #'  properties
    #'
    #' @examples
    #' PM <- cbind(
    #'    c(8,7,2,1),
    #'    c(5,3,7,5),
    #'    c(7,5,6,4),
    #'    c(9,9,7,3),
    #'    c(11,10,3,7),
    #'    c(6,9,5,4)
    #' )
    #' colnames(PM) <- c('Site 1', 'Site 2', 'Site 3', 'Site 4', 'Site 5',
    #'    'Site 6')
    #' rownames(PM) <- c('Investment costs (million EUR)',
    #'                   'Employment needs (hundred employees)',
    #'                   'Social impact (1-7)',
    #'                   'Environmental impact (1-7)')
    #' PM <- t(PM)
    #' minmax <- 'max'
    #' w <- c(0.4, 0.4, 0.1, 0.2)
    #' result <- piv$new(PM, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      t <- topsis$new(pm, w, minmax)
      ranks <- rank(t$D_ideal, ties = "min")

      self$pm <- as.data.frame(pm)
      self$w <- w
      self$minmax <- minmax
      self$normPM <- t$normPM
      self$weightPM <- t$weightPM
      self$A_ideal <- t$A_ideal
      self$D_ideal <- t$D_ideal
      self$ranks <- ranks
    },

    #' @description
    #' Creates summary for PIV model and sends it to console.
    summary = function() {
      nalt <- nrow(self$pm)
      cat(paste0(
        "PIV\nprocessed ",
        nalt,
        " alternatives in ",
        length(self$w),
        " criteria\n\nA ideal\n"
      ))
      print(self$A_ideal, pretty = TRUE)
      cat(paste("\nD (alternative) ideal\n"))
      print(self$D_ideal, pretty = TRUE)
      cat(paste("\n\nRanked alternatives\n"))
      print(self$ranks, pretty = TRUE)
    }
  )
)