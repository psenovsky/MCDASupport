#' Stable Preference Ordering Super Intelligence
#'
#' @description
#' Relatively new approach to computation of preference ordering. Its novelty
#'   lies in ability of the method to prevent rank reversal - a problem to
#'   which almost all MCDA methods are subject.
#'
#' Similarly to other MCDA methods, SPOTIS starts with Altives performance
#'   estimation in the criteria.Also similartly method requires criteria
#'   weights to be specified.
#'
#' Unlike other MCDA methods, SPOTIS requires Specification of bounds of the
#'  criteria. These bounds are established apriori (as part of decision problem
#'  specification) and are used to compute the ideal solution point (ISP) to
#'  which the criteria are compared to.
#'
#' Idea is, that there cannot be another variant (alternative), whose
#'  performance will be outside of thse bounds. Introducing such variant then
#'  cannot change used scale for the criteria, which means that the performance
#'  of the original alternatives will remain unchanges, thus preventing rank
#'  reversal.
#'
#' Approach:
#' \itemize{
#'  \item{specify performance matrix in natural units (PM), weights,
#' optimization direction of the criteria and their bounds}
#'  \item{compute ideal solution point as max or min of the bounds, depending
#' on optimization direction}
#'  \item{compute distance between the alternatives and ideal solution point}
#'  \item{apply weight}
#'  \item{compute average distace}
#'  \item{order alternatives by the average distance}
#' }
#'
#' @references
#' Dezert et al. The SPOTIS Rank Reversal Free Method for Multi-Criteria
#'   Decisionmaking Support. In: IEEE 23rd International Conferencew on
#'   Information Fusion (FUSION). Rustenburg, South Africa, 2020, pp. 1-8,
#'   doi: 10.23919/FUSION45008.2020.9190347
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords SPOTIS reversal
spotis <- R6Class("spotis",
  public = list(
    #' @field pm performance matrix of alternatives in criteria
    pm = NULL,

    #' @field w vector of weights
    w = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field bounds dataframe of bounds of criteria (criteria in rows with
    #'   min and max columns)
    bounds = NULL,

    #' @field score_raw vector of average distance to ideal solution
    score_raw = NULL,

    #' @field score vector of average distance to ideal solution ordered
    score = NULL,

    #' @description
    #' public constructor allowing the user to construct SPOTIS decision
    #'  analysis problem and compute it.
    #' @param pm performance matrix of alternatives in criteria
    #' @param w vector of weights
    #' @param minmax direction (max or min) of criteria optimization
    #' @param bounds dataframe of bounds of criteria (criteria in rows with
    #'   min and max columns)
    #'
    #' @return initialized R6 class with computed results for SPOTIS
    #'
    #' @examples
    #' # Example from the book (see references)
    #' PM <- rbind(
    #'    c(10.5, -3.1, 1.7),
    #'    c(-4.7, 0, 3.4),
    #'    c(8.1, 0.3, 1.3),
    #'    c(3.2, 7.3, -5.3)
    #' )
    #' rownames(PM) <- c('A1', 'A2', 'A3', 'A4')
    #' colnames(PM) <- c('C1', 'C2', 'C3')
    #' minmax <- c('max', 'min', 'max')
    #' w <- c(0.2, 0.3, 0.5)
    #' bounds <- rbind(
    #'    c(-5, 12),
    #'    c(-6, 10),
    #'    c(-8, 5)
    #' )
    #' rownames(bounds) <- colnames(PM)
    #' colnames(bounds) <- c('min', 'max')
    #' result <- SPOTIS(PM, w, minmax, bounds)
    initialize = function(pm, w, minmax = "max", bounds) {
      ## check validity of the objects manipulated by the current function
      self$pm <- pm
      validation$validate_matrix_numeric(pm)
      ncri <- ncol(pm) # no. of criteria
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_matrix_numeric(bounds)
      if (nrow(bounds) != ncri || ncol(bounds) != 2) {
        stop("Dimensions of the matrix 'bounds' are not correct.")
      }
      ## End of checking the validity of the "inputs"

      self$w <- w / sum(w)
      self$bounds <- bounds
      self$compute()
      self
    },

    #' @description
    #' Computes SPOTIS model based on parametrs specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation si required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm) # no. of criteria
      alt <- rownames(self$pm)
      cri <- colnames(self$pm)
      isp <- rep(0, times = ncri) # ideal solution point
      isp <- ifelse(self$minmax == "max", self$bounds[, 2], self$bounds[, 1])
      # normalized distance matrix
      dij <- matrix(0, nrow = nalt, ncol = ncri)
      rownames(dij) <- alt
      colnames(dij) <- cri
      for (j in 1:ncri) {
        for (i in 1:nalt) {
          dij[i, j] <- (abs(self$pm[i, j] - isp[j])) /
            abs(self$bounds[j, 2] - self$bounds[j, 1])
        }
      }
      d <- as.data.frame(sweep(dij, 2, self$w, "*"))
      self$score_raw <- rowSums(d)
      self$score <- self$score_raw[order(self$score_raw)]
    },

    #' @description
    #' prepares summary of the SPOTIS method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "SPOTIS:\n", "processed ", nalt,
        " alternatives in ", ncri, " criteria.\nResults\n"
      ))
      print(self$score, pretty = TRUE)
    }
  )
)
