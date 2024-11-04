#' METthod based on Removal Effects of Criteria
#'
#' @description
#' simple method for weight estimation. Works in six steps. Start with (1)
#'  assembling performance matrix with alternatives in rows and criteria in
#'  columns.
#'
#' Next we need to normalize the PM (2). For benefit criteria we use:
#'
#' \mjsdeqn{n_{ij} = \frac{min_k x_{kj}}{x_{ij}}}
#'
#' for cost criteria
#'
#' \mjsdeqn{n_{ij} = \frac{x_{ij}}{max_k x_{kj}}}
#'
#' Then we compute overal performance of overal performance of the alternatives
#'  Si (step 3)
#'
#' \mjsdeqn{S_i = ln(1 + (\frac{1}{m} \sum_j |ln(n_{ij})|)}
#'
#' In step 4 the calculation of performance alternatives by removig each
#'  attribute (Sij') is performed
#'
#' \mjsdeqn{S_{ij}^\prime = ln(1 + (\frac{1}{m} \sum_{k,k \ne j} |ln(n_{ik})|))}
#'
#' Then a Removal effect (Ej) is calculated (step 5).
#'
#' \mjsdeqn{E_j = \sum_j |S_{ij}^\prime - S_i|}
#'
#' Finally the final weights (wj) are coputed in step 6.
#'
#' \mjsdeqn{w_j = \frac{E_j}{\sum_k E_k}}
#'
#' @references
#' Bhangale, P. P., Agrawal, V. P., & Saha, S. K. (2004). Attribute based
#'  specification, comparison and selection of a robot. Mechanism and Machine
#'  Theory, 39(12), 1345-1366.
#'
#' Keshavarz-Ghorabaee, M., Amiri, M., Zavadskas, E. K., Turskis, Z., &
#'  Antucheviciene, J. (2021). Determination of objective weights using a new
#'  method based on the removal effects of criteria (MEREC). Symmetry, 13(4),
#'  525.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords MEREC
merec <- R6Class("merec",
  public = list(
    #' @field pm_orig original (not normalized) performance matrix
    pm_orig = NULL,

    #' @field pm normalized performance matrix
    pm = NULL,

    #' @field minmax criteria optimization direction (min or max). Can be
    #'  substituted for single min or max value if all criteria share same
    #'  optimization direction.
    minmax = NULL,

    #' @field w estimated weights
    w = NULL,

    #' @description
    #' Initializes and computes MEREC model.
    #'
    #' @param pm performance matrix (alternatives in rows, criteria in columns)
    #' @param minmax criteria optimization direction (min or max). Can be
    #'  substituted for single min or max value if all criteria share same
    #'  optimization direction.
    #'
    #' @return computed MEREC model
    #'
    #' @examples
    #' #see Bhanale et al. for the reference
    #' alternatives <- c("ASEA-IRB 60/2", "Cincinnati Milacrone T3-726",
    #'  "Cybotech V15 Electric Robot", "Hitachi America Process Robot",
    #'  "Unimation PUMA 500/600", "United States Robots Maker 110",
    #'  "Yaskawa Electric Motoman L3C")
    #' criteria <- c("Load capacity (LC) [kg]", "Repeatability (RE) [mm]",
    #'  "Maximum tip speed (MPS) [mm/s]", "Memory capacity (MC)",
    #'  "Manipulator reach (MR) [mm]")
    #' pm <- data.frame(
    #'  c(60, 6.35, 6.8, 10, 2.5, 4.5, 3),
    #'  c(0.4, 0.15, 0.1, 0.2, 0.1, 0.08, 0.1),
    #'  c(2540, 1016, 1727.2, 1000, 560, 1016, 177),
    #'  c(500, 3000, 1500, 2000, 500, 350, 1000),
    #'  c(990, 1041, 1676, 965, 915, 508, 920)
    #' )
    #' colnames(pm) <- criteria
    #' rownames(pm) <- alternatives
    #' minmax <- c("max", "min", "max", "max", "max")
    #' t <- merec$new(pm, minmax)
    initialize = function(pm, minmax = "max") {
      # validate inputs
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of validation

      self$pm_orig <- pm
      self$compute()
      self
    },

    #' @description
    #' Compute the MEREC using params provided in constructor. Usually run
    #'  automatically by constructor.
    compute = function() {
      ncri <- ncol(self$pm_orig)
      self$pm <- self$pm_orig
      c_max <- apply(self$pm_orig, 2, max)
      c_min <- apply(self$pm_orig, 2, min)
      for (i in 1:ncri) {
        if (self$minmax[i] == "max") {
          self$pm[, i] <- c_min[i] / self$pm_orig[, i]
        } else {
          self$pm[, i] <- self$pm_orig[, i] / c_max[i]
        }
      }
      ln_pm <- abs(log(self$pm))
      si <- log(1 + ((1 / ncri) * rowSums(ln_pm)))
      sij <- self$pm
      for (i in 1:ncri) {
        sij[, i] <- log(1 + ((1 / ncri) * rowSums(ln_pm[, -i])))
      }
      ej <- colSums(abs(sij - si))
      self$w <- ej / sum(ej)
    },

    #' @description
    #' prepares summary of the GRA method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("MEREC method results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nweights:\n"))
      print(self$w)
    }
  )
)