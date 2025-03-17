#' Multi-Attribute Utility Theory
#'
#' @description
#' Developed by Keeney and Raiffa in 1976. Provides a way to estimate overall
#'  utility of the alternatives characterized by the attributes (criteria).
#'
#' Method presumes independent criteria expressed in numeric form. Criteria
#'  need to be independent. Method is also compensatory.
#'
#' In first step the performance matrix (alternatives in rows, criteria in
#'  columns) is normalized using min-max normalization.
#'
#' Next marginal utility score is computed:
#'
#' \mjsdeqn{u_{ij} = \frac{exp(r_{ij}^2) - 1}{1.71}}
#'
#' where \mjseqn{r_{ij}} is normalized value of performance of alternative i
#'  in criterium j.
#'
#' Final utility score is then computer by:
#'
#' \mjsdeqn{U_i = \sum_{j = 1}^n u_{ij} \cdot w_j}
#'
#' Final ranking is derived from final utility score Ui by ordering in
#'  descending order.
#'
#' @references
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords MAUT
maut <- R6Class("maut",
  public = list(
    #' @field pm performance matrix. Criteria (in columns) are expected to be
    #'  ordered from most influential to least influential.
    pm = NULL,

    #' @field w weight of the criteria
    w = NULL,

    #' @field minmax max for benefit and min for cost criteria. Single min or
    #'  max value is also allowed in situation when all criteria share same
    #'  direction.
    minmax = NULL,

    #' @field utility_score final utility score
    utility_score = NULL,

    #' @field finalRank final ranking of the alternatives
    finalRank = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix. Criteria in columns, ordered from most to
    #'  least influential
    #' @param w weight of the criteria
    #' @param minmax max for benefit and min for cost criteria. Single min or
    #'  max value is also allowed in situation when all criteria share same
    #'  direction.
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' alternatives <- c("A1", "A2", "A3", "A4")
    #' criteria <- c("C1", "C2", "C3", "C4")
    #' pm <- rbind(
    #'   c(429, 0.6, 5, 4),
    #'   c(649, 0.7, 4, 5),
    #'   c(459, 0.4, 1, 1),
    #'   c(419, 0.5, 2, 2)
    #' )
    #' rownames(pm) <- alternatives
    #' colnames(pm) <- criteria
    #' w <-c(0.345, 0.35, 0.155, 0.15)
    #' minmax <- c("min", "min", "max", "max")
    #' t <- maut$new(pm, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      # validation of the parameters
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of validation

      self$pm <- pm
      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' computes MAUT model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)
      cri <- colnames(self$pm)
      pm_norm <- self$pm
      for (i in 1:ncri) { # normalize performance matrix
        pm_norm[, i] <- mcda_norm(self$pm[, i], self$minmax[i], "minmax")
      }
      u <- pm
      for (i in 1:nalt) {
        for (j in 1:ncri) {
          u[i, j] <- (exp(pm_norm[i, j]^2) - 1) / 1.71
        }
      }
      ui <- sweep(u, 2, self$w, "*")
      ui <- rowSums(ui)
      self$utility_score <- ui
      self$finalRank <- rank(-self$utility_score, ties.method = "max")
    },

    #' @description
    #' prepares summary of the MAUT method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("MAUT results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n\nFinal utility:\n"))
      print(self$utility_score, pretty = TRUE)
      cat(paste("\nFinal ranking:\n"))
      print(self$finalRank, pretty = TRUE)
    }
  )
)