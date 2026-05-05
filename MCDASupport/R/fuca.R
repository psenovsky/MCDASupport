#' Faire Un Choix Adégnat
#'
#' @description
#' translates to "make adequate choice". Method proposed by Fernando et al
#'  (2011). The best alternative is determined based on ranks aggregation.
#'
#' First we assign ranks to performance of the alternatives in criteria and we
#'  apply weights on resulting matrix of these ranks.
#'
#' Alternative with smallest Ri is top ranked.
#'
#' \mjsdeqn{R_i = \sum_{j=1}^n (r_{ij} \cdot w_j)}
#'
#' where rij is ranked performance of alternative in criterium.
#'
#' @references
#' MENDOZA LUIS FERNANDO, Morales et al. Selecting the best portfolio
#'  alternative from a hybrid multiobjective GA-MCDM approach for New
#'  Product Development in the pharmaceutical industry. In: 2011 IEEE
#'  Symposium on Computational Intelligence in Multicriteria
#'  Decision-Making (MDCM). 2011, pp. 159–166, available from:
#'  https://doi.org/10.1109/SMDCM.2011.5949271.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords FUCA
fuca <- R6Class(
  "fuca",
  public = list(
    #' @field pm performance matrix (alternatives in rows, criteria in columns)
    pm = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field minmax vector of optimization direction for criteria. Use
    #'  min/max. If sing min or max is used, function will presume same
    #'  direction for all criteria
    minmax = NULL,

    #' @field rankPM performance matrix expresed in form of ranks
    rankPM = NULL,

    #' @field Ri vector of agreggated weighted ranks
    Ri = NULL,

    #' @field rank vector of alternative ranks derived from Ri
    rank = NULL,

    #' @field result data frame with values of Ri and rank
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
    #' res <- fuca$new(pm, w = w, minmax = minmax)
    initialize = function(pm, w, minmax = "max") {
      # validity check
      ncri <- ncol(pm)
      self$pm <- pm
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      validation$validate_w_sum_eq_1(w)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of validaty check

      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' performs computation of FUCA model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$pm)
      alt <- rownames(self$pm)
      cri <- colnames(self$pm)
      rankPM <- self$pm
      for (j in 1:ncri) {
        if (self$minmax[j] == "max") {
          rankPM[, j] <- rank(-self$pm[, j], ties.method = "min")
        } else {
          rankPM[, j] <- rank(self$pm[, j], ties.method = "min")
        }
      }
      R <- sweep(rankPM, MARGIN = 2, self$w, `*`)
      colnames(R) <- cri
      rownames(R) <- alt
      Ri <- rowSums(R)
      names(Ri) <- alt
      rank <- rank(Ri, ties.method = "min")

      # prepare output
      self$rank <- rank
      self$Ri <- Ri
      self$rankPM <- rankPM
      self$result <- data.frame(Ri, rank)
      rownames(self$result) <- rownames(self$pm)
    },

    #' @description
    #' summary of the FUCA count method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "FUCA:\n",
        "processed ", nalt, " alternatives in ", ncri, " criteria\n\nResults:\n"
      ))
      print(self$result, pretty = TRUE)
    }
  )
)