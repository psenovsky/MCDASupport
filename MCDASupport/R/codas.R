#' COmbinative Distance-based ASsessment
#'
#' @description
#' Method uses Euclidean and taxicab distances to anti-ideal (negative-ideal)
#'  solution.
#'
#' The performance of alternatives (rows) in criteria (columns) is normalized
#'  using "to best" normalization (see \link{mcda_norm}). Then the weights are
#'  applied.
#'
#' \mjsdeqn{r_{ij} = w_j n_{ij}}
#'
#' where w are weights of criteria, n are normalized values of performance
#'  matrix. We use this information to identify negative-ideal (anti-ideal)
#'  solution
#'
#' \mjsdeqn{ns_j = min_i r_{ij}}
#'
#' Next we compute Eucludean and taxicab distances of the alternatives to this
#'  negative-ideal solution.
#'
#' \mjsdeqn{E_i = \sqrt{\sum_{j=1}^m (r_{ij} - ns_j)^2}}
#'
#' \mjsdeqn{T_i = \sum_{j=1}^m | r_{ij} - ns_j |}
#'
#' Then we compute relative assesment matrix
#'
#' \mjsdeqn{h_{ik} = (E_i - E_k) + \psi(E_i - E_k) \times (T_i - T_k)}
#'
#' Finaly we calculate assessment score, which is a measure that can be used
#'  directly to establish ranking of the alternatives. (Highest value 'best').
#'
#' \mjsdeqn{H_i = \sum_{k=1}^n h_{ik}}
#'
#' @references
#' Vitarka. CODAS - Combinative Distance-based Assessment #MADM #Optimization
#'  #MaterialSelection #MCDM. Available from
#' \url{https://www.youtube.com/watch?v=d0W8wBxPkhY}
#'
#' Manshadi B.D., Mahmudi, H., Abedian, A., Manmudi, R. A novel method for
#'  materials selection in mechanical design: combination of non-linear
#'  linearization and a modified digital logic method. Mater Des 2007, 28:
#'  8-15
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords CODAS
codas <- R6Class("codas",
  public = list(
    #' @field pm_orig original (unnormalized) performance matrix of
    #'  alternatives (in rows) in criteria (columns)
    pm_orig = NULL,

    #' @field pm normalized performance matrix
    pm = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field psi coefficient of determination
    psi = NULL,

    #' @field score assesment score (best = highest)
    score = NULL,

    #' @field score_sorted assesment score sorted from best to worst
    score_sorted = NULL,

    #' @field minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria.
    minmax = NULL,
    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix (alternatives in rows, criteria in columns).
    #' @param w weight vector (sum of weights = 1)
    #' @param minmax vector of optimization direction for criteria (min/max).
    #'  Can use single min/max if optimization direction of all criteria is
    #'  same.
    #' @param psi coefficient of determination, set to 0.02 by default
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' # from: https://www.youtube.com/watch?v=VetfNIHLyn0
    #' alternatives <- c("AI 2024-T6", "AI 5052-O", "SS 301 FH", "SS 310-3AH",
    #'  "Ti-6AI-4V", "Inconel 718", "70Cu-30Zn")
    #' criteria <- c("Toughness index", "Yield Strength", "Young's Modulus",
    #'  "Density", "Thermal expansion", "Thermal conductivity", "Specific Heat")
    #' pm <- cbind(
    #'  c(75.5, 95, 770, 187, 179, 239, 273),
    #'  c(420, 91, 1365, 1120, 875, 1190, 200),
    #'  c(74.2, 70, 189, 210, 112, 217, 112),
    #'  c(2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53),
    #'  c(21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9),
    #'  c(0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29),
    #'  c(0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06)
    #' )
    #' rownames(pm) <- alternatives
    #' colnames(pm) <- criteria
    #' w <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
    #' minmax <- c("max", "max", "max", "min", "min", "min", "min")
    #' t <- codas$new(pm, w, minmax)
    initialize = function(pm, w, minmax = "max", psi = 0.02) {
      # validation of the parameters
      ncri <- ncol(pm)
      self$pm_orig <- pm
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_w_sum_eq_1(w)
      validation$validate_scalar_numeric(psi, "coefficient of determination")
      # end of validation

      self$w <- w
      self$psi <- psi
      self$compute()
      self
    },

    #' @description
    #' computes CODAS model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm_orig)
      nalt <- ncol(self$pm_orig)
      alt <- rownames(self$pm_orig)
      self$pm <- self$pm_orig
      for (i in 1:ncri) {
        self$pm[, i] <- mcda_norm(self$pm_orig[, i], self$minmax[i], "tobest")
      }
      w_pm <- as.data.frame(sweep(self$pm, 2, self$w, "*"))
      ns <- apply(w_pm, 2, min)
      wpm_ns <- sweep(w_pm, 2, ns, "-")
      ei <- sqrt(rowSums(wpm_ns^2))
      ti <- rowSums(abs(wpm_ns))
      psi <- 0.02
      hik <- matrix(0, nrow = nalt, ncol = nalt)
      rownames(hik) <- colnames(hik) <- alt
      for (i in 1:nalt) {
        for (j in 1:nalt) {
          hik[i, j] <- (ei[i] - ei[j]) +
            (psi * (ei[i] - ei[j]) * (ti[i] - ti[j]))
        }
      }
      self$score <- rowSums(hik)
      self$score_sorted <- sort(self$score, decreasing = TRUE)
    },

    #' @description
    #' prepares summary of the CODAS method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("CODAS results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n"))
      print(self$score_sorted, pretty = TRUE)
    }
  )
)