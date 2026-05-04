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
#' \mjsdeqn{h_{ik} = (E_i - E_k) + \psi \times (T_i - T_k)}
#'
#' Where psi is 1 if difference of eucledian distances is greater, than set
#'  threshold: |Ei - Ek| >= tau, otherwise the psi = 0
#'
#' Finaly we calculate assessment score, which is a measure that can be used
#'  directly to establish ranking of the alternatives. (Highest value 'best').
#'
#' \mjsdeqn{H_i = \sum_{k=1}^n h_{ik}}
#'
#' @references
#' GHORABAEE, M.K. et al. A NEW COMBINATIVE DISTANCE-BASED ASSESSMENT (CODAS)
#' METHOD FOR MULTI - CRITERIA DECISION - MAKING. Economic Computation and
#'  Economic Cybernetics Studies and Research, Vol. 50, no. 3, pp. 25-44.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords CODAS
codas <- R6Class(
  "codas",
  public = list(
    #' @field pm_orig original (unnormalized) performance matrix of
    #'  alternatives (in rows) in criteria (columns)
    pm_orig = NULL,

    #' @field pm normalized performance matrix
    pm = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field tau coefficient of determination
    tau = NULL,

    #' @field score assesment score (best = highest)
    score = NULL,

    #' @field score_sorted assesment score sorted from best to worst
    score_sorted = NULL,

    #'@field result databrame with score and rank
    result = NULL,

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
    #' @param tau parameter of threshold, set to 0.02 by default
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' # adjusted from reference article
    #' pm <- rbind(
    #'  c(60, 0.4, 2540, 500, 990),
    #'  c(6.35, 0.15, 1016, 3000, 1041),
    #'  c(6.8, 0.10, 1727.2, 1500, 1676),
    #'  c(10, 0.2, 1000, 2000, 965),
    #'  c(2.5, 0.10, 560, 500, 915),
    #'  c(4.5, 0.08, 1016, 350, 508),
    #'  c(3, 0.1, 1778, 1000, 920)
    #' )
    #' colnames(pm) <- c("Load cap.", "Max. tip speed", 
    #'   "Repeatability", "Memory capacity", "Manipulator reach")
    #' rownames(pm) <- c(paste0("A", 1:7))
    #' w <- c(0.036, 0.192, 0.326, 0.326, 0.120)
    #' minmax <- c("max", "min", "max", "max", "max")
    #' t <- codas$new(pm, w, minmax = minmax, tau = 0.02)
    #' summary(t)
    initialize = function(pm, w, minmax = "max", tau = 0.02) {
      # validation of the parameters
      ncri <- ncol(pm)
      self$pm_orig <- pm
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_w_sum_eq_1(w)
      validation$validate_scalar_numeric(tau, "coefficient of determination")
      if (tau > 0.05 || tau < 0.01) warning("Value of tau is usualy in <0.01; 0.05>")
      # end of validation

      self$w <- w
      self$tau <- tau
      self$compute()
      self
    },

    #' @description
    #' computes CODAS model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm_orig)
      nalt <- nrow(self$pm_orig)
      alt <- rownames(self$pm_orig)
      wpm_ns <- w_pm <- self$pm <- self$pm_orig
      for (i in 1:ncri) {
        self$pm[, i] <- mcda_norm(self$pm_orig[, i], self$minmax[i], "tobest")
        w_pm[, i] <- self$pm[, i] * self$w[i]
      }
      ns <- apply(w_pm, 2, min) # validováno
      for (i in 1:ncri) {
        wpm_ns[, i] <- w_pm[, i] - ns[i]
      }
      ei <- sqrt(rowSums(wpm_ns^2))
      ti <- rowSums(abs(wpm_ns))
      diff_ei <- outer(ei, ei, "-")
      diff_ti <- outer(ti, ti, "-")
      psi <- abs(diff_ei) >= self$tau
      hik <- diff_ei + psi * diff_ti
      self$score <- rowSums(hik)
      self$score_sorted <- sort(self$score, decreasing = TRUE)

      self$result <- data.frame(
        self$score,
        rank(-self$score)
      )
      colnames(self$result) <- c("score", "rank")
      rownames(self$result) <- alt
    },

    #' @description
    #' prepares summary of the CODAS method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste(
        "CODAS results:\nProcessed ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria\n\nResults:\n"
      ))
      print(self$result, pretty = TRUE)
    }
  )
)