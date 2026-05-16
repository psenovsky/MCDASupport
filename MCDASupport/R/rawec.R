#' Ranking of Alternatives with Weights of Criterion
#'
#' @description
#' Proposed by Puška et al. (2024). The procedure is as follows. First we
#'  formulate performance matrix. Next we double normalize it. For benefit
#'  criteria:
#'
#' \mjsdeqn{n_{ij} =\frac{x_{ij}}{x_{jmax}}}
#'
#' \mjsdeqn{n_{ij}^{'} =\frac{x_{jmin}}{x_{ij}}}
#'
#' For cost criteria:
#'
#' \mjsdeqn{n_{ij} =\frac{x_{jmin}}{x_{ij}}}
#'
#' \mjsdeqn{n_{ij}^{'} =\frac{x_{ij}}{x_{jmax}}}
#'
#' In other words we transfor all criteria to benefit criteria (nij) and cost
#'  criteria (n'ij). This is unique feature of RAWEC method.
#'
#' In step 3 deviations from criterion weight are computed:
#'
#' \mjsdeqn{v_{ij} =\sum_{j=1}^n w_j(1-n_{ij})}
#'
#' \mjsdeqn{v_{ij}^{'} =\sum_{j=1}^n w_j(1-n_{ij}^{'})}
#'
#' Finaly we calculate value of RAWEC Qi by:
#'
#' \mjsdeqn{Q_i =\frac{v_{ij}^{'} - v_{ij}}{v_{ij}^{'} + v_{ij}}}
#'
#' The higher the better. This value can be translated into the ranks for
#'  alternatives.
#'
#' @references
#' Puška, A., Štilić, A., Pamučar, D., Božanić, D., & Nedeljković, M. (2024).
#'  Introducing a Novel multi-criteria Ranking of Alternatives with Weights of
#'  Criterion (RAWEC) model. MethodsX, 102628.
#'  https://doi.org/10.1016/j.mex.2024.102628
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords RAWEC
rawec <- R6Class(
  "rawec",
  public = list(
    #' @field pm performance matrix (alternatives in rows, criteria in columns)
    pm = NULL,

    #' @field normPM1 normalized performance matrix (to benefit)
    normPM1 = NULL,

    #' @field normPM2 normalized performance matrix (to cost)
    normPM2 = NULL,

    #' @field w weights vector
    w = NULL,

    #' @field Qi vector of RAWEC values
    Qi = NULL,

    #' @field rank vector of ranks for alternatives based on Qi
    rank = NULL,

    #' @field minmax vector of optimization directions to the criteria. Use
    #'  max/min only. Single max or min will be interpreted as direction to
    #'  optimize all criteria.
    minmax = NULL,

    #' @field result dataframe with Q1 and ranks
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
    #' res <- rawec$new(pm, w = w, minmax = minmax)
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
    #' performs computation of RAWEC model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      cri <- colnames(self$pm)
      alt <- rownames(self$pm)

      x_min <- x_max <- rep(0, times = ncri)
      normPM2 <- normPM1 <- self$pm
      for (j in 1:ncri) {
        x_max[j] <- max(self$pm[, j])
        x_min[j] <- min(self$pm[, j])
        if (self$minmax[j] == "max") {
          normPM1[, j] <- self$pm[, j] / x_max[j]
          normPM2[, j] <- x_min[j] / self$pm[, j] 
        } else {
          normPM1[, j] <- x_min[j] / self$pm[, j] 
          normPM2[, j] <- self$pm[, j] / x_max[j]
        }
      }
      # step 3: deviation from weight
      vij2 <- vij1 <- rep(0, times = nalt)
      for (i in 1:nalt) {
        for (j in 1:ncri) {
          vij1[i] <- vij1[i] + self$w[j] * (1 - normPM1[i, j])
          vij2[i] <- vij2[i] + self$w[j] * (1 - normPM2[i, j])
        }
      }
      Qi <- (vij2 - vij1) / (vij2 + vij1)
      names(Qi) <- alt
      rank <- rank(-Qi, , ties.method = "min")
      names(rank) <- alt
      
      # results
      self$Qi <- Qi
      self$rank <- rank
      self$normPM1 <- normPM1
      self$normPM2 <- normPM2
      self$result <- data.frame(
        self$Qi,
        self$rank
      )
      colnames(self$result) <- c("Qi", "rank")
      rownames(self$result) <- alt
    },

    #' @description
    #' summary of the RAWEC count method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "RAWEC:\n",
        "processed ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria\n\n Results:\n",
        ""
      ))
      print(self$result, pretty = TRUE)
      #print(self$Qi, pretty = TRUE)
      #cat("\nRaning of the alternatives:\n")
      #print(self$rank, pretty = TRUE)
    }
  )
)

