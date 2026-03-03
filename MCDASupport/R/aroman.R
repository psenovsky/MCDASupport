#' Alternative Ranking Order Method Accounting for Two-Step Normalization
#'
#' @description
#' The approach uses linear (tij) and vector (tij*) normalization of
#'  performance measures. Based of these averaged normalization is computed:
#'
#' \mjsdeqn{t_{ij}^{norm} = \frac{\beta t_{ij} + (1 - \beta)t_{ij}^*}{2}}
#'
#' Where bata is weighting factor in interval 0-1, 0.5 by default.
#'
#' To t_norm weights are appied (t^). Next we separately aggreates for cost
#'  criteria (Li) and benefit criteria (Ai).
#'
#' As last step final ranking Ri is computed:
#'
#' \mjsdeqn{R_i = L_i^\lambda + A_i^{1 - \lambda}}
#'
#' Lambda parameter represents coefficient degree of the criteria type. It is
#'  0.5 by default, but any number 0-1 can be assigned. As rule of thumb the
#'  ratio between cost and benefit criteria is good starting point.
#'
#' @references
#' S. Bošković, L. Švadlenka, S. Jovčić, M. Dobrodolac, V. Simić and
#'  N. Bacanin. "An Alternative Ranking Order Method Accounting for Two-Step
#'  Normalization (AROMAN)—A Case Study of the Electric Vehicle Selection
#'  Problem," in IEEE Access, vol. 11, pp. 39496-39507, 2023,
#'  doi: 10.1109/ACCESS.2023.3265818.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords AROMAN
aroman <- R6Class(
  "aroman",
  public = list(
    #' @field pm performance matrix of alternatives in criteria
    pm = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field beta value 0-1 representing weighting factor between 2 types of
    #'  normalization (default = 0.5)
    beta = NULL,

    #' @field lambda values 0-1, coefficient degree of the criteria type (0.5
    #'  by default)
    lambda = NULL,

    #' @field result dataframe with cost (Li) and Bbenefit (Ai) normalized
    #'  aggregated value, final result (Ri) and rank of alternatives.
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
    #' @param beta value 0-1 representing weighting factor between 2 types of
    #'  normalization (default = 0.5)
    #' @param lambda values 0-1, coefficient degree of the criteria type (0.5
    #'  by default)
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
    #' res <- aroman$new(pm, w = w, minmax = minmax)
    initialize = function(pm, w, minmax = "max", beta = 0.5, lambda = 0.5) {
      # validity check
      ncri <- ncol(pm)
      self$pm <- pm
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      validation$validate_w_sum_eq_1(w)
      validation_env$validate_value_in_interval(beta, 0, 1, "beta value")
      validation_env$validate_value_in_interval(lambda, 0, 1, "lambda value")
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of validaty check

      self$w <- w
      self$beta <- beta
      self$lambda <- lambda
      self$compute()
      self
    },

    #' @description
    #' performs computation of AROMAN model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      cri <- colnames(self$pm)
      alt <- rownames(self$pm)

      # 2. normalization
      aggregPM <- vectorPM <- linearPM <- self$pm
      for (j in 1:ncri) {
        linearPM[, j] <- mcda_norm(
          self$pm[, j],
          minmax = self$minmax[j],
          method = "minmax"
        )
        vectorPM[, j] <- mcda_norm(
          self$pm[, j],
          minmax = self$minmax[j],
          method = "vector"
        )
        aggregPM[, j] <- (self$beta * linearPM[, j] + (1 - self$beta) * vectorPM[, j]) / 2
      }
      # 3. apply weights
      tij <- sweep(aggregPM, 2, self$w, "*")
      # 4. separately compute cost (Li) and benefit criteria
      t <- which(self$minmax == "min")
      if (length(t) > 1) {
        Li <- rowSums(tij[, t])
      } else if (length(t) == 1) {
        Li <- tij[, t]
      } else {
        Li <- rep(0, times = nalt)
      }
      t <- which(self$minmax == "max")
      if (length(t) > 1) {
        Ai <- rowSums(tij[, t])
      } else if (length(t) == 1) {
        Ai <- tij[, t]
      } else {
        Ai <- rep(0, times = nalt)
      }
      Ri <- Li^self$lambda + Ai^(1 - self$lambda)
      rank <- rank(-Ri, ties.method = "min")
      result <- data.frame(Li, Ai, Ri, rank)
      rownames(result) <- alt
      self$result <- result
    },

    #' @description
    #' summary of the AROMAN method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "AROMAN:\n",
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