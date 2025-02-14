#' Simple Multi-Attribute Rating Technique
#'
#' @description
#' Simple approach to evaluate utility of the alternatives characterized by
#'  mixed criteria (expressed either numerically or using ordinal values).
#'
#' For ordinal values, the criteria are expected to use following scale:
#' \itemize{
#'  \item{poor: 4}
#'  \item{fairly weak: 5}
#'  \item{medium: 6}
#'  \item{fairly good: 7}
#'  \item{good: 8}
#'  \item{very good: 9}
#'  \item{excellent: 10}
#' }
#'
#' Qualitatively expressed criteria use their natural units for its first step.
#'  We compute:
#'
#' \mjsdeqn{v = log_2 64 \cdot \frac{P_v - P_{min}}{P_{max} - P_{min}}}
#'
#' Where Pv indicates actual value of the attribute in the criterium, Pmax and
#'  Pmin maximum and minimum on the used scale. Decision maker states these
#'  values.
#'
#' We use this information to compute effective weights for positive criteria
#'  as:
#'
#' \mjsdeqn{g_{ij} = 4 + v}
#'
#' and for negative (cost) criteria:
#'
#' \mjsdeqn{g_{ij} = 10 - v}
#'
#' As outcome both ordinal and numeric criteria will be in interval (4; 10).
#'
#' In next step the criteria weights need to be established. To do that the
#'  decision maker provides his/her insight into how much do various criteria
#'  contribute to the outcome of the decision. Information  is provided in form
#'  of vector of the criteria's ranks $h_j$ using same ordinal scale (4 - 10)
#'  as before. The weights are normalized using equation:
#'
#' \mjsdeqn{w_j = \frac{\sqrt{2}^{h_j}}{\sum_{j=1}^n \sqrt{2}^{h_j}}}
#'
#' Final ranking is derived from measure fi:
#'
#' \mjsdeqn{f_i = \sum_{j=1}^n w_j \cdot g_{ij}}
#'
#' Alternative with highest value of fi is considered best. This implementation
#'  uses \link{wsm} function to compute this final results and provide more
#'  fancy outputs.
#'
#' @references
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords SMART WSM
smart <- R6Class("smart",
  public = list(
    #' @field pm performance matrix
    pm = NULL,

    #' @field hj rank of the criteria. The decision maker assigns a value on
    #'  ordinal scale (4; 10), see general remars of the method, to express how
    #'  the criterium contributes to explanation of the decision
    hj = NULL,

    #' @field minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria. The value needs to be provided for all
    #'  numeric criteria (ordinal criteria are already considered processed).
    minmax = NULL,

    #' @field ordinal vector describing nature of the criteria. Criteria with
    #'  TRUE values are considered ordinal and with FALSE values are considered
    #'  numeric.
    ordinal = NULL,

    #' @field pmax numeric vector of maximums of the criteria
    pmax = NULL,

    #' @field pmin numeric vector of minimums of the criteria
    pmin = NULL,

    #' @field w normalized weights of the criteria derived from hj
    w = NULL,

    #' @field result_table has weighted performance matrix with added columns
    #'  to summ performance and extress this sum as a percentage of the best
    #'  alternative
    result_table = NULL,

    #' @field weighted_sum_prc vector specifying how close the alternatives are
    #'  to the best aleternative (expresed as the percentage of best) sorted
    #'  descending (from best to worst)
    weighted_sum_prc = NULL,

    #' @field scoreM results of the WSM presented as stacked bar chart. The
    #'  graph clearly shows contribution of the criteria to overall performance
    #'  of the alternative.
    scoreM = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix
    #' @param hj rank of the criteria. The decision maker assigns a value on
    #'  ordinal scale (4; 10), see general remars of the method, to express how
    #'  the criterium contributes to explanation of the decision
    #' @param minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria. The value needs to be provided for all
    #'  numeric criteria (ordinal criteria are already considered processed).
    #' @param ordinal vector describing nature of the criteria. Criteria with
    #'  TRUE values are considered ordinal and with FALSE values are considered
    #'  numeric.
    #' @param pmax numeric vector of maximums of the criteria
    #' @param pmin numeric vector of minimums of the criteria
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' alternatives <- c("A1", "A2", "A3")
    #' criteria <- c("consumer price", "max. speed", "acceleration to 100", "trunk volume")
    #' pm <- rbind(
    #'   c(25000, 153, 15.3, 250),
    #'   c(33000, 177, 12.3, 380),
    #'   c(40000, 199, 11.1, 480)
    #' )
    #' rownames(pm) <- alternatives
    #' colnames(pm) <- criteria
    #' minmax <- c("min", "max", "min", "max")
    #' ordinal <- c(FALSE, FALSE, FALSE, FALSE)
    #' hj <- c(9, 5, 7, 6)
    #' t <- smart(pm, hj, minmax, ordinal)
    initialize = function(pm, hj, minmax = "max", ordinal, pmax, pmin) {
      # validation of the parameters
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      self$pm <- pm
      if (!all(minmax %in% c("min", "max", NULL))) {
        stop("Values in minmax vector expected to be min, max or NULL only.")
      } else if (length(minmax == 1)) {
        self$minmax <- rep(minmax, times = ncri)
      }
      if (!all(ordinal %in% c(TRUE, FALSE))) {
        stop("Values in ordinal vector expected to be TRUE or FALSE only.")
      }
      validation$validate_no_elements_vs_cri(hj, ncri, "criteria rank", TRUE)
      validation$validate_no_elements_vs_cri(ordinal, ncri, "ordinal/numeric vector")
      validation$validate_no_elements_vs_cri(pmax, ncri, "maxima in criteria", TRUE)
      validation$validate_no_elements_vs_cri(pmin, ncri, "minima in criteria", TRUE)
      scale <- 4:10
      if (!all(hj %in% scale)) {
        stop("criteria rank must contain integer in interval (4;10).")
      }
      if (!all(pmin < pmax)) stop("Some values in Pmin are bigger than Pmax!")
      for (i in 1:ncri) {
        if (ordinal[i]) {
          if (!all(pm[, i] %in% scale)) stop("ordinal criteria must contain integer values in interval (4;10).")
        } else {
          validation$validation_vector_in_interval(pm[, i], pmin[i], pmax[i], "Performance of numeric criteri must be between pmin and pmax")
        }
      }
      # end of validation

      self$ordinal <- ordinal
      self$hj <- hj
      self$pmax <- pmax
      self$pmin <- pmin
      self$compute()
      self
    },

    #' @description
    #' computes SMART model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      pm2 <- self$pm
      ncri <- ncol(pm2)
      for (i in which(!self$ordinal)) {
        v <- log2(64 * (self$pm[, i] - self$pmin[i]) /
                    (self$pmax[i] - self$pmin[i]))
        if (self$minmax[i] == "max") {
          pm2[, i] <- 4 + v
        } else {
          pm2[, i] <- 10 - v
        }
      }
      w1 <- sqrt(2)^self$hj
      self$w <- w1 / sum(w1)

      t <- wsm$new(pm2, self$w)
      self$result_table <- t$result_table
      self$weighted_sum_prc <- t$weighted_sum_prc
      self$scoreM <- t$scoreM
    },

    #' @description
    #' prepares summary of the CODAS method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("SMAR results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n"))
      print(self$result_table, pretty = TRUE)
      cat(paste("\nClosenes to best alternative as % of best\n"))
      print(self$weighted_sum_prc, pretty = TRUE)
      print(self$scoreM)
    }
  )
)