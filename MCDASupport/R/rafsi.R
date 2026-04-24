#' Ranking of Alternatives Through Functional of Criterion Sub-Intervals into a Single Interval
#'
#' @description
#' As in other methods we start with performance matrix. We use it to derive
#'  ideal (ai) and antiidedeal (an) values.
#'
#' Then we map each of the elements of performance matrix into correcponding
#'  criteria intervals Cj in <an, ai> for beneficial critera and Cj in <ai, an>
#'  for cost criteria.
#'
#' We work with 2k fixed points. So worst performance in criterium will always
#'  be mapped to n1 and the best will be mapped to n2k. With n1 = 1 and n2k can
#'  be seen as how many times better is the best against the aitiideal. In is
#'  recommended for n2k to be at least 6 or more.
#'
#' The PM needs to be mapped to the intervals:
#'
#' \mjsdeqn{f_i (x) = \frac{n2k - n_1}{a_{Ij} - a_{Nj}} x + \frac{a_{Ij}n_1 - a_{Nj}n_{2k}}{a_{Ij} - a_{Nj}}}
#'
#' The result needs to be normalized. For benefit sriteria we use:
#'
#' \mjsdeqn{\hat{s_{ij}} = \frac{s_{ij}}{2A}}
#'
#' and for cost criteria:
#'
#' \mjsdeqn{\hat{s_{ij}} = \frac{H}{2s_{ij}}}
#'
#' where A is arithmetic and H is harmonic mean:
#'
#' \mjsdeqn{A = \frac{n_1 + n_{2k}}{2}}
#'
#' \mjsdeqn{H = \frac{2}{\frac{1}{n_1} + \frac{1}{n_{2k}}}}
#'
#' To the resulting matrix we apply the weights and sum across the criteria to
#'  get measure Vi, which we sort in descending maner.
#'
#' @references
#' Shankar Chakraborty, Prasenjit Chatterjee, Partha Protim Das. Ranking of
#'  Alternatives through Functional Mapping of Criterion Sub-Intervals into
#'  a Single Interval (RAFSI) Method. In Multi-Criteria Decision-Making
#'  Methods in Manufacturing Environments. Apple Academic Press: New York, 2023,
#'  pp. 317-323, ISBN 9781003377030.
rafsi <- R6Class(
  "rafsi",
  public = list(
    #' @field pm performance matrix
    pm = NULL,

    #' @field w vector of weights
    w = NULL,

    #' @field minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    minmax = NULL,

    #' @field n2k number of intervals, recommended to be 6 or more (integer)
    n2k = 6,

    #' @field result results dataframe with value and ranking of the alternatives
    result = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm normalized performance matrix
    #' @param w vector of weights, its sum must be equal to 1
    #' @param minmax minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    #' @param n2k number of intervals, recommended to be 6 or more (integer)
    #'
    #' @examples
    #' pm <- cbind(
    initialize = function(pm, w, minmax = "max", n2k = 6) {
      # validity check
      ncri <- ncol(pm)
      self$pm <- pm
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      validation$validate_w_sum_eq_1(w)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      if (n2k < 6) {
        stop("n2k should be 6 or more.")
      }
      # end of validaty check

      self$w <- w
      self$n2k <- n2k
      self$compute()
      self
    },

    #' @description
    #' performs computation of RAFSI model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$pm)
      ai <- an <- rep(0, times = ncri)
      pm3 <- pm2 <- self$pm
      A <- (1 + self$n2k) / 2 # arithmetic mean
      H <- 2 / (1 + 1 / self$n2k) # harmonic mean
      for (i in 1:ncri) {
        if (self$minmax[i] == "max") {
          ai[i] <- private$round_up(max(pm[, i]), 1)
          an[i] <- private$round_down(min(pm[, i]), 1)
        } else {
          ai[i] <- private$round_down(min(pm[, i]), 1)
          an[i] <- private$round_up(max(pm[, i]), 1)
        }
        # mapping function
        pm2[, i] <- self$pm[, i] *
          (self$n2k - 1) /
          (ai[i] - an[i]) +
          (ai[i] - an[i] * self$n2k) / (ai[i] - an[i])
        # normalize
        if (self$minmax[i] == "max") {
          pm3[, i] <- pm2[, i] / (2 * A)
        } else {
          pm3[, i] <- H / (2 * pm2[, i])
        }
      }
      # apply weight
      weighted_pm <- as.data.frame(sweep(pm3, 2, self$w, "*"))
      weighted_sum <- rowSums(weighted_pm)
      result <- cbind(
        rownames(self$pm),
        weighted_sum,
        rank(-weighted_sum)
      )
      colnames(result) <- c("alternatives", "values", "rank")
      self$result <- result
    },

    #' @description
    #' prepares summary of the RAFSI method resutls and outputs them
    #'  to the console.
    summary = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      cat(paste(
        "RAFSI:\nProcessed, ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria. ",
        self$n2k,
        " intervals have been used.\n"
      ))
      print(self$result, pretty = TRUE)
    }
  ),
  # round up to 1 decimal
  private = list(
    round_up = function(x, digits = 0) {
      shift <- 10^digits
      t <- ceiling(x * shift) / shift
      return(t)
    },
    # round down to 1 decimal
    round_down = function(x, digits = 0) {
      shift <- 10^digits
      t <- floor(x * shift) / shift
      return(t)
    }
  )
)