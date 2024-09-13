#' method for computations of partial preference using PROMETHEE I method
#'
#' @description
#' PROMETHEE stands for Preference Ranking Organization METHod for Enrichment
#'  Evaluation. Promethee I method is intended for establishment of partial
#'  ranking of the alternatives, by evaluation of positive and negative
#'  prefferences flows in pairweise comparisons of the alternatives.
#'
#' Method uses general \link{PROMETHEE} function and computes comparison states
#'  on top of it.
#'
#' The computation starts with computing positive and negative flows using
#'  PROMETHEE function. This function is being used in PROMETHEE I, II and III
#'  methods (see \code{\link{PROMETHEE_II}}, \code{\link{PROMETHEE_III}}).
#'
#' Since the flow computation is shared among the PROMETHEE family methods, it
#'  is implemented as separate function in \code{\link{PROMETHEE}}. Refer to
#'  the function documentation for details on computation of flows.
#'
#' Flows are then used to derive some preferences in alternatives. For example
#'  if (P+(a) > P+(b) and P-(a) < P-(b)) or (P+(a) == P+(b) and P-(a) < P-(b))
#'  or (P+(a) > P+(b) and P-(a) == P-(b)) then we can say that a is preffered
#'  to b: aPb.
#'
#' If positive and negative flows are same for both alternatives, we can say,
#'  that we are indifferent (preferentially): aIb, otherwise the alternatives
#'  are incomparable: aRb.
#'
#' @references
#' ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca
#'  Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords promethee1 PROMETHEE
promethee1 <- R6Class("promethee1",
  public = list(

    #' @field pm_orig original performance matrix (as used in constructor)
    pm_orig = NULL,

    #' @field pm performance matrix with all criteria converted to maximize
    pm = NULL,

    #' @field pref_function vector of preference functions for the criteria
    pref_function = NULL,

    #' @field w weights vector
    w = NULL,

    #' @field minmax vector of min/max (set direction of optimization for
    #'  criteria)
    minmax = NULL,

    #' @field i_threshold indefference threshold vector
    i_threshold = NULL,

    #' @field p_threshold prefference threshold vector
    p_threshold = NULL,

    #' @field im_threshold itermediate threshold vector
    im_threshold = NULL,

    #' @field positiveFlow vector representing how the alternative is preffered
    #'  to other alternatives
    positiveFlow = NULL,

    #' @field negativeFlow vector representing how altenative is outranked by
    #'  other alternatives
    negativeFlow = NULL,

    #' @field preferenceDegree matrix representing aggregated weighted
    #'  preferences of the alternatives across the criteria
    preferenceDegree = NULL,

    #' @field preferenceDegreeUnw list of matrixes with unweighted preferences,
    #'  separate matrix for each criterion
    preferenceDegreeUnw = NULL,

    #' @field pairweiseComparison list of matrixes measuring nominal
    #'  differences in alternatives performance in criterions. Separate
    #'  matrixes are constructed for each criterion.
    pairweiseComparison = NULL,

    #' @field preferenceMatrix preference matrix with specified P+ (a prefered
    #'  to b), P- (b prefered to a), I (indifferent) and R (incomparable) for
    #'  every pair of alternatives
    preferenceMatrix = NULL,

    #' @description
    #' Public constructor for PROMETHEE I method. Valides the inputs and
    #'  computes the method based on them.
    #'
    #' @param pm Matrix or data frame containing the performance table. Each
    #'  row corresponds to an alternative, and each column to a criterion. only
    #'  numeric values expercted. Rows and columns are expected to be named.
    #' @param pref_function vector, specifies type of function used to compute
    #'  preferences. Need to be set for each criterion. Possible values are:
    #'  'default', 'U-shape', 'V-shape', 'level', 'linear', 'Gaussian'. Choice
    #'  of function type will decide on what type of threshold (if any) is
    #'  required for computation. Each criterion can use different preference
    #'  function.
    #' @param w vector containing the weights of the criteria. Values need to
    #'  0 <= wi <= 1, sum(wi) = 1
    #' @param minmax can be set to either value or vector. Value (min or max)
    #'  is usable in situation when all criteria are either benefit or cost
    #'  (are not mixed). If Criteria orientation is mixed, vector is required
    #'  to set criterion orientation right.
    #' @param i_threshold vector containing indifference threshods for
    #'  criteria. Not all types of performance functions require it. The
    #'  parameter must be used if there is at least one criterion, for which it
    #'  is required. Values for all other criteria should be 0 (and will not be
    #'  used during computations). Only 'U-shape', 'level', 'linear' functions
    #'  need this threshold.
    #' @param p_threshold vector containing prefference threshods for criteria.
    #'  Not all types of performance functions require it. The parameter must
    #'  be used if there is at least one criterion, for which it is required.
    #'  Values for all other criteria should be 0 (and will not be used during
    #'  computations). Only 'V-shape', 'level', 'linear' functions need this
    #'  threshold.
    #' @param im_threshold vector containing intermetiate thresholds for
    #'  criteria. only Gaussian type performance functions rewuire this type of
    #'  threshold. If prefference and indifference thresholds are present, the
    #'  PROMETHEE function will try to 'gues' intermediate threshold as value
    #'  right in the middle between these thresholds.
    #'
    #' @examples
    #' # Example from Fuzzy TOPSIS book (see references)
    #' # ammended error in tab. 4.9, the computation presumes maximization of
    #' # all criteria
    #' PM <- cbind(
    #'   c(80, 65, 83, 40, 52, 94),
    #'   c(90, 58, 60, 80, 72, 96),
    #'   c(600, 200, 400, 1000, 600, 700),
    #'   c(54, 97, 72, 75, 20, 36),
    #'   c(8, 1, 4, 7, 3, 5),
    #'   c(5, 1, 7, 10, 8, 6)
    #' )
    #' colnames(PM) <- c('C1', 'C2', 'C3', 'C4', 'C5', 'C6')
    #' rownames(PM) <- c('A1', 'A2', 'A3', 'A4', 'A5', 'A6')
    #' minmax <- 'max'
    #' shape <- c('U-shape', 'V-shape', 'linear', 'level', 'default',
    #'            'Gaussian')
    #' p <- c(10, 0, 450, 50, 0, 0) #indifference threshold
    #' q <- c(0, 30, 50, 10, 0, 0) #prefference threshold
    #' s <- c(0,0,0,0,0,5) #intermediate threshold
    #' w <- c(0.1667, 0.1667, 0.1667, 0.1667, 0.1667, 0.1665)
    #' result <- promethee1$new(PM, shape, w, minmax, q, p, s)
    initialize = function(pm, pref_function, w, minmax = "max",
                          i_threshold = NULL, p_threshold = NULL,
                          im_threshold = NULL) {
      # params consistency check centralized in generalized PROMETHEE function
      # here is only minmax evaluation
      # validate minmax and invert scales if neccessary
      self$pm_orig <- pm
      self$pm <- util_pm_minmax(pm, minmax)
      #end of parameter consistency check

      self$w <- w
      self$i_threshold <- i_threshold
      self$p_threshold <- p_threshold
      self$im_threshold <- im_threshold
      self$pref_function <- pref_function
      self$minmax <- minmax
      self$compute()
      self
    },

    #' @description
    #' computes PROMETHEE I model based on parameters set in properties of the
    #'  class. It is usually not neccessary to call this funtion manually as it
    #'  is called automatically by the class constructor.
    compute = function() {
      nalt <- nrow(self$pm)  #no. of alternatives
      alt  <- rownames(self$pm) #list of alternatives
      flow <- PROMETHEE(self$pm, self$pref_function, self$w, self$i_threshold,
                        self$p_threshold, self$im_threshold)
      pf <- flow$positiveFlow
      nf <- flow$negativeFlow

      # establish preference system
      pref <- matrix(data = 0, nrow = nalt, ncol = nalt)
      rownames(pref) <- alt
      colnames(pref) <- alt
      k <- 2
      for (i in 1:nalt) {
        for (j in k:nalt) {
          pf_i_greater_pf_j <- pf[i] > pf[j]
          pf_i_eq_pf_j <- pf[i] == pf[j]
          nf_i_less_nf_j <- nf[i] < nf[j]
          nf_i_eq_nf_j <- nf[i] == nf[j]

          if ((pf_i_greater_pf_j && (nf_i_less_nf_j || nf_i_eq_nf_j)) ||
                (pf_i_eq_pf_j && nf_i_less_nf_j)) { #aPb (a preffered to b)
            pref[i, j] <- "P+"
            pref[j, i] <- "P-"
          } else if (pf_i_eq_pf_j && nf_i_eq_nf_j) { #aIb (a indifferent to b)
            pref[i, j] <- pref[j, i] <- "I"
          } else if ((pf_i_greater_pf_j && !nf_i_less_nf_j) ||
                       (!pf_i_greater_pf_j && nf_i_less_nf_j)) {
            #aRb (a incomparable to b)
            pref[i, j] <- pref[j, i] <- "R"
          } else { # bPa (b is prefered to a)
            pref[i, j] <- "P-"
            pref[j, i] <- "P+"
          }
        }
        if (k < nalt) k <- k + 1
      }
      diag(pref) <- "I"

      self$positiveFlow <- pf
      self$negativeFlow <- nf
      self$preferenceDegree <- flow$preferenceDegree
      self$preferenceDegreeUnw <- flow$preferenceDegreeUnw
      self$pairweiseComparison <- flow$pairweiseComparison
      self$preferenceMatrix <- pref
    },

    #' @description
    #' genrates basic information about the computed model and prints it to the
    #'  console
    summary = function() {
      nalt <- nrow(self$pm)
      cat(paste0("PROMETHEE I\nprocessed ", nalt, " alternatives in ",
                 length(self$w), " criteria\nPositive flow\n"))
      print(self$positiveFlow, pretty = TRUE)
      cat(paste("\nNegative flow\n"))
      print(self$negativeFlow, pretty = TRUE)
    },

    #' @description
    #' test sensitivity of the model to changes in the thresholds.
    #'
    #' Provides sens_i (for indifference threshold), sens_p (for prefference
    #'  threshold) and sens_im (for intermediate treshold) dataframes in
    #'  structure:
    #'
    #' \itemize{
    #'   \item criterium
    #'   \item from - lower bound of sensitivity
    #'   \item default - value computed by original model
    #'   \item to - upper bound of sensitivity
    #'   \item function - type of preference function for criterium
    #' }
    #'
    #' Dataframes do have a numeric value with lower/upper bound of
    #'  sensitivity identifying last value of the threshold for which the
    #'  result still doesn't change.
    #'
    #' If no such value is identified "insens." is provided.
    #'
    #' Also note that PROMETHEE function is complex, because of existence of
    #'  "preference" functions, which are stated separately for each criterium
    #'  and each of these has a different requirements on types of thresholds
    #'  it uses. For example level and linear functions use both preference and
    #'  indifference thresholds (but not intermediate). V-shape function uses
    #'  prefference threshold only, U-shape uses indifference threshold only.
    #'
    #' Gaussian function uses intermediate threshold only, but if preference
    #'  andindifference thresholds are provided, the sensitivity is being
    #'  tested on in interval between these two.
    #'
    #' @param step number of steps to divide threshold testing interval
    #'
    #' @return dataframes sens_i, sens_p and sens_im with sensitivity limit for
    #'  the criteria
    sensitivity = function(step = 100) {
      if (!is.numeric(step) || step < 0) {
        stop("step parameter mus be positive number")
      }
      ncri <- ncol(self$pm)
      cri <- colnames(self$pm)
      sens_p <- data.frame(matrix(0, nrow = ncri, ncol = 5))
      colnames(sens_p) <- c("criterium", "from", "default", "to", "function")
      sens_i <- sens_p
      sens_im <- sens_p
      indif_types <- c("U-shape", "level", "linear")
      pref_types <- c("V-shape", "level", "linear")
      for (i in 1:ncri) {
        sens_p[i, 5] <- sens_im[i, 5] <- sens_i[i, 5] <- self$pref_function[i]
        sens_p[i, 1] <- sens_im[i, 1] <- sens_i[i, 1] <- cri[i]
        if (self$pref_function[i] %in% pref_types) {
          sens_p[i, 3] <- self$p_threshold[i]
        } else {
          sens_p[i, 2:4] <- "NA"
        }
        if (self$pref_function[i] %in% pref_types) {
          sens_i[i, 3] <- self$i_threshold[i]
        } else {
          sens_i[i, 2:4] <- "NA"
        }
        if (self$pref_function[i] == "Gaussian") {
          sens_im[i, 3] <- self$im_threshold[i]
        } else {
          sens_im[i, 2:4] <- "NA"
        }
        m_i <- max(self$pm[, i])

        # functions level and linear (both p & i)
        if (self$pref_function[i] %in% c("level", "linear")) {
          s <- (self$p_threshold[i] - self$i_threshold[i]) / step
          hyp_i0 <- rev(seq(from = 0, to = self$i_threshold[i],
                            by = self$i_threshold[i] / step))
          hyp_i1 <- seq(from = self$i_threshold[i],
                        to = self$p_threshold[i] - s, by = s)
          hyp_p0 <- rev(hyp_i1)
          hyp_p1 <- seq(from = self$p_threshold[i], to = m_i,
                        by = (m_i - self$p_threshold[i] / step))
          sens_p[i, 2] <- private$sens_p2(hyp_p0, i)
          sens_p[i, 4] <- private$sens_p2(hyp_p1, i)
          sens_i[i, 2] <- private$sens_i2(hyp_i0, i)
          sens_i[i, 4] <- private$sens_i2(hyp_i1, i)
        }

        # prefference threshold (p) only
        if (self$pref_function[i] == "V-shape") {
          hyp_p0 <- rev(seq(from = 0, to = self$p_threshold[i],
                            by = self$p_threshold[i] / step))
          hyp_p1 <- seq(from = self$p_threshold[i], to = m_i,
                        by = (m_i - self$p_threshold[i] / step))
          sens_p[i, 2] <- private$sens_p2(hyp_p0, i)
          sens_p[i, 4] <- private$sens_p2(hyp_p1, i)
        }

        # only indifference threshold (i)
        if (self$pref_function[i] == "U-shape") {
          hyp_i0 <- rev(seq(from = 0, to = self$i_threshold[i],
                            by = self$i_threshold[i] / step))
          hyp_i1 <- seq(from = self$i_threshold[i], to = m_i,
                        by = (m_i - self$i_threshold[i]) / step)
          sens_i[i, 2] <- private$sens_i2(hyp_i0, i)
          sens_i[i, 4] <- private$sens_i2(hyp_i1, i)
        }

        # intermediate threshold (im)
        if (self$pref_function[i] == "Gaussian") {
          if (!is.null(self$i_threshold[i]) &&
                is.numeric(self$i_threshold[i]) &&
                !is.null(self$p_threshold[i]) &&
                is.numeric(self$p_threshold[i]) &&
                self$p_threshold[i] > self$i_threshold[i]) {
            # im based on p and i
            t <- seq(from = self$i_threshold[i], to = self$im_threshold[i],
                     by = (self$im_threshold[i] - self$i_threshold[i]) / step)
            hyp_im0 <- rev(t)
            s <- (self$p_threshold[i] - self$im_threshold[i]) / step
            hyp_im1 <- seq(from = self$im_threshold[i],
                           to = self$p_threshold - s, by = s)
          } else { # im only
            hyp_im0 <- rev(seq(from = 0, to = self$im_threshold[i],
                               by = self$im_threshold[i] / step))
            hyp_im1 <- seq(from = self$im_threshold[i], to = m_i,
                           by = (m_i - self$im_threshold[i]) / step)
          }
          sens_im[i, 2] <- private$sens_im2(hyp_im0, i)
          sens_im[i, 4] <- private$sens_im2(hyp_im1, i)
        }
      } # end of cycle iterating over criteria
      t <- list(
        sens_prefference = sens_p,
        sens_indifference = sens_i,
        sens_intermediate = sens_im
      )
      return(t)
    }
  ),
  private = list(
    # @description
    # sensitivity testing for preference threshold
    #
    # @param p vector of preference threshold values to be tested for
    #  sensitivity
    # @param j criterium being tested
    #
    # @return
    # value of preference threshold at which provided solution for the decision
    # problem changes. If no change detected returns insens.
    sens_p2 = function(p, j) {
      p2 <- self$p_threshold
      for (i in seq_along(p)) {
        p2[j] <- p[i]
        t <- promethee1$new(self$pm_orig, self$pref_function, self$w,
                            self$minmax, self$i_threshold, p2,
                            self$im_threshold)
        if (!vector_compare(t$positiveFlow, self$positiveFlow) ||
              !vector_compare(t$negativeFlow, self$negativeFlow)) {
          if (i != 1) return(p[i - 1])
          return(p[i])
        }
      }
      return("insens.")
    },

    # @description
    # sensitivity testing for indifference threshold
    #
    # @param indiff vector of indifference threshold values to be tested for
    #  sensitivity
    # @param j criterium being tested
    #
    # @return
    # value of indifference threshold at which provided solution for the
    # decision problem changes. If no change detected returns insens.
    sens_i2 = function(indiff, j) {
      indiff2 <- self$i_threshold
      for (i in seq_along(indiff)) {
        indiff2[j] <- indiff[i]
        t <- promethee1$new(self$pm_orig, self$pref_function, self$w,
                            self$minmax, indiff2, self$p_threshold,
                            self$im_threshold)
        if (!vector_compare(t$positiveFlow, self$positiveFlow) ||
              !vector_compare(t$negativeFlow, self$negativeFlow)) {
          if (i != 1) return(indiff[i - 1])
          return(indiff[i])
        }
      }
      return("insens.")
    },

    # @description
    # sensitivity testing for intermediate threshold
    #
    # @param im vector of intermediate threshold values to be tested for
    #  sensitivity
    # @param j criterium being tested
    #
    # @return
    # value of intermediate threshold at which provided solution for the
    # decision problem changes. If no change detected returns insens.
    sens_im2 = function(im, j) {
      im2 <- self$im_threshold
      for (i in seq_along(im)) {
        im2[j] <- im[i]
        t <- promethee1$new(self$pm_orig, self$pref_function, self$w,
                            self$minmax, self$i_threshold, self$p_threshold,
                            im2)
        if (!vector_compare(t$positiveFlow, self$positiveFlow) ||
              !vector_compare(t$negativeFlow, self$negativeFlow)) {
          if (i != 1) return(im[i - 1])
          return(im[i])
        }
      }
      return("insens.")
    }
  )
)