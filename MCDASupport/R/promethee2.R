#' method for computations of partial preference using PROMETHEE II method
#'
#' @description
#' PROMETHEE stands for Preference Ranking Organization Method for Enrichment
#'  Evaluation. While \link{promethee1} focuses on computation of
#'  flows and establishment of basic outranking relation between the
#'  alternatives, PPROMETHEE II takes the proces one step further and provides
#'  full ranking of alternatives.
#'
#' Positive, negative and net flows are computed in same way as in PROMETHEE I,
#'  see \code{\link{PROMETHEE}} function, which serves as general
#'  implementation of the approach in PROMETHEE I - III implementation in this
#'  package.
#'
#' PROMETHEE II has very straighforward approach to computation of the ranks for
#'  alternatives. It takes net flow provided by PROMETHEE function and orders it
#'  decresingly. Alternatives with same values do have same rank, othervise we
#'  get full order of alternatives.
#'
#' @references
#' ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca
#'  Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords promethee2 promethee
promethee2 <- R6Class("promethee2",
  public = list(
    #' @field pm_orig original (not transformed) performance matrix of
    #'  alternatives in criteria
    pm_orig = NULL,

    #' @field pm performance matrix of alternatives in criteria (all criteria
    #'  are maximized)
    pm = NULL,

    #' @field pref_function vector, specifies type of function used to compute
    #'  preferences. Need to be set for each criterion. Possible values are:
    #'  'default', 'U-shape', 'V-shape', 'level', 'linear', 'Gaussian'. Choice
    #'  of function type will decide on what type of threshold (if any) is
    #'  required for computation. Each criterion can use different preference
    #'  function.
    pref_function = NULL,

    #' @field w vector of weights
    w = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field i_threshold vector of indifference thresholds
    i_threshold = NULL,

    #' @field p_threshold vector of proference thresholds
    p_threshold = NULL,

    #' @field im_threshold vector containing intermetiate thresholds for
    #'  criteria. only Gaussian type performance functions rewuire this type of
    #'  threshold. If prefference and indifference thresholds are present, the
    #'  PROMETHEE function will try to 'gues' intermediate threshold as value
    #'  right in the middle between these thresholds.
    im_threshold = NULL,

    #' @field positiveFlow vector representing how the alternative is preffered
    #'  to other alternatives
    positiveFlow = NULL,

    #' @field negativeFlow vector representing how altenative is outranked by
    #'  other alternatives
    negativeFlow = NULL,

    #' @field netFlow positive - negative flow, forms an indicator PROMETHEE II
    #'  uses to directly rank the alternatives
    netFlow = NULL,

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

    #' @field rankedList ranked list of alternatives
    rankedList = NULL,

    #' @field rankedListNOF sorted list of alternatives with assigned net
    #'  outranking flow indicator, which can be used to sort the list.
    rankedListNOF = NULL,

    #' @field preferenceMatrix preference matrix describing ourranking
    #'  relation between the alternatives
    preferenceMatrix = NULL,

    #' @description
    #' public constructor for the promethee2 class. Validates input parameters
    #'  and computes model.
    #'
    #' @param pm Matrix or data frame containing the performance table. Each
    #'  row corresponds to an alternative, and each column to a criterion.
    #'  only numeric values expercted. Rows and columns are expected to be
    #'  named.
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
    #' result <- promethee2$new(PM, shape, w, minmax, q, p, s)
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
    #' Computes PROMETHEE II model based on parametrs specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation si required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      alt  <- rownames(self$pm) #list of alternatives
      flow <- PROMETHEE(self$pm, self$pref_function, self$w, self$i_threshold,
                        self$p_threshold, self$im_threshold)
      pf <- flow$positiveFlow
      nf <- flow$negativeFlow
      nof <- flow$netFlow #net outranking flow (nof)
      names(nof) <- alt
      ordering <- sort(nof, decreasing = TRUE)
      groups <- split(names(ordering), ordering)
      rank <- sapply(groups, function(g) paste(g, collapse = ", "))
      r <- data.frame(rank, as.numeric(names(rank)))
      rownames(r) <- NULL
      colnames(r) <- c("alternative", "netFlow")
      r <- r[order(r$netFlow, decreasing = TRUE), ]
      r$rank <- seq_along(rank)
      #establish preference system
      pref <- outer(nof, nof, function(x, y) as.integer(x > y))
      rownames(pref) <- alt
      colnames(pref) <- alt

      # assign results
      self$positiveFlow <- pf
      self$negativeFlow <- nf
      self$netFlow <- nof
      self$preferenceDegree <- flow$preferenceDegree
      self$preferenceDegreeUnw <- flow$preferenceDegreeUnw
      self$pairweiseComparison <- flow$pairweiseComparison
      self$rankedList <- r
      self$rankedListNOF <- ordering
      self$preferenceMatrix <- pref
    },

    #' @description
    #' Creates summary for PROMETHEE II model and sends it to console.
    summary = function() {
      nalt <- nrow(self$pm)
      cat(paste0("PROMETHEE II\nprocessed ", nalt, " alternatives in ",
                 length(self$w), " criteria\nPositive flow\n"))
      print(self$positiveFlow, pretty = TRUE)
      cat(paste("\nNegative flow\n"))
      print(self$negativeFlow, pretty = TRUE)
      cat(paste("\nNet flow\n"))
      print(self$netFlow, pretty = TRUE)
      cat(paste("\nRanked alternatives\n"))
      print(self$rankedList, pretty = TRUE)
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
    #'  (default: 100)
    #'
    #' @return dataframes sens_i, sens_p and sens_im with sensitivity limit for
    #'  the criteria
    sensitivity = function(step = 100) {
      return(sensitivity_p12(self, step))
    }
  )
)