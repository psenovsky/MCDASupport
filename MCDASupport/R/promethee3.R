#' computations of partial preference using PROMETHEE III method
#'
#' @description
#' PROMETHEE stands for Preference ranking organization method for enrichment
#'  evaluation. Promethee II method is intended for establishment of full
#'  ranking of the alternatives, by evaluation of positive and negative
#'  prefference flows in pairweise comparisons of the alternatives.
#'
#' Difference between positive and negative flows forms net outranking flow.
#'  PROMETHEE III function is usable for partial preorder, similarly to
#'  \link{promethee1}, but uses intervals for establishing preorder.
#'
#' Method starts with quantitatively expressed performance of the alternatives
#'  in the criteria. Positive, negative and net flows is computed using
#'  \code{\link{PROMETHEE}} function (see PROMETHEE function documentation for
#'  details on computation), in same way as in \link{promethee1} and
#'  \link{promethee2}. The difference is in what the method does with the
#'  results next.
#'
#' \mjsdeqn{s_{err} = \frac{sd_{net}}{\sqrt{n}}}
#'
#' Where \mjseqn{s_{err}} ... standard error, \mjseqn{sd_{net}} standard
#'  deviation of net flow, n ... number of alternatives.
#'
#' For each alternative we then specify x and y limit computed as:
#'
#' \mjsdeqn{x_{lim} = net_i - s_{err}}
#'
#' \mjsdeqn{y_{lim} = net_i + s_{err}}
#'
#' Where x limit is lover limit of of the interval and y is the upper limit,
#'  \mjseqn{s_{err}} ... is standard error.
#'
#' Partial preference is then  established based on comparing lower and upper
#'  limits of the alternatives.
#'
#' If \mjseqn{x_{lim}(a) > y_{lim}(b)} then alternative is prefered to b: aPb.
#'  If \mjseqn{x_{lim}(a) == y_{lim}(b)} than we are clearly indifferent to the
#'  alternatives: aIb. Otherwise alternative b is prefered to a.
#'
#' Method does not provide ranking.
#'
#' Limit comparison can be seen as an alternative implementation of thresholds
#'  as the performance must exceed some limit for the alternative to be
#'  considered prefered.
#'
#' Note: the implementation of function is partially inspired by implementation
#'  of PROMETHEE III portion of promethee123 package, thou this implementation
#'  provides different output and utilizes general PROMETHEE function developed
#'  for PROMETHEE I and II functions.
#'
#' @references
#' ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca
#'  Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
#'
#' BRANS, Jean-Pierre; DE SMET, Yves. PROMETHEE methods. In: Multiple criteria
#'  decision analysis. Springer, New York, NY, 2016. p. 187-219. DOI:
#'  \url{https://dx.doi.org/10.1007/978-1-4939-3094-4_6}.
#'
#' Moreira, M.A.L., dos Santos, M., Gomes, C.F.S. promethee123 package.
#'  \url{https://cran.r-project.org/web/packages/promethee123/index.html}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords PROMETHEE promethee3
promethee3 <- R6Class("promethee3",
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

    #' @field preferenceMatrix preference matrix describing ourranking
    #'  relation between the alternatives
    preferenceMatrix = NULL,

    #' @description
    #' Public constructor to assemble promethee3 object.
    #'
    #' First it validates inpust, then it computes the model using PROMETHEE
    #'  III method. See class documentation for computational details of it.
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
    #'  \mjseqn{0 \le w_i \le 1, \sum w_i = 1}
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
    #' @param p_threshold vector containing prefference threshods for
    #'  criteria. Not all types of performance functions require it. The
    #'  parameter must be used if there is at least one criterion, for which it
    #'  is required. Values for all other criteria should be 0 (and will not be
    #'  used during computations). Only 'V-shape', 'level', 'linear' functions
    #'  need this threshold.
    #' @param im_threshold vector containing intermetiate thresholds for
    #'  criteria. only Gaussian type performance functions rewuire this type of
    #'  threshold. If prefference and indifference thresholds are present, the
    #'  PROMETHEE function will try to 'gues' intermediate threshold as value
    #'  right in the middle between these thresholds.
    #'
    #' @return initialized and computed promethee3 R6 class.
    #'
    #' @examples
    #' #Example from Fuzzy TOPSIS book (see references)
    #' #ammended error in tab. 4.9, the computation presumes maximization of
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
    #'  'Gaussian')
    #' p <- c(10, 0, 450, 50, 0, 0) #indifference threshold
    #' q <- c(0, 30, 50, 10, 0, 0) #prefference threshold
    #' s <- c(0,0,0,0,0,5) #intermediate threshold
    #' w <- c(0.1667, 0.1667, 0.1667, 0.1667, 0.1667, 0.1665)
    #' result <- promethee3$new(PM, shape, w, minmax, q, p, s)
    initialize = function(pm, pref_function, w, minmax = "max",
                          i_threshold = NULL, p_threshold = NULL,
                          im_threshold = NULL) {
      #params consistency check centralized in generalized PROMETHEE function
      #here is only minmax evaluation
      self$pm_orig <- pm
      #validate minmax and invert scales if neccessary
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
    #' Computes PROMETHEE III model based on parametrs specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation si required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      nalt <- nrow(PM)  #no. of alternatives
      alt  <- rownames(PM) #list of alternatives
      flow <- PROMETHEE(self$pm, self$pref_function, self$w, self$i_threshold,
                        self$p_threshold, self$im_threshold)
      pf <- flow$positiveFlow
      nf <- flow$negativeFlow
      nof <- flow$netFlow
      names(nof) <- alt


      stand_error <- round((sd(nof) / sqrt(nalt)), 3)
      x_limit <- round(nof - stand_error, 3)
      y_limit <- round(nof + stand_error, 3)

      #establish preference system
      pref <- matrix(data = "-", nrow = nalt, ncol = nalt)
      p_plus <- outer(x_limit, y_limit, ">")
      i <- outer(x_limit, y_limit, "<=") & outer(y_limit, x_limit, ">=")
      p_minus <- !(p_plus | i)
      diag(p_plus) <- diag(p_minus) <- diag(i) <- FALSE
      pref[p_plus] <- "P+"
      pref[p_minus] <- "P-"
      pref[i] <- "I"
      rownames(pref) <- alt
      colnames(pref) <- alt

      self$positiveFlow <- pf
      self$negativeFlow <- nf
      self$netFlow <- nof
      self$preferenceDegree <- flow$preferenceDegree
      self$preferenceDegreeUnw <- flow$preferenceDegreeUnw
      self$pairweiseComparison <- flow$pairweiseComparison
      self$preferenceMatrix <- pref
    },

    #' @description
    #' Creates summary for PROMETHEE III model and sends it to console.
    summary = function() {
      nalt <- nrow(self$pm)
      cat(paste0("PROMETHEE III\nprocessed ", nalt, " alternatives in ",
                 length(self$w), " criteria\nPositive flow\n"))
      print(self$positiveFlow, pretty = TRUE)
      cat(paste("\nNegative flow\n"))
      print(self$negativeFlow, pretty = TRUE)
      cat(paste("\nNet flow\n"))
      print(self$netFlow, pretty = TRUE)
      cat(paste("\nPairweise comaprison martix\n"))
      print(self$preferenceMatrix, pretty = TRUE)
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