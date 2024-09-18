#' TOPSIS decision support method
#'
#' @description
#' The acronym TOPSIS stands for: Technique of Order Preference Similarity to
#'  the Ideal Solution. As name suggests TOPSIS provides its guidance based on
#'  evaluation of the similarity to both ideal and anti-ideal variant of the
#'  solution.
#'
#' Original method uses 5 steps for the procedure. In first step the procedure
#'  normalizes values in performance matrix nad applies weights to it in step
#'  2. In step 3 ideal variant \mjseqn{A^*} and anti-ideal variant \mjseqn{A^-}
#'  is computed as maximums and minimums of the criteria in performance matrix.
#'
#' In step 4 distance to ideal \mjseqn{D^*} and anti-ideal variant \mjseqn{D^-}
#'  is computed and in step 5 used to compute closenes criterium
#'
#' \mjsdeqn{C_i = \frac{D^-_i}{D^-_i + D^*_i}}
#'
#' Criterium C is then directly usable to rank alternatives. C is always in
#'  interval of 0-1, the closer the value is to 1, the closer it is to ideal
#'  variant.
#'
#' @references
#' ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca
#'  Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
#'
#' Papathanasiou, Jason, Ploskas, Nikolaos. Multiple Criteria Decision Aid
#'  Methods, Examples and Python Implementations. Springer, 173 p., ISBN
#'  978-3-319-91648-4.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords TOPSIS
topsis <- R6Class("topsis",
  public = list(

    #' @field pm_orig original (not transformed) performance matrix of
    #'  alternatives in criteria
    pm_orig = NULL,

    #' @field pm performance matrix of alternatives in criteria (all criteria
    #'  are maximized)
    pm = NULL,

    #' @field w vector of weights
    w = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field closeness_ord ordered list of alternatives using closeness (C)
    #'  criterium
    closeness_ord = NULL,

    #' @field normPM normalized performance matrix
    normPM = NULL,

    #' @field weightPM weighted normalized performance matrix
    weightPM = NULL,

    #' @field A_ideal positive ideal solution
    A_ideal = NULL,

    #' @field A_anti anti ideal solution
    A_anti = NULL,

    #' @field D_ideal alternative closeness to ideal variant
    D_ideal = NULL,

    #' @field D_anti alternative closeness to anti-ideal variant
    D_anti = NULL,

    #' @description
    #' Public constructor for TOPSIS object. Performas validation of input
    #'  parameters and uses them to compute the model.
    #'
    #' @param pm Matrix or data frame containing the performance table. Each
    #'  row corresponds to an alternative, and each column to a criterion.
    #'  Only numeric values expercted. Rows and columns are expected to be
    #'  named.
    #' @param w vector containing the weights of the criteria.
    #' @param minmax criteria MinMax Vector containing the preference direction
    #'  on each of the criteria. "min" (resp."max") indicates that the
    #'  criterion has to be minimized (maximized). Default value: max
    #'
    #' @return
    #' Instance of topsis class uncluding inputs and model results in class's
    #'  properties
    #'
    #' @examples
    #' PM <- cbind(
    #'    c(8,7,2,1),
    #'    c(5,3,7,5),
    #'    c(7,5,6,4),
    #'    c(9,9,7,3),
    #'    c(11,10,3,7),
    #'    c(6,9,5,4)
    #' )
    #' colnames(PM) <- c('Site 1', 'Site 2', 'Site 3', 'Site 4', 'Site 5',
    #'    'Site 6')
    #' rownames(PM) <- c('Investment costs (million EUR)',
    #'                   'Employment needs (hundred employees)',
    #'                   'Social impact (1-7)',
    #'                   'Environmental impact (1-7)')
    #' PM <- t(PM)
    #' minmax <- 'max'
    #' w <- c(0.4, 0.4, 0.1, 0.2)
    #' result <- topsis$new(PM, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      ## check validity of the objects manipulated by the current function
      # with < 2 criteria or 2 alternatives, there is no MCDA problem
      if (is.null(dim(pm))) stop("less than 2 criteria or 2 alternatives")
      if (!(is.matrix(pm) || (is.data.frame(pm)))) {
        stop("wrong performance matrix, should be a matrix or a data frame")
      }
      if (!is.numeric(unlist(pm))) {
        stop("Only numeric values in performance matrix expected")
      }
      self$pm_orig <- pm
      # validate minmax and invert scales if neccessary
      self$pm <- util_pm_minmax(pm, minmax)
      self$pm <- as.data.frame(pm)
      if (!(is.vector(w, mode = "numeric"))) {
        stop("criteria weights should be a vector")
      }
      if (ncol(pm) != length(w)) {
        stop("length of criteria weights should be checked")
      }
      ## End of checking the validity of the "inputs"

      self$w <- w
      self$minmax <- minmax
      self$compute()
      self
    },

    #' @description
    #' Computes TOPSIS model based on parametrs specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation si required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      ncri <- ncol(self$pm)  #no. of criteria
      alt  <- rownames(self$pm)
      cri  <- colnames(self$pm)

      #Step 1. Normalization of the Decision Matrix (r) (validated)
      r <- sapply(1:ncri, function(j) self$pm[, j] / sqrt(sum(self$pm[, j] * self$pm[, j])))
      colnames(r) <- cri
      rownames(r) <- alt

      # Step 2. Calculation of the Weighted Normalized Decision Matrix (v)
      v <- sweep(r, MARGIN = 2, w, `*`)
      colnames(v) <- cri
      rownames(v) <- alt
      v <- as.data.frame(v)

      # steps 3 - 5 in topsis_ideal
      top_i <- topsis_ideal(v)
      #Step 6. order by C
      ordered_c <- sort(top_i$closenes, decreasing = TRUE)

      self$closeness_ord <- ordered_c
      self$normPM <- r
      self$weightPM <- v
      self$A_ideal <- top_i$a_ideal
      self$A_anti <- top_i$a_anti
      self$D_ideal <- top_i$d_ideal
      self$D_anti <- top_i$d_anti
    },

    #' @description
    #' Creates summary for TOPSIS model and sends it to console.
    summary = function() {
      nalt <- nrow(self$pm)
      cat(paste0("TOPSIS\nprocessed ", nalt, " alternatives in ",
                 length(self$w), " criteria\n\nA ideal\n"))
      print(self$A_ideal, pretty = TRUE)
      cat(paste("\nA antiideal\n"))
      print(self$A_anti, pretty = TRUE)
      cat(paste("\nD (alternative) ideal\n"))
      print(self$D_ideal, pretty = TRUE)
      cat(paste("\nD (alternative) antiideal\n\n"))
      print(self$D_anti, pretty = TRUE)
      cat(paste("\n\nSolution (ordered alternatives)\n"))
      print(self$closeness_ord, pretty = TRUE)
    }
  )
)