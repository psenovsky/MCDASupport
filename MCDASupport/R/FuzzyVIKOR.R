#' method used to solve multiple criteria decision making
#'
#' @description
#' The acronym VIKOR stands for: VlseKriterijumska Optimizacija I Kompromisno
#'  Resenje, in Serbian multicriteria optimization and compromise solution. The
#'  method has been especially designed to deal with problematic situations
#'  when the alternatives are characterized by non-commensurable and
#'  conflicting criteria, for which VIKOR provides compromise solution.
#'  Methodologically VIKOR is close to another method TOPSIS. Original VIKOR
#'  uses five steps to derive such compromise solution.
#'
#' FuzzyVIKOR uses trapezoidal fuzzy number for computation purposes. Other
#'  then using fuzzy number for computation, the results are basically the
#'  same: S, R and Q, which can be used to compute compromise solution. Look
#'  in the VIKOR function (see \link{vikor}) description to better
#'  understand the computation proces (or the source code).
#'
#' At present time FuzzyVIKOR function does not have implemented establishment
#'  of compromise solution.
#'
#' Mathematically the only thing different of FuzzyVIKOR to norlam VIKOR is
#'  necessity to compute crisp number from the trapezoidal fuzzy number.
#'  Following formula is used to do that.
#'
#' \mjsdeqn{ crisp_a =  -a_1 \cdot a_2 + a_3 \cdot a_4 + \frac{(a_4 - a_3)^2}{3} - \frac{1}{3} \cdot \frac{(a_2 - a_1)^2}{-a_1 - a_2 + a_3 + a_4} }
#'
#' Where \mjseqn{crisp_a} ... is crisp number for number \mjseqn{a = (a_1, a_2, a_3, a_4)}.
#'
#' @references
#' ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca
#'  Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
#'
#' Papathanasiou, Jason, Ploskas, Nikolaos. Multiple Criteria Decision Aid
#'  Methods, Examples and Python Implementations. Springer, 173 p.,
#'  ISBN 978-3-319-91648-4.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords VIKOR FuzzyVIKOR fuzzy set
fuzzyvikor <- R6Class("fuzzyvikor",
  public = list(
    #' @field criteria vector of criterias
    criteria = NULL,

    #' @field alt vector of alternative's names
    alt = NULL,

    #' @field w weights (matrix of weights - decision makers in columns and
    #'  criteria in rows)
    w = NULL,

    #' @field pm performance matrix with n columns and no. criteria x no. of
    #'  alternatives rows. I. e. with 3 criteria and 2 alternatives the rows
    #'  are: 1: 1.cri-1.alt., 2: 1.cri.-2.alt, 3: 2.cri-1.alt, 4: 2.cri-2.alt,
    #'  ...
    pm = NULL,

    #' @field v weight for strategy of majority of the criteria (0-1)
    v = NULL,

    #' @field dictionary_pm dictionary of linguistic variables for criteria
    dictionary_pm = NULL,

    #' @field dictionary_w dictionary for wights (single matrix|dataframe)
    dictionary_w = NULL,

    #' @field S list of alternatives using S-metric
    S = NULL,

    #' @field R list of alternatives using R-metric
    R = NULL,

    #' @field Q list of alternatives using Q-metric
    Q = NULL,

    #' @field compromiseSolution compromise solution based on S, R and Q metrics
    compromiseSolution = NULL,

    #' @description
    #' public constructor for fuzzyvikor R6 class. Checks validity of the
    #'  provided parameters. Initializes model and computes it.
    #'
    #' @param pm performance matrix with n columns and no. criteria x no. of
    #'  alternatives rows. I. e. with 3 criteria and 2  alternatives the rows
    #'  are: 1: 1.cri-1.alt., 2: 1.cri.-2.alt, 3: 2.cri-1.alt,
    #'  4: 2.cri-2.alt, ...
    #' @param dictionary_pm - dictionary of linguistic variables for criteria
    #'  (single matrix)
    #' @param w weights (matrix of weights - decision makers in columns and
    #'  criteria in rows)
    #' @param dictionary_w dictionary for wights (single matrix|dataframe)
    #' @param alt vector with names of the alternatives
    #' @param v weight for strategy of majority of the criteria (0-1)
    #'
    #' @examples
    #' dictionaryW <- rbind(
    #'   c(0, 0, 0.1, 0.2),
    #'   c(0.1, 0.2, 0.2, 0.3),
    #'   c(0.2, 0.3, 0.4, 0.5),
    #'   c(0.4, 0.5, 0.5, 0.6),
    #'   c(0.5, 0.6, 0.7, 0.8),
    #'   c(0.7, 0.8, 0.8, 0.9),
    #'   c(0.8, 0.9, 1, 1)
    #' )
    #' # V very, L low, M medium, H high
    #' rownames(dictionaryW) <- c('VL', 'L', 'ML', 'M', 'MH', 'H', 'VH')
    #' w <- rbind(
    #'   c('H', 'VH', 'VH'),
    #'   c('M', 'H', 'VH'),
    #'   c('M', 'MH', 'ML'),
    #'   c('H', 'VH', 'MH')
    #' )
    #' rownames(w) <- c('Investment cost', 'Employment needs', 'Social impact',
    #'  'Environmental impact')
    #' dictionaryPM <- rbind(
    #'   c(0, 0, 0.1, 0.2),
    #'   c(0.1, 0.2, 0.2, 0.3),
    #'   c(0.2, 0.3, 0.4, 0.5),
    #'   c(0.4, 0.5, 0.5, 0.6),
    #'   c(0.5, 0.6, 0.7, 0.8),
    #'   c(0.7, 0.8, 0.8, 0.9),
    #'   c(0.8, 0.9, 1, 1)
    #' )
    #' # V very, P poor, M medium, F fair, G good
    #' rownames(dictionaryPM) <- c('VP', 'P', 'MP', 'F', 'MG', 'G', 'VG')
    #' PM <- rbind(
    #'   c('VG', 'G', 'MG'),
    #'   c('MP', 'F', 'F'),
    #'   c('MG', 'MP', 'F'),
    #'   c('MG', 'MG', 'VG'),
    #'   c('VP', 'P', 'G'),
    #'   c('F', 'G', 'G'),
    #'   c('F', 'MG', 'MG'),
    #'   c('F', 'VG', 'G'),
    #'   c('MG', 'MG', 'VG'),
    #'   c('G', 'G', 'VG'),
    #'   c('P', 'VP', 'MP'),
    #'   c('F', 'MP', 'MG'),
    #'   c('P', 'P', 'MP'),
    #'   c('MG', 'VG', 'G'),
    #'   c('MP', 'F', 'F'),
    #'   c('MG', 'VG', 'G'),
    #'   c('G', 'G', 'VG'),
    #'   c('VG', 'MG', 'F'),
    #'   c('G', 'VG', 'G'),
    #'   c('MG', 'F', 'MP'),
    #'   c('MP', 'P', 'P'),
    #'   c('VP', 'F', 'P'),
    #'   c('G', 'MG', 'MG'),
    #'   c('P', 'MP', 'F')
    #' )
    #' alternatives <- c('site 1', 'site 2', 'site 3', 'site 4', 'site 5',
    #'                   'site 6')
    #' result <- fuzzyvikor$new(PM, dictionaryPM, w, dictionaryW, alternatives)
    initialize = function(pm, dictionary_pm, w, dictionary_w, alt, v = NULL) {
      ## check validity of the objects manipulated by the current function
      n <- ncol(pm) #no. of decision makers
      criteria <- rownames(w)
      if (ncol(w) != n) {
        stop("number of decission makers in w and PM is not same")
      }
      ncri <- nrow(w)
      if (nrow(pm) %% ncri != 0) {
        stop("no. of rows in PM must == to no. cri * no. alt.")
      }
      nalt <- trunc(nrow(pm) / ncri)
      if (nalt != length(alt)) {
        stop("No. of alternatives in alt param does not corespond to 
             no. of alternatives in PM")
      }
      if (!private$fuzzy_consistency(pm, dictionary_pm, "PM")) {
        stop("Unknown error when checking consistency of fuzzy numbers in PM")
      }
      if (!private$fuzzy_consistency(w, dictionary_w, "weights")) {
        stop("Unknown error when checking consistency of fuzzy numbers
             in weights matrix")
      }
      if (is.null(v)) {
        v <- (ncri + 1) / (2 * ncri)
      } else if (!is.numeric(v)) {
        stop("weight of the strategy v must be numeric value (or NULL) 
             for procedure to work")
      } else if (v < 0 || v > 1) {
        stop("weight of the strategy v must be in interval 0-1 (or NULL).")
      }
      ## End of checking the validity of the "inputs"

      self$pm <- pm
      self$v <- v
      self$dictionary_pm <- dictionary_pm
      self$w <- w
      self$dictionary_w <- dictionary_w
      self$alt <- alt
      self$criteria <- criteria
      self$compute()
      self
    },

    #' @description
    #' computes the Fuzzy VIKOR problem. Usually doesn't need to be run manually
    #'  as it is called automatically by class constructor.
    compute = function() {
      n <- ncol(self$pm) #no. of decision makers
      ncri <- nrow(self$w)
      nalt <- length(self$alt)

      w2 <- private$agg_fuzzy_value(self$dictionary_w, self$w, n)
      rownames(w2) <- self$criteria
      f_rdm_all <- private$agg_fuzzy_value(self$dictionary_pm, self$pm, n)
      crisp_weights <- private$defuzz_m(w2)
      names(crisp_weights) <- self$criteria
      crisp_alternative_ratings <- matrix(private$defuzz_m(f_rdm_all),
                                          nrow = nalt)
      rownames(crisp_alternative_ratings) <- self$alt
      colnames(crisp_alternative_ratings) <- self$criteria
      bw <- private$best_worst_fij(crisp_alternative_ratings)
      sr_indexes <- VIKORIndexes(crisp_alternative_ratings, bw, crisp_weights,
                                 self$v)
      # fill in results
      self$S <- sr_indexes$S
      self$R <- sr_indexes$R
      self$Q <- sr_indexes$Q
      self$compromiseSolution <- sr_indexes$compromiseSolution
    },

    #' @description
    #' summary of the Fuzzy VIKOR method resutls.
    #' @return basic information on the model including ranking.
    summary = function() {
      cat(paste0("Fuzzy VIKOR\nS metric\n"))
      print(self$S, pretty = TRUE)
      cat(paste("\nR metric\n"))
      print(self$R, pretty = TRUE)
      cat(paste("\nQ metric\n"))
      print(self$Q, pretty = TRUE)
      cat(paste("\nCompromise solution\n"))
      print(unlist(self$compromiseSolution), pretty = TRUE)
    }
  ),
  private = list(
    # function to check consistency of matrix againts dictionary of values
    #
    # parameters
    #   M - matrix to be checked
    #   dict - dictionary to check matrix against
    #   msg - message to guide user on what is being examined i.e. PM,
    #         weight matrix, etc.
    #   returns - TRUE if everything is OK, otherwise stops the computation
    #             (at first problem)
    fuzzy_consistency = function(m, dict, msg = NULL) {
      # check consistency of dictionary (needed to also check consistency
      # of M later)
      if (ncol(dict) != 4) {
        stop(paste("dictionary ", msg,
                   " needs to be defined as trapezoid fuzzy number (4 columns)"))
      }
      if (!is.numeric(unlist(dict))) {
        stop(paste("Only numeric values expected in ", msg))
      }
      # check consistency of dictionary - numbers are expected to grow from
      # left to right and from top to bottom
      for (i in seq_len(dict)) {
        for (j in 2:4) {
          if (dict[i, j - 1] > dict[i, j - 1]) {
            stop(paste("Problem with dictionaryPM inconsistency in row ", i))
          }
        }
        if (i > 1) { # top to bottom check
          for (j in 1:4) {
            if (dict[i, j] < dict[i - 1, j]) {
              stop(paste("Inconsistency in dict. ", msg,
                         " detected on [line, row]: [", i, ",", j,
                         "], rows are expected to go from lowest value",
                         " to highest."))
            }
          }
        }
      } # end check consistency of dictionary
      dict_names <- rownames(dict) #list of variables values used in dictionary
      # check consistency of matrix M
      if (!(is.matrix(m) || (is.data.frame(m)))) {
        stop(paste("Problem with definition of matrix, ", msg,
                   ": not matrix or dataframe."))
      }
      #check whether the M uses only fuzzy numbers from dictionary
      tm <- unlist(m)
      for (j in seq_along(tm)) { #check for consistency of preference function
        if (!(tm[j] %in% dict_names)) {
          stop(paste("Dictionary ", msg, " uses value not in the dictionary (",
                     tm[j], ")"))
        }
      }
      return(TRUE) # every check passed, return TRUE
    }, # end of fuzzyConsistency function

    # Convert the linguistic variables for the criteria weights or the ratings
    # into fuzzy weights and fuzzy decision matrix, respectively
    #
    # parameters
    #   dictionary - dictionary with the linguistic variables
    #   M - matrix with the criteria weights (or the ratings)
    #   n - number of the decision makers
    #   returns fuzzy weights/decision matrix
    agg_fuzzy_value = function(dictionary, m, n) {
      ncri <- nrow(m)
      f <- matrix(data = 0, nrow = ncri, ncol = 4)
      for (j in 1:ncri) {
        mj <- which(rownames(dictionary) %in% m[j, ])
        if (length(mj) > 0) {
          k0 <- min(dictionary[mj, 1])
          k1 <- sum(dictionary[mj, 2])
          k2 <- sum(dictionary[mj, 3])
          k3 <- max(dictionary[mj, 4])

          f[j, 1] <- round(k0, 3)
          f[j, 2] <- round(k1 / n, 3)
          f[j, 3] <- round(k2 / n, 3)
          f[j, 4] <- round(k3, 3)
        }
      }
      return(f)
    },

    # Function to deffuzify a trapezoidal fuzzy number into a crisp value
    #
    # parameters
    #   fN - trapezoidal fuzzy number matrix
    #   returns - crisp value for fuzzy number matrix
    defuzz_m = function(fn) {
      t <- (-fn[, 1] * fn[, 2] + fn[, 3] * fn[, 4] + 1 / 3 * (fn[, 4] - fn[, 3])^2
            - 1 / 3 * (fn[, 2] - fn[, 1])^2) / (-fn[, 1] - fn[, 2] + fn[, 3]
                                                + fn[, 4])
      return(t)
    },

    # Function to deffuzify a trapezoidal fuzzy number into a crisp value
    #
    # parameters
    #   fN - trapezoidal fuzzy number
    #   returns - crisp value for fuzzy number
    defuzz = function(fn){
      t <- (-fn[1] * fn[2] + fn[3] * fn[4] + 1 / 3 * (fn[4] - fn[3])^2
            - 1 / 3 * (fn[2] - fn[1])^2) / (-fn[1] - fn[2] + fn[3] + fn[4])
      return(t)
    },

    # Function determines best, worst values and difference between them for all
    # criteria functions
    #
    # parameters
    #   car is the array with the performances (crisp alternative ratings)
    #   return matrix with max and min and difference values for the criteria
    best_worst_fij = function(car) {
      best <- apply(car, 2, max)
      worst <- apply(car, 2, min)
      difference <- best - worst

      f <- rbind(best, worst, difference)

      colnames(f) <- colnames(car)
      rownames(f) <- c("best", "worst", "difference")
      f <- t(f)

      return(f)
    }
  )
)