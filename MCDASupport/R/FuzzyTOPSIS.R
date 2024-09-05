#' TOPSIS method used to solve multiple criteria decision making based on fuzzy
#'  sets
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
#'  is computed and in step 5 used to compute closenes criterium (CC).
#'
#' Criterium CC is then directly usable to rank alternatives. CC is always in
#'  interval of 0-1, the closer the value is to 1, the closer it is to ideal
#'  variant.
#'
#' The approach described above is same as for TOPSIS method
#'  (see \code{\link{TOPSIS}}), thou in case of FuzzyTOPSIS triangular fuzzy
#'  numbers are used to describe the values in both criteria weights and
#'  performance matrix.
#'
#' Note: in present version of the function only benefit criteria are being
#'  supported.
#'
#' Mathematically main difference to TOPSIS is requirement to create crips
#'  number from triangular fuzzy number using following formula to compute
#'  perceived distance between the alternatives:
#'
#' \mjsdeqn{ dist = \sqrt{\frac{(a_1 - b_1)^2  + (a_2 - b_2)^2 + (a_3 - b_3)^2}{3}}}
#'
#' Where dist ... distance between alternatives a and b, both a and b are
#'  specifed in the form of triangle \mjseqn{a = (a_1, a_2, a_3)} and
#'  \mjseqn{b = (b_1, b_2, b_3)}.
#'
#' Technically we compute the distance as square root of average square of
#'  differences between the alternatives.
#'
#' @references
#' Papathanasiou, Jason, Ploskas, Nikolaos. Multiple Criteria Decision Aid
#'  Methods, Examples and Python Implementations. Springer, 173 p., ISBN
#'  978-3-319-91648-4.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords TOPSIS
#' @keywords fuzzy set
#' @keywords Outranking approaches
#' @keywords preference modelling
#' @keywords multicriteria analysis
#' @keywords ideal solution
#' @keywords anti-ideal solution
fuzzytopsis <- R6Class("fuzzytopsis",
  public = list(

    #' @field pm normalized performance matrix (direction of all criteria is
    #'  maximalize)
    pm = NULL,

    #' @field dictionary_pm dictionary of linguistic variables for criteria
    dictionary_pm = NULL,

    #' @field w weght vector
    w = NULL,

    #' @field dictionary_w dictionary for wights (single matrix or dataframe)
    dictionary_w = NULL,

    #' @field alt vector with names of the alternatives
    alt = NULL,

    #' @field n number of decision makers
    n = 0,

    #' @field fuzzy_weights fuzzy weights
    fuzzy_weights = NULL,

    #' @field fuzzy_decision_matrix fuzzy decision matrix
    fuzzy_decision_matrix = NULL,

    #' @field wfndm weighted normalized fuzzy decision matrix
    wfndm = NULL,

    #' @field a_plus a+
    a_plus = NULL,

    #' @field a_minus a-
    a_minus = NULL,

    #' @field cc Alternatives sorted by closeness coeficient
    cc = NULL,

    #' @description
    #' public constructor, creates fuzzytopsis object.
    #'
    #' @param pm performance matrix
    #' @param dictionary_pm dictionary scale for performance matrix
    #' @param w weights
    #' @param dictionary_w dicrtionary scale for weights
    #' @param alt alternative's names
    #'
    #' @examples
    #' dictionaryW <- rbind(
    #'  c(0, 0, 0.1),
    #'  c(0, 0.1, 0.3),
    #'  c(0.1, 0.3, 0.5),
    #'  c(0.3, 0.5, 0.7),
    #'  c(0.5, 0.7, 0.9),
    #'  c(0.7, 0.9, 1),
    #'  c(0.9, 1, 1)
    #' )
    #' # V = very, L = low, M = medium, H = high
    #' rownames(dictionaryW) <- c('VL', 'L', 'ML', 'M', 'MH', 'H', 'VH')
    #' w <- rbind(
    #'  c('H', 'VH', 'VH'),
    #'  c('M', 'H', 'VH'),
    #'  c('M', 'MH', 'ML'),
    #'  c('H', 'VH', 'MH')
    #' )
    #' rownames(w) <- c('Investment cost', 'Employment needs', 'Social impact',
    #'  'Environmental impact')
    #' dictionaryPM <- rbind(
    #'  c(0,0,1),
    #'  c(0,1,3),
    #'  c(1,3,5),
    #'  c(3,5,7),
    #'  c(5,7,9),
    #'  c(7,9,10),
    #'  c(9,10,10)
    #' )
    #' #V very, P poor, M medium, F fair, G good
    #' rownames(dictionaryPM) <- c('VP', 'P', 'MP', 'F', 'MG', 'G', 'VG')
    #' PM <- rbind(
    #'  c('VG', 'G', 'MG'),
    #'  c('MP', 'F', 'F'),
    #'  c('MG', 'MP', 'F'),
    #'  c('MG', 'MG', 'VG'),
    #'  c('VP', 'P', 'G'),
    #'  c('F', 'G', 'G'),
    #'  c('F', 'MG', 'MG'),
    #'  c('F', 'VG', 'G'),
    #'  c('MG', 'MG', 'VG'),
    #'  c('G', 'G', 'VG'),
    #'  c('P', 'VP', 'MP'),
    #'  c('F', 'MP', 'MG'),
    #'  c('P', 'P', 'MP'),
    #'  c('MG', 'VG', 'G'),
    #'  c('MP', 'F', 'F'),
    #'  c('MG', 'VG', 'G'),
    #'  c('G', 'G', 'VG'),
    #'  c('VG', 'MG', 'F'),
    #'  c('G', 'VG', 'G'),
    #'  c('MG', 'F', 'MP'),
    #'  c('MP', 'P', 'P'),
    #'  c('VP', 'F', 'P'),
    #'  c('G', 'MG', 'MG'),
    #'  c('P', 'MP', 'F')
    #' )
    #' alternatives <- c('site 1', 'site 2', 'site 3', 'site 4', 'site 5',
    #'  'site 6')
    #' result <- fuzzytopsis$new(PM, dictionaryPM, w, dictionaryW, alternatives)
    initialize = function(pm, dictionary_pm, w, dictionary_w, alt) {
      ## check validity of the objects manipulated by the current function
      n <- ncol(pm) #no. of decision makers
      if (ncol(w) != n) {
        stop("number of decission makers in w and PM is not same")
      }
      ncri <- nrow(w)
      if (nrow(pm) %% ncri != 0) {
        stop("no. of rows in PM must == to no. cri * no. alt.")
      }
      nalt <- trunc(nrow(pm) / ncri)
      if (nalt != length(alt)) {
        stop("No. of alternatives in alt param != no. of alternatives in PM")
      }
      if (!private$fuzzy_consistency(pm, dictionary_pm, "PM")) {
        stop("Error when checking consistency of fuzzy numbers in PM")
      }
      if (!private$fuzzy_consistency(w, dictionary_w, "weights")) {
        stop("Error when checking consistency of fuzzy numbers in weights matrix")
      }
      ## End of checking the validity of the "inputs"

      self$pm <- pm
      self$dictionary_pm <- dictionary_pm
      self$w <- w
      self$dictionary_w <- dictionary_w
      self$alt <- alt
      self$n <- n
      self$compute()
      self
    },

    #' @description
    #' computes the FuzzyTOPSIS problem. Usually doesn't need to be run manually
    #'  as it is called automatically by class constructor.
    compute = function() {
      ncri <- nrow(self$w)
      nalt <- trunc(nrow(self$pm) / ncri)
      # Fuzzy TOPSIS procedure
      fuzzy_weights <- private$cal(self$dictionary_w, self$w, self$n)
      fuzzy_decision_matrix <- private$cal(self$dictionary_pm, self$pm, self$n)
      fuzzy_norm_decision_matrix <- private$fndm(fuzzy_decision_matrix, ncri,
                                                 nalt)
      # weighted fuzzy norm decision matrix: wfndm
      wfndm <- private$weighted_fndm(fuzzy_norm_decision_matrix, fuzzy_weights,
                                     ncri, nalt)
      a_plus <- private$func_dist(wfndm, ncri, nalt, mode = "FPIS")
      names(a_plus) <- self$alt
      a_minus <- private$func_dist(wfndm, ncri, nalt, mode = "FNIS")
      names(a_minus) <- self$alt
      cc <- a_minus / (a_plus + a_minus)
      names(cc) <- self$alt

      # results
      self$fuzzy_weights <- fuzzy_weights
      self$fuzzy_decision_matrix <- fuzzy_decision_matrix
      self$wfndm <- wfndm
      self$a_plus <- a_plus
      self$a_minus <- a_minus
      self$cc <- sort(cc, decreasing = TRUE)
    },

    #' @description
    #' summary of the Fuzzy TOPSIS resutls.
    #'
    #' @return basic information on the model including ranking.
    summary = function() {
      nalt <- length(self$alt)
      ncri <- nrow(self$w)
      cat(paste0("Fuzzy TOPSIS processed ", nalt, " alternatives in ", ncri,
                 " criteria\n\n", "a+:\n"))
      print(self$a_plus, pretty = TRUE)
      cat(paste("\na-:\n"))
      print(self$a_minus, pretty = TRUE)
      cat(paste("\nAlternatives sorted by closeness coef.:\n"))
      print(self$cc, pretty = TRUE)
    }
  ),
  private = list(
    # function to check consistency of matrix againts dictionary of values
    #
    # @param m matrix to be checked
    # @param dict dictionary to check matrix against
    # @param msg message to guide user on what is being examined i.e. PM,
    #   weight matrix, etc.
    # @return TRUE if everything is OK, otherwise stops the computation
    #    (at first problem)
    fuzzy_consistency = function(m, dict, msg = NULL) {
      # check consistency of dictionary (needed to also check consistency
      # of M later)
      if (ncol(dict) != 3) {
        stop(paste("dictionary ", msg,
                   " needs to be defined as stringle fuzzy number (3 columns)"))
      }
      if (!is.numeric(unlist(dict))) {
        stop(paste("Only numeric values expected in ", msg))
      }
      for (i in seq_len(dict)) {
        # check consistency of dictionary - numbers are expected to grow from
        # left to right and from top to bottom
        if (dict[i, 2] < dict[i, 1] || dict[i, 2] > dict[i, 3]) {
          stop(paste("Problem with dictionaryPM inconsistency in row ", i))
        }
        if (i > 1) { # top to bottom check
          for (j in 1:3) {
            if (dict[i, j] < dict[i - 1, j]) {
              stop(paste("Inconsistency in dict. ", msg,
                         " detected on [line, row]: [", i, ",", j,
                         "], rows are expected to go from lowest ",
                         "value to highest."))
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
      t_m <- unlist(m)
      for (j in seq_along(t_m)) { #check for consistency of preference function
        if (!(t_m[j] %in% dict_names)) {
          stop(paste("Dictionary ", msg, " uses value not in the dictionary (",
                     t_m[j], ")"))
        }
      }
      return(TRUE) # every check passed, return TRUE
    }, # end of fuzzyConsistency function

    # Convert the linguistic variables for the criteria weights or the ratings
    # into fuzzy weights and fuzzy decision matrix, respectively
    #
    # @param dict dictionary with the linguistic variables for criteria weights
    #    (or the linguistic variables for the ratings)
    # @param m matrix with criteria weights (or the ratings)
    # @param n number of the decision makers
    # @return fuzzy decision matrix or the fuzzy weights of the criteria
    cal = function(dict, m, n) {
      f <- list()
      for (i in 1:nrow(m)) {
        c <- list()
        for (z in 1:3) {
          x <- 0
          for (j in 1:n) {
            x <- x + dict[m[i, j], z]
          }
          c[[length(c) + 1]] <- round(x / n, 3)
        }
        f[[length(f) + 1]] <- c
      }
      t <- do.call(rbind.data.frame, f)
      colnames(t) <- c("fn1", "fn2", "fn3") #def. fuzzy number
      rownames(t) <- rownames(m) #works fine for w, but line does nothing for PM
      return(t)
    },

    # calculates fuzzy normalized decision matrix
    #
    # @param FDM the fuzzy decision matrix
    # @param n number of criteria
    # @param m number of the alternatives
    # @return fuzzy normalized decision matrix
    fndm = function(fdm, n, m) {
      x <- max(fdm[, 2:3])
      f <- round(fdm / x, 3)
      colnames(f) <- c("fn1", "fn2", "fn3") #triangular fuzzy number
      return(f)
    },

    # Calculate the fuzzy weighted normalized decision matrix
    #
    # @param FNDM fuzzy normalized decision matrix
    # @param w weights
    # @param n number of criteria
    # @param m number of alternatives
    # @return fuzzy weighted normalized decision matrix
    weighted_fndm = function(fndm, w, n, m) {
      w2 <- NULL
      for (i in 1:n) {
        t <- matrix(rep(w[i, ], times = m), ncol = 3, byrow = TRUE)
        w2 <- rbind(w2, t)
      }
      w2 <- as.numeric(w2)
      f <- fndm * w2
      colnames(f) <- c("fn1", "fn2", "fn3")
      return(f)
    },

    # Calculate the distance between two fuzzy triangular numbers
    #
    # @param a bare fuzzy triangular number
    # @param b bare fuzzy triangular number
    # @return distance between a and b
    distance = function(a, b) {
      t <- sqrt(1 / 3 * ((a[1] - b[1])^2 + (a[2] - b[2])^2 + (a[3] - b[3])^2))
      return(t)
    },

    # Determine the fuzzy positive ideal solution (FPIS) or
    # fuzzy negative ideal solution (FNIS)
    #
    # @param fwndm fuzzy weighted normalized decision matrix
    # @param n number of criteria
    # @param m number of the alternatives
    # @return ideal solution of each criterion
    func_dist = function(fwndm, n, m, mode = "FPIS") {
      if (mode == "FPIS") {
        pis <- matrix(data = 1, nrow = 3, ncol = 1)
      } else if (mode == "FNIS") {
        pis <- matrix(data = 0, nrow = 3, ncol = 1)
      } else {
        stop(paste("unexpected mode in func_dist: ", mode))
      }
      dist_pis <- rep(0, times = m)
      for (i in 1:m) { # iterate alternatives
        for (j in 1:n) { # iterate criteria
          if (j == 1) {
            t <- private$distance(fwndm[i, ], pis)
          } else {
            t <- private$distance(fwndm[i + (j - 1) * m, ], pis)
          }
          dist_pis[i] <- dist_pis[i] + t
        }
      }
      return(as.numeric(dist_pis))
    }
  )
)
