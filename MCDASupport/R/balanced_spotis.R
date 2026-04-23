#' Balanced Stable Preference Ordering Super Intelligence
#'
#' @description
#' introduces Expected solution point (ESP) and alfa confidence coefficient
#'  into the SPOTIS method.
#'
#' Uses distance d(Ai, S*) as in normal SPOTIS and d(Ai, S+), where S+ is ESP
#'  measured as best provided performance in the criterium.
#'
#' Alpha then denotes confidence of decision maker to púrovided values. The
#'  coeficient is always in interval <0; 1>, balancing d(Ai, S*) and d(Ai, S+)
#'  components:
#'
#' \mjsdeqn{P_i = \alpha \cdot d(A_i, S^+) + (1 - \alpha) \cdot d(A_i, S^*)}
#'
#' @references
#' Shekhovtsov, A., Dezert, J. Sałabun, W. Enhancing Personalized Decision-Making
#'  with the Balanced SPOTIS Algorithm. In Proceedings of the 17th International
#'  Conference on Agents and Artificial Intelligence (ICAART 2025) - Vol. 3,
#'  pp. 264-271, DOI: 10.5220/0013119800003890, ISBN: 978-989-758-737-5;
#'  ISSN: 2184-433X
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords B-SPOTIS
balanced_spotis <- R6Class(
  "balanced_spotis",
  public = list(
    #' @field pm performance matrix of alternatives in criteria
    pm = NULL,

    #' @field w vector of weights
    w = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field bounds dataframe of bounds of criteria (criteria in rows with
    #'   min and max columns)
    bounds = NULL,

    #' @field alpha confidence value for pm (in inteval <0; 1>)
    alpha = 0.5,

    #' @field  esp Expected solution point as vector of best values of the
    #'  criteria
    esp = NULL,

    #' @field score_raw vector of average distance to ideal solution
    score_raw = NULL,

    #' @field score vector of average distance to ideal solution ordered
    score = NULL,

    #' @description
    #' public constructor allowing the user to construct SPOTIS decision
    #'  analysis problem and compute it.
    #' @param pm performance matrix of alternatives in criteria
    #' @param w vector of weights
    #' @param minmax direction (max or min) of criteria optimization
    #' @param bounds dataframe of bounds of criteria (criteria in rows with
    #'   min and max columns)
    #' @param alpha confidence value for pm (in inteval <0; 1>)
    #'
    #' @return initialized R6 class with computed results for SPOTIS
    #'
    #' @examples
    #' # Example from the book (see references)
    #' PM <- rbind(
    #'    c(10.5, -3.1, 1.7),
    #'    c(-4.7, 0, 3.4),
    #'    c(8.1, 0.3, 1.3),
    #'    c(3.2, 7.3, -5.3)
    #' )
    #' rownames(PM) <- c('A1', 'A2', 'A3', 'A4')
    #' colnames(PM) <- c('C1', 'C2', 'C3')
    #' minmax <- c('max', 'min', 'max')
    #' w <- c(0.2, 0.3, 0.5)
    #' bounds <- rbind(
    #'    c(-5, 12),
    #'    c(-6, 10),
    #'    c(-8, 5)
    #' )
    #' rownames(bounds) <- colnames(PM)
    #' colnames(bounds) <- c('min', 'max')
    #' result <- balanced_spotis$new(PM, w, minmax, bounds, alpha = 0.4)
    initialize = function(pm, w, minmax = "max", bounds, alpha = 0.5) {
      ## check validity of the objects manipulated by the current function
      self$pm <- pm
      validation$validate_matrix_numeric(pm)
      ncri <- ncol(pm) # no. of criteria
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_matrix_numeric(bounds)
      validation$validate_value_in_interval(alpha, 0, 1, "alpha")
      if (nrow(bounds) != ncri || ncol(bounds) != 2) {
        stop("Dimensions of the matrix 'bounds' are not correct.")
      }
      ## End of checking the validity of the "inputs"
      esp <- bounds
      for (i in 1:ncri) {
        esp[i, 1] <- min(pm[, i])
        esp[i, 2] <- max(pm[, i])
      }

      self$w <- w / sum(w)
      self$bounds <- bounds
      self$alpha <- alpha
      self$esp <- esp
      self$compute()
      self
    },

    #' @description
    #' Computes Balanced SPOTIS model based on parametrs specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation si required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      s2 <- spotis$new(self$pm, self$w, self$minmax, self$esp)
      self$score_raw <- self$alpha *
        s2$score_raw +
        (1 - self$alpha) * s2$score_raw
      self$score <- self$score_raw[order(self$score_raw)]
    },

    #' @description
    #' prepares summary of the balanced SPOTIS method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "Balanced SPOTIS:\n",
        "processed ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria. With confidence coefficient: ",
        self$alpha,
        "\nResults\n"
      ))
      print(self$score, pretty = TRUE)
    }
  )
)