#' Reference Ideal Model
#'
#' @description
#' Uses \link{topsis} approach to recommending optimal solution to the problem.
#'  Unlike TOPSIS, the RIM method uses its own approach to normalization (RIM
#'  normalization, see \link{rim_norm}).
#'
#' After normalization step RIM follows TOPSIS approach. It applies weights,
#'  calculates the variation to the normalized reference ideal for each
#'  alternative.
#'
#' \mjsdeqn{I_i^+ = \sqrt{\sum_{j=1}^n (y_{ij} - w_j)^2}}
#'
#' \mjsdeqn{I_i^- = \sqrt{\sum_{j=1}^n (y_{ij})^2}}
#'
#' The y is normalized weighted performance matrix. Finally we compute relative
#'  index for each alternative.
#'
#' \mjsdeqn{R_i = \frac{I_i^-}{I_i^+ + I_i^-}}
#'
#' Final ranking of the alternatives can be derived from Ri by sorting it
#'  descending.
#'
#' @references
#' CABLES, E., LAMATA, M. T., VERDEGAY, J. L. RIM-reference ideal method in
#'  multicriteria decision making. Information Sciences. 2016, Vol. 337–338,
#'  pp. 1–10, available from: https://doi.org/10.1016/j.ins.2015.12.011,
#'  ISSN 0020-0255.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords RIM
rim <- R6Class(
  "rim",
  public = list(
    #' @field pm performance matrix (alternatives in rows, criteria in columns)
    pm = NULL,

    #' @field normPM normalized performance matrix
    normPM = NULL,

    #' @field A numeric vector of lower ranges of criteria
    A = NULL,

    #' @field B numeric vector of upper ranges of the criteria
    B = NULL,

    #' @field C numeric vector of lower limit of reference ideal
    C = NULL,

    #' @field D numeric vector of upper limit of reference ideal
    D = NULL,

    #' @field w numeric weight vector
    w = NULL,

    #' @field result data frame with variances (I+, I-), relative index and
    #'  ranking of the alternatives
    result = NULL,

    #' @description
    #' public constructor allowing the user to construct Borta count decision
    #'  analysis problem and compute it.
    #'
    #' @param pm Matrix or data frame containing the performance table. Each
    #'  row corresponds to an alternative, and each column to a criterion. only
    #'  numeric values expercted. Rows and columns are expected to be named.
    #' @param A numeric vector of lower ranges of criteria
    #' @param B numeric vector of upper ranges of the criteria
    #' @param C numeric vector of lower limit of reference ideal
    #' @param D numeric vector of upper limit of reference ideal
    #' @param w numeric weight vector
    #'
    #' @return initialized R6 class with computed results
    #'
    #' @examples
    #' pm <- data.frame(
    #'   age = c(25, 30, 35, 40, 50),
    #'   pay = c(40000, 45000, 38000, 44000, 37000),
    #'   qualification = c(1, 2, 3, 5, 5))
    #' rownames(pm) <- c("A1", "A2", "A3", "A4", "A5")
    #' A <- c(23, 35000, 1)
    #' B <- c(60, 50000, 5)
    #' C <- c(30, 35000, 4)
    #' D <- c(35, 42000, 5)
    #' w <- c(0.25, 0.45, 0.3)
    #' t <- rim$new(pm, A, B, C, D, w)
    initialize = function(pm, A, B, C, D, w) {
      ## Validation
      self$pm <- pm
      validation$validate_pm(pm)
      normPM <- pm
      ncri <- ncol(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_w_sum_eq_1(w)
      for (j in 1:ncri) {
        normPM[, j] <- rim_norm(pm[, j], A[j], B[j], C[j], D[j])
      }
      # end of validation

      self$A <- A
      self$B <- B
      self$C <- C
      self$D <- D
      self$normPM <- normPM
      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' Computes RIM based on parametrs specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation si required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)

      # 4. apply weight
      weighted_pm <- as.data.frame(sweep(self$normPM, 2, self$w, "*"))
      # 5. calculate variantions to normalized reference ideal
      I_minus <- I_plus <- rep(0, times = nalt)
      for (j in 1:ncri) {
        I_plus <- I_plus + (self$normPM[, j] - self$w[j])^2
        I_minus <- I_minus + (self$normPM[, j])^2
      }
      I_plus <- sqrt(I_plus)
      I_minus <- sqrt(I_minus)
      # 6. calcutate relative index
      Ri <- I_minus / (I_plus + I_minus)
      rank <- rank(-Ri, ties.method = "min")

      result <- data.frame(I_plus, I_minus, Ri, rank)
      rownames(result) <- alt
      self$result <- result
    },

    #' @description
    #' summary of the RIM method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "RIM:\n",
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