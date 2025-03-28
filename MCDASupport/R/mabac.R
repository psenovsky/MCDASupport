#' Multi-Attribute Border Approximation are Comparison
#'
#' @description
#' Method was introduces by Pamucar and Cirovic in 2015. Its recommendations
#'  are based on alternatives distance from border aproximation area.
#'
#' The precedure starts with normalizong performance matrix of alternatives
#'  (rows) in criteria (columns) using min-max normalization. Then a weighted
#'  normalized matrix is computed
#'
#' \mjsdeqn{\hat{r}_{ij} = w_j + r_{ij}^*w_j}
#'
#' Where r* is normalized perrformance matrix values.
#'
#' Next border approximation area vector (g) is computed
#'
#' \mjsdeqn{g_j = (\prod_{i=1}^m \hat{r}_{ij})^{1/m}}
#'
#' m is number of alternatives being considered. Next the distance from border
#'  approximation area (q) is computed
#'
#' \mjsdeqn{q_{ij} = \hat{r}_{ij} - g_j}
#'
#' Total distance form border approximation area (S) can be computed by summing
#'  rows of matrix q. This measure is also directly applicable for purposes of
#'  ranking by softring the alternatives descentively using this measure.
#'
#' \mjsdeqn{S_i = \sum_{j=1}^n q_{ij}}
#'
#' @references
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords MABAC
mabac <- R6Class("mabac",
  public = list(
    #' @field pm normalized performance matrix. Criteria (in columns) are
    #'  expected to be ordered from most influential to least influential.
    pm = NULL,

    #' @field pm_orig original (not normalized) performance matrix
    pm_orig = NULL,

    #' @field minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria. The value needs to be provided for all
    #'  numeric criteria (ordinal criteria are already considered processed).
    minmax = NULL,

    #' @field w weights vector
    w = NULL,

    #' @field dist_border total distance from border approximation area
    dist_border = NULL,

    #' @field final_rank final ranking of the alternatives based on dist_border
    final_rank = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix. Criteria in columns, ordered from most to
    #'  least influential
    #' @param minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria. The value needs to be provided for all
    #'  numeric criteria (ordinal criteria are already considered processed).
    #' @param w weights vector
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' alternatives <- c("A1", "A2", "A3", "A4")
    #' criteria <- c("C1", "C2", "C3", "C4")
    #' pm <- cbind(
    #'   c(5, 1, 7, 10),
    #'   c(54, 97, 72, 75),
    #'   c(600, 200, 400, 1000),
    #'   c(80, 65, 83, 40)
    #' )
    #' rownames(pm) <- alternatives
    #' colnames(pm) <- criteria
    #' minmax <- c("min", "min", "max", "max")
    #' w <- rep(1, times = 4)
    #' t <- mabac$new(pm, minmax, w)
    initialize = function(pm, minmax = "max", w) {
      # validation of the parameters
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      self$pm <- pm
      for (i in 1:ncri) {
        self$pm[, i] <- mcda_norm(pm[, i], minmax = self$minmax[i],
                                  method = "minmax")
      }
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      # end of validation

      self$pm_orig <- pm
      self$w <- w / sum(w)
      self$compute()
      self
    },

    #' @description
    #' Computed the MABAC using params provided in constructor. Usually run
    #'  automatically by constructor.
    compute = function() {
      ncri <- ncol(self$pm)
      m <- nrow(self$pm)
      pm_w <- self$pm
      # weighted normalized decision matrix
      for (j in 1:ncri) pm_w[, j] <- self$w[j] + self$pm[, j] * self$w[j]
      # border approximation area vector
      g <- apply(pm_w, 2, prod)^(1 / m)
      # distance from border approximated area
      q <- sweep(pm_w, 2, g, "-")
      # total distance from border approximate area
      self$dist_border <- rowSums(q)
      # final ranking
      self$final_rank <- rank(-self$dist_border, ties.method = "max")
    },

    #' @description
    #' prepares summary of the MABAC method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("MABAC method results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n\n",
                "Total distance from border approximate area:\n"))
      print(self$dist_border, pretty = TRUE)
      cat(paste("\nFinal rank:\n"))
      print(self$final_rank)
    }
  )
)