#' Measuring Alternatives by Categorical Based Evaluation Technique
#'
#' @description
#' Developed by Bana e Costa and Vansnick in 1990.
#'
#' Method presumes independent criteria expressed in numeric forms, either
#'  directly or by translating semantic ordinal scale into numeric form.
#'
#' This implementation presumes that all criteria are expressed in such way,
#'  that it is possible to see them as beneficial. Original method lets the
#'  decision maker to do this manual translation as first step of the method.
#'  Method also presumes the weights of the criteria are known and takes them as
#'  input.
#'
#' First true step of the method is establishing reference levels for each
#'  criterium:
#'
#' \mjsdeqn{r_j^- = min(r_{ij})}
#'
#' \mjsdeqn{r_j^+ = max(r_{ij})}
#'
#' then the MACBETH score (v) is computed:
#'
#' \mjsdeqn{v(r_{ij} = v(r_j^-) + \frac{r_{ij} - r_j^-}{r_j^+ - r_j^-}[v(r_j^+) - v(r_j^-)]}
#'
#' where \mjseqn{r_{ij}} represents performance of i-th alternative in j-th
#'  criterium. \mjseqn{v(r_j^-)} and \mjseqn{v(r_j^+)} represents lower and
#'  upper limit of the scale, in which we want the result v to be in and can
#'  for example be set to 0 and 100, which are default values for the parametrs
#'  in this implementation.
#'
#' Overal score is then computer by:
#'
#' \mjsdeqn{V_i = \sum_{j = 1}^n v(r_{ij}) \cdot w_j}
#'
#' Vi then can be ordered in descending manner to identify ranking of the
#'  alternatives.
#'
#' @references
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords MACBETH
macbeth <- R6Class("macbeth",
  public = list(
    #' @field pm performance matrix. Criteria (in columns) are expected to be
    #'  ordered from most influential to least influential.
    pm = NULL,

    #' @field w weight of the criteria
    w = NULL,

    #' @field v_plus ideal MACBETH score (100 by default)
    v_plus = NULL, 

    #' @field v_minus anti-ideal MACBETH score (0 by default)
    v_minus = NULL,

    #' @field v MACBETH score
    v = NULL,

    #' @field finalRank final ranking of the alternatives
    finalRank = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix. Criteria in columns, ordered from most to
    #'  least influential
    #' @param w weight of the criteria
    #' @param v_min minimal MACBETH score (0 by default)
    #' @param v_max maximal MACBETH score (100 by default)
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' alternatives <- c("A1", "A2", "A3", "A4")
    #' criteria <- c("C1", "C2", "C3", "C4", "C5")
    #' pm <- rbind(
    #'   c(4, 3000, 200, 2, 1),
    #'   c(3, 1800, 140, 3, 4),
    #'   c(5, 2200, 230, 1, 3),
    #'   c(2, 2500, 180, 4, 2)
    #' )
    #' rownames(pm) <- alternatives
    #' colnames(pm) <- criteria
    #' w <-c(0.116, 0.207, 0.242, 0.34, 0.095)
    #' t <- macbeth$new(pm, w)
    initialize = function(pm, w, v_min = 0, v_max = 100) {
      # validation of the parameters
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_vector_progression(c(v_min, v_max))
      # end of validation

      self$pm <- pm
      self$w <- w / sum(w)
      self$v_minus <- v_min
      self$v_plus <- v_max
      self$compute()
      self
    },

    #' @description
    #' computes MACBETH model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)
      cri <- colnames(self$pm)
      r_minus <- apply(pm, 2, min)
      r_plus <- apply(pm, 2, max)
      # DEBUG
      print("r-:")
      print(r_minus)
      print("r+:")
      print(r_plus)
      v <- matrix(0, nrow = nalt, ncol = ncri)
      rownames(v) <- alt
      colnames(v) <- cri
      for (i in 1:nalt) {
        for (j in 1:ncri) {
          v[i, j] <- self$v_minus + ((self$pm[i, j] - r_minus[j]) / (r_plus[j] - r_minus[j])) * (self$v_plus - self$v_minus)
        }
      }
      # DEBUG
      print("MACBETH score V")
      print(v)
      vi <- sweep(v, 2, self$w, "*")
      vi <- rowSums(vi)
      self$v <- vi
      self$finalRank <- rank(-self$v, ties.method = "max")
      # DEBUG
      print("Overal score V for alrternatives")
      print(vi)
      print("Final ranking")
      print(self$finalRank)
    },

    #' @description
    #' prepares summary of the MACBETH method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("MACBETH results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n\nOverall score:\n"))
      print(self$v, pretty = TRUE)
      print("Final ranking:")
      print(self$finalRank, pretty = TRUE)
    }
  )
)