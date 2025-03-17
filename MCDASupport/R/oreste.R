#' ORESTE
#'
#' @description
#' Simple MADM method for decision support in situation. Proposed by Roubens in
#'  1983.
#'
#' The method is based on computation of the distance of the rank of the
#'  alternative A in criterium X to rank 0 (optimum).
#'
#' Method takes as input perfomance matrix, criteria (in columns) must be
#'  ordered from the most important one to least important one. So for example
#'  first criterium is more important than the second, etc.
#'
#' Next we create position matrix, in which we rank the alternatives for each
#'  criterium separately. This information is used for computation of block
#'  distances:
#'
#' \mjsdeqn{d(0, A_{ij}) = \alpha r_{ij} + (1 - \alpha) r_j}
#'
#' Where r ij is the ran of the alternative i in criterium j, rj then means
#'  column, where the criterium i located. The positioning is important as all
#'  critera are ordered by their significance, making criterium 1 the most
#'  important one, with r1 = 1 (second criterium r2 = 2, ...).
#'
#' The results are writen to block distance matrix.
#'
#' Value alpha presents succession rate. Its values must always be in interval
#'  (0;1), but usually we are choosing aplpha such that
#'
#' \mjsdeqn{1/3 \le \alpha \le 1/2}
#'
#' For final ranking we presume that lower distances to the theoretical optimum
#'  will be represenative of better rank of such alternative.
#'
#' \mjsdeqn{d(0, A_{ij}) \le d(0, A_{i'j})}
#'
#' \mjsdeqn{R(A_{ij}) \le R(A_{ij})}
#'
#' Value of R is derived from values of computed block distances. We order all
#'  of these from lowest to highest and rank these values. We then substitute
#'  the values in block distance matrix with the rank assigned to this distance
#'  in previous step.
#'
#' We can compute the final value for ranking by aggregating these distance
#'  ranks for the alternative across the criteria.
#'
#' \mjsdeqn{R(A_{i}) = \sum_{j = 1}^n R(A_{ij})}
#'
#' Resulting R(A) can be directly used for ranking purposes (highest value is
#'  best).
#'
#' @references
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ORESTE MADM
oreste <- R6Class("oreste",
  public = list(
    #' @field pm performance matrix. Criteria (in columns) are expected to be
    #'  ordered from most influential to least influential. 
    pm = NULL,

    #' @field minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria. The value needs to be provided for all
    #'  numeric criteria (ordinal criteria are already considered processed).
    minmax = NULL,

    #' @field alpha succession rate in interval <0, 1>, usually in <0.33, 0.5>
    alpha = NULL,

    #' @field finalRank - final ranking of the alternatives
    finalRank = NULL,

    #' @field ra total ranking of the alternatives
    ra = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix. Criteria in columns, ordered from most to
    #'  least influential
    #' @param minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria. The value needs to be provided for all
    #'  numeric criteria (ordinal criteria are already considered processed).
    #' @param alpha succession rate in interval <0, 1>, usually in <0.33, 0.5>
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' alternatives <- c("A1", "A2", "A3")
    #' criteria <- c("C1", "C2", "C3")
    #' pm <- rbind(
    #'   c(4, 3, 3),
    #'   c(2, 1.2, 2),
    #'   c(1, 1.5, 4)
    #' )
    #' rownames(pm) <- alternatives
    #' colnames(pm) <- criteria
    #' minmax <- c("max", "min", "max")
    #' alpha <- 0.4
    #' t <- oreste$new(pm, minmax, alpha)
    initialize = function(pm, minmax = "max", alpha = 0.4) {
      # validation of the parameters
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_value_in_interval(alpha, 0, 1, "succession rate")
      if (alpha < 0.33 | alpha > 0.5) {
        print("Warning: alpha value usually in interval <0.33, 0.5>")
      }
      # end of validation

      self$pm <- pm
      self$alpha <- alpha
      self$compute()
      self
    },

    #' @description
    #' computes ORESTE model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)
      cri <- colnames(self$pm)
      # position matrix (pos_m)
      t <- matrix(0, nrow = nalt, ncol = ncri)
      pos_m <- t
      for (i in which(self$minmax == "max")) pos_m[, i] <- rank(-self$pm[, i])
      for (i in which(self$minmax == "min")) pos_m[, i] <- rank(self$pm[, i])
      # block distance matrix (bdm)
      bdm <- t
      for (i in 1:nalt) {
        for (j in 1:ncri) {
          bdm[i, j] <- alpha * pos_m[i, j] + j * (1 - alpha)
        }
      }
      unique_values <- sort(unique(bdm))
      ranked_values <- rank(unique_values)
      lookup_table <- setNames(ranked_values, unique_values)
      bdm2 <- matrix(lookup_table[as.character(bdm)], nrow = nalt, ncol = ncri)
      self$ra <- rowSums(bdm2)
      self$finalRank <- rank(-self$ra, ties.method = max)
    },

    #' @description
    #' prepares summary of the ORESTE method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("ORESTE results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n\nTotal ranking:\n"))
      print(self$ra, pretty = TRUE)
      cat(paste("\nFinal rank of alternatives:\n"))
      print(self$finalRank, pretty = TRUE)
    }
  )
)