#' QUALIFLEX
#'
#' @description
#' Simple MADM method for decision support. Proposed by Paelinck in 1975.
#'  Method presumes we are using independent criteria for comparison for
#'  which weights are known.
#'
#' The method is based on permutation approach and tries to identify the
#'  comparison permutation, which is closest to the values provided in the
#'  performance matrix.
#'
#' Method is using following steps:
#'
#' step 1) initial permutation of the alternatives. Number of permutation
#'  generated is always equal to m!, where m is number of alternatives (i.e.
#'  m = 3, m! = 6)
#'
#' step 2) initial ranking of the alternatives - provided performance matrix
#'  is ranked separately for each criterium
#'
#' step 3) dominant and dominated values - for each permutation in each
#'  criterium the ranking provided by permutation is compared with the one in
#'  performance matrix. Where the permutation agrees with performance matrix
#'  value 1, when it disagrees -1 and when the alternatives in performance
#'  matrix are equal 0 is provided.
#'
#' For example permutation 1 = A2, A1, A3, but true ranking in criterium is
#'  A1 > A2 = A3, hence A2 < A1 = -1
#'
#' step 4) The results are consolidated into matrix of permutation values of
#'  criteria.
#'
#' step 5) permutation values of alternatives is computed as weighted average
#'  of permutation values of criteria.
#'
#' step 6) final ranking is established based on the permutation or
#'  permutations with highest permutation value.
#'
#' @references
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords QUALIFLEX
qualiflex <- R6Class("qualiflex",
  public = list(
    #' @field pm performance matrix. Criteria (in columns) are expected to be
    #'  ordered from most influential to least influential.
    pm = NULL,

    #' @field minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria. The value needs to be provided for all
    #'  numeric criteria (ordinal criteria are already considered processed).
    minmax = NULL,

    #' @field w weight of the criteria
    w = NULL,

    #' @field finalRank final ranking of the alternatives
    finalRank = NULL,

    #' @field permutation_val vermutation value
    permutation_val = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix. Criteria in columns, ordered from most to
    #'  least influential
    #' @param minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria. The value needs to be provided for all
    #'  numeric criteria (ordinal criteria are already considered processed).
    #' @param w weight of the criteria
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' alternatives <- c("A1", "A2", "A3")
    #' criteria <- c("C1", "C2", "C3")
    #' pm <- rbind(
    #'   c(1.2, 18, 32000),
    #'   c(2, 23, 24000),
    #'   c(2, 15, 25000)
    #' )
    #' rownames(pm) <- alternatives
    #' colnames(pm) <- criteria
    #' minmax <- c("min", "max", "max")
    #' w <-c(1, 1, 1)
    #' t <- qualiflex$new(pm, minmax, w)
    initialize = function(pm, minmax = "max", w) {
      # validation of the parameters
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      # end of validation

      self$pm <- pm
      self$w <- w / sum(w)
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
      # permutation generation
      perm <- util_permutations(alt)
      # pos_perm <- private$find_positions(perm, alt)
      # colnames(pos_perm) <- alt
      # initial ranking of alternatives (pos_m)
      t <- matrix(0, nrow = nalt, ncol = ncri)
      rownames(t) <- alt
      colnames(t) <- cri
      pos_m <- t
      for (i in which(self$minmax == "max")) {
        pos_m[, i] <- rank(-self$pm[, i], ties.method = "min")
      }
      for (i in which(self$minmax == "min")) {
        pos_m[, i] <- rank(self$pm[, i], ties.method = "min")
      }
      # permutation values of critera (per_c)
      nperm <- length(perm)
      per_c <- matrix(0, nrow = nperm, ncol = ncri)
      for (i in 1:nperm) {
        p <- perm[[i]]
        for (j in 1:ncri) {
          pv <- 0 # permutation value
          for (k in 1:nalt) {
            if (k == nalt) { # compare last with first
              pk <- p[1]
              pk1 <- p[k]
            } else {
              pk <- p[k]
              pk1 <- p[k + 1]
            }
            if (pos_m[pk, j] < pos_m[pk1, j]) {
              pv <- pv + 1
            } else if (pos_m[pk, j] > pos_m[pk1, j]) {
              pv <- pv - 1
            }
          }
          per_c[i, j] <- pv
        }
      }
      # permutation values for the alternatives
      w_perc <- sweep(per_c, 2, self$w, "*")
      row_sums <- rowSums(w_perc)
      self$permutation_val <- row_sums
      # final ranking
      self$finalRank <- list()
      j <- 0
      for (i in which(row_sums == max(row_sums))) {
        j <- j + 1
        self$finalRank[[j]] <- perm[[i]]
      }
    },

    #' @description
    #' prepares summary of the ORESTE method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("QUALIFLEX results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n\nMax permutation value: ",
                max(self$permutation_val),
                " is consistent for proposed final order:\n"))
      print(self$finalRank, pretty = TRUE)
    }
  )
)