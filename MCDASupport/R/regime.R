#' REGIME
#'
#' @description
#' Simple MADM method for decision support in situation. Proposed by Hinloopen
#'  et al in 1983.
#'
#' As in other methods we start we establishing performance of the alternatives
#'  in criteria. This implementation presumes that the numerical scales are
#'  used.
#'
#' First we compute superiority index. We do so by pairwise comparison of the
#'  alternatives in their respective criteria, identifying such criteria, where
#'  alternative 1 is superior to alternative 2.
#'
#' Next we compute superiority identifier for the pairwise comparisons by
#'  summing weights of the criteria we identified in superior index. (We sum
#'  the weights of the criteria in which alternative 1 is superior to
#'  alternative 2).
#'
#' We construct impact analysis matrix in which we rank all alternatives
#'  separately for each criterium.
#'
#' Based on the ranking we construct REGIME matrix of parwise comparisons of
#'  the alternatives in criteria. We use -1 value if aleternative 1 is lower
#'  rank then alternative 2, 0 if they are equal and +1 if alternative 1 has
#'  better rank then alternative 2.
#'
#' Based on information provided by REGIME metrix guide index is constructed.
#'  WE apply weights to criteria and sum across the criteria to derive
#'  agregated comparison matrix - in REGIME method caled guiding index.
#'
#' Modern interpretation would be to see this matrix as adjacancy matrix
#'  representing comparisons among the alternatives.
#'
#' Based on this information we can derive approximate order of the
#'  alternatives. Basicaly alternatives which have been prefered more times are
#'  better then these prefered lower number of times.
#'
#' Positive guiding index indicates that alternative 1 is better then
#'  alternative 2 (and vice versa).
#'
#' This information can be used to visualize the preference direction, although
#'  Hinloopen did not use this interpretation when he developed the method.
#'
#' @references
#' Hinloopen, E., Nijkamp, P., Rietveld, P. (1983). The Regime Method: A New
#'  Multicriteria Technique. In: Hansen, P. (eds) Essays and Surveys on
#'  Multiple Criteria Decision Making. Lecture Notes in Economics and
#'  Mathematical Systems, vol 209. Springer, Berlin, Heidelberg.
#'  \url{https://doi.org/10.1007/978-3-642-46473-7_13}
#'
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords REGIME MADM
regime <- R6Class("regime",
  public = list(
    #' @field pm performance matrix
    pm = NULL,

    #' @field minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria. The value needs to be provided for all
    #'  numeric criteria (ordinal criteria are already considered processed).
    minmax = NULL,

    #' @field w normalized weights of the criteria
    w = NULL,

    #' @field gi guide index - comparison matrix of the alternatives agregated
    #'  accros the criteria
    gi = NULL,

    #' @field finalRank - final ranking of the alternatives
    finalRank = NULL,

    #' @field graph - visualization of the guide index using network diagram
    graph = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix
    #' @param w normalized weight vector for the criteria
    #' @param minmax optimization direction vector for criteria. Either min or
    #'  max. Can be substituted by single min or max if optimization direction
    #'  is same for all criteria. The value needs to be provided for all
    #'  numeric criteria (ordinal criteria are already considered processed).
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' alternatives <- c("A1", "A2", "A3")
    #' criteria <- c("C1", "C2", "C3", "C4", "C5")
    #' pm <- rbind(
    #'   c(3, 2, 4, 24000, 4),
    #'   c(1.2, 3, 2, 25000, 3),
    #'   c(1.5, 4, 1, 32000, 1)
    #' )
    #' rownames(pm) <- alternatives
    #' colnames(pm) <- criteria
    #' w <- c(0.1, 0.175, 0.25, 0.35, 0.125)
    #' minmax <- c("min", "max", "max", "max", "min")
    #' t <- regime$new(pm, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      # validation of the parameters
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_w_sum_eq_1(w)
      # end of validation

      self$pm <- pm
      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' computes REGIME model based on parameters of the fields of the class.
    #'  Usually run automatically as part of class inititation.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)
      cri <- colnames(self$pm)
      # superiority index (si), superiority identifier (sid)
      t_matrix <- matrix(0, nrow = nalt, ncol = nalt)
      rownames(t_matrix) <- colnames(t_matrix) <- alt
      sid <- t_matrix
      for (i in 1:nalt) {
        for (j in 1:nalt) {
          si <- which((self$minmax == "max" & self$pm[i, ] > pm[j, ]) |
                        (self$minmax == "min" & self$pm[i, ] < pm[j, ]))
          if (length(si) > 0) {
            sid[i, j] <- sum(w[si])
          } else {
            sid[i, j] <- 0
          }
        }
      }
      # impact analysis (r)
      r <- matrix(0, nrow = nalt, ncol = ncri)
      rownames(r) <- alt
      colnames(r) <- cri
      for (i in which(self$minmax == "max")) r[, i] <- rank(-self$pm[, i])
      for (i in which(self$minmax == "min")) r[, i] <- rank(self$pm[, i])
      # regime matrix (rm), for criterion (rm_c)
      rm <- list()
      for (i in 1:ncri) {
        rm_c <- t_matrix
        # OOPORTUNITY: optimize to compute only right triangle of the matrix and derive second half
        for (j in 1:nalt) {
          for (k in 1:nalt) {
            if (r[j, i] < r[k, i]) {
              rm_c[j, k] <- 1
            } else if (r[j, i] > r[k, i]) {
              rm_c[j, k] <- -1
            }
          }
        }
        rm[[i]] <- rm_c
      }
      # Guiding index (gi)
      wrm <- rm
      for (i in 1:ncri) wrm[[i]] <- rm[[i]] * self$w[i]
      self$gi <- Reduce("+", wrm)

      self$finalRank <- rank(-rowSums(self$gi))
      names(self$finalRank) <- alt
      tgi <- self$gi
      tgi[tgi < 0] <- 0
      self$graph <- plot.prefM(tgi)
    },

    #' @description
    #' prepares summary of the REGIME method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("REGIME results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n\nGuiding index:\n"))
      print(self$gi, pretty = TRUE)
      cat(paste("\nFinal rank of alternatives:\n"))
      print(self$finalRank, pretty = TRUE)
      print(self$graph)
    }
  )
)
