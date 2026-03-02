#' Simplified Preference Ranking on the Basis of Ideal-Average Distance (WIP
#'  implementation)
#'
#' @description
#' Simplified version of \link{probid}. The method has first 4 steps same as
#'  PROBID. In step 5 evaluates first and last quarted of ideal solution using
#'  Si_pid (Si positive ideal) and Si_nid (Si negative ideal).
#'
#' For positive solution ideal whem m >= 4:
#'
#' \mjsdeqn{S_{i(pid)} = \sum_{k=1}^{m/4} \frac{1}{k}(S_{i(k)})}
#'
#' when 0 < m < 4:
#'
#' \mjsdeqn{S_{i(pid)} = S_{i1}}
#'
#' For negative ideal solution when m >=4:
#'
#' \mjsdeqn{S_{i(nid)} = \sum_{k=m+1-(m/4)}^{m} \frac{1}{m - k + 1}(S_{i(k)})}
#'
#' when 0 < m < 4
#'
#' \mjsdeqn{S_{i(nid)} = S_{im}}
#'
#' Then the ratio of two Si measures is computed performance score P:
#'
#' \mjsdeqn{P_i = \frac{S_{i(nid)}}{S_{i(pid)}}}
#'
#' Pi measure is directly usable for ranking of the alternatives (higher =
#'  better)
#'
#' @references
#' Baydaş, M., Kavacik, M. Wang, Z. Interpreting the Determinants of
#'  Sensitivity in MCDM Methods with a New Perspective: An Application on E -
#'  Scooter Selection with the PROBID Method. Spectrum of Engineering and
#'  Management Sciences, Vol. 2, No. 1, pp. 17-35, available from:
#'  https://doi.org/10.31181/sems2120242b, ISSN 3009-3309
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords sPROBID
sprobid <- R6Class(
  "sprobid",
  public = list(
    #' @field pm performance matrix (alternatives in rows, criteria in columns)
    pm = NULL,

    #' @field normPM normalized performance matrix (using vector normalization)
    normPM = NULL,

    #' @field w weights vector
    w = NULL,

    #' @field result data frame with distances to ideal solution (si_pid), anti-
    #'  ideal solution (si_nid), distances ratio (Ri), performance score (Pi),
    #'  rank of the alternatives
    result = NULL,

    #' @field minmax vector of optimization directions to the criteria. Use
    #'  max/min only. Single max or min will be interpreted as direction to
    #'  optimize all criteria.
    minmax = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm performance matrix
    #' @param w vector of weights, its sum must be equal to 1
    #' @param minmax minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    #'
    #' @examples
    #' pm <- data.frame(
    #'   C1 = c(0.185, 0.317, 0.555, 0.731, 0.948),
    #'   C2 = c(2.33, 1.08, 6.45, 8.88, 7.39),
    #'   C3 = c(454, 298, 174, 849, 517)
    #' )
    #' rownames(pm) <- c("A1", "A2", "A3", "A4", "A5")
    #' w <- c(0.25, 0.4, 0.35)
    #' minmax <- c("max", "min", "max")
    #' res <- sprobid$new(pm, w = w, minmax = minmax)
    initialize = function(pm, w, minmax = "max") {
      # validity check
      ncri <- ncol(pm)
      self$pm <- pm
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      validation$validate_w_sum_eq_1(w)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of validaty check

      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' performs computation of PROBID model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      cri <- colnames(self$pm)
      alt <- rownames(self$pm)
      ideal <- normPM <- self$pm
      avg <- rep(0, times = ncri)
      # vector normalization
      for (j in 1:ncri) {
        normPM[, j] <- mcda_norm(
          self$pm[, j],
          minmax = self$minmax[j],
          method = "vector"
        )
      }
      # apply weights
      v <- sweep(normPM, MARGIN = 2, self$w, `*`)
      colnames(v) <- cri
      rownames(v) <- alt
      # compute tiered ideal solution
      for (j in 1:ncri) {
        ideal[, j] <- sort(v[, j], decreasing = TRUE)
        avg[j] <- mean(ideal[, j])
      }
      # distance to tiers of ideal solution
      sik <- matrix(0, ncol = nalt, nrow = nalt)
      rownames(sik) <- alt
      si_names <- c()
      si_avg <- rep(0, times = nalt)
      for (k in 1:nalt) {
        si_names <- c(si_names, paste0("Si(", k, ")"))
        for (i in 1:nalt) {
          dif <- 0
          for (j in 1:ncri) {
            dif <- dif + (normPM[i, j] - ideal[k, j])^2
          }
          sik[i, k] <- sqrt(dif)
        }
      }
      colnames(sik) <- si_names
      # distance to average
      for (i in 1:nalt) {
        dif <- 0
        for (j in 1:ncri) {
          dif <- dif + (normPM[i, j] - avg[j])^2
        }
        si_avg[i] <- dif
      }
      names(si_avg) <- alt
      # overal positive ideal and negative ideal distances
      si_nid <- si_pid <- rep(0, times = nalt)
      for (k in 1:nalt) {
        if (nalt >= 4) {
          for (i in 1:(nalt / 4)) {
            #positive-ideal
            si_pid[k] <- si_pid[k] + (1 / k) * sik[k, i]
          }
          for (i in (nalt + 1 - (nalt / 4)):nalt) {
            # negative-ideal
            si_nid[k] <- si_nid[k] + sik[k, i] / (nalt - i + 1)
          }
        } else {
          si_pid[k] <- sik[k, 1] # positive ideal 0 < m < 4
          si_nid[k] <- sik[k, nalt] # negative ideal 0 < m < 4
        }
      }
      names(si_pid) <- alt
      names(si_nid) <- alt

      Pi <- si_nid / si_pid
      rank <- rank(Pi)

      result <- data.frame(si_pid, si_nid, Pi, rank)
      rownames(result) <- alt
      self$result <- result
    },

    #' @description
    #' summary of the sPROBID count method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "sPROBID:\n",
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