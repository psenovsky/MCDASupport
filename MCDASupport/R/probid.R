#' Preference Ranking on the Basis of Ideal-Average Distance (WIP implementation)
#'
#' @description
#' Proposed by Wng et al. (2021). The evaluation works on basis of comparison
#'  of the perfomance of the alternatives to range of reference solutions.
#'  For example TOPSIS works with single ideal and antiideal solution, PROBID
#'  works with tiered ideal solutions - forming m tiers of ideal solutions. (m
#'  is number of alternatives).
#'
#' Additionaly method compares the performance also to average perfomance.
#'
#' In computation we first normalize performance matrix using vector
#'  normalization. Then we apply weights on the normalized performance matrix.
#'
#' To continue with computation we need to construct tiered ideal solution by
#'  sorting weighted normalized performance matrix and compute averages.
#'
#' Euclidean distances of each alternative to each of the ideal solutions as
#'  well as to average needs to be computed.
#'
#' Distance do k-th ideal solution:
#'
#' \mjsdeqn{S_{i(k)} = \sqrt{\sum_{j=1}^n (v_{ij} - v_{(k)j})^2}}
#'
#' Distance to average solution:
#'
#' \mjsdeqn{S_{i(avg)} = \sqrt{\sum_{j=1}^n (v_{ij} - \overline{v_j})^2}}
#'
#' Based on it we can compute overall positive-ideal and negative-ideal
#'  distances.
#'
#' For positive-ideal (pid) when m is odd number:
#'
#' \mjsdeqn{S_{i(pid)} = \sum_{k=1}^{\frac{m+1}{2}}\frac{1}{k}S_{i(k)}}
#'
#' when m is even number:
#'
#' \mjsdeqn{S_{i(pid)} = \sum_{k=1}^{m/2}\frac{1}{k}S_{i(k)}}
#'
#' For negative-ideal (nid) when m is odd number:
#'
#' \mjsdeqn{S_{i(nid)} = \sum_{k=\frac{m+1}{2}}^{m}\frac{1}{m - k + 1}S_{i(k)}}
#'
#' when m is even number:
#'
#' \mjsdeqn{S_{i(nid)} = \sum_{k=\frac{m}{2} + 1}^{m}\frac{1}{m - k + 1}S_{i(k)}}
#'
#' Ideal and anti-ideal solutions then can be used to compute their ratio:
#'
#' \mjsdeqn{R_i = \frac{S_{i(pid)}}{S_{i(nid)}}}
#'
#' which is used to compute performance score P (higher is better):
#'
#' \mjsdeqn{P_i = \frac{1}{1 + R_i^2} + S_{i(avg)}}
#'
#' @references
#' WANG, Zhiyuan, RANGAIAH, Gade Pandu, WANG, Xiaonan. Preference Ranking on
#'  the Basis of Ideal-Average Distance Method for Multi-Criteria
#'  Decision-Making. Industrial & Engineering Chemistry Research. 2021, vol.
#'  60, no. 30, pp. 11216–11230, available from:
#'  https://doi.org/10.1021/acs.iecr.1c01413, ISSN 0888-5885.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords PROBID
probid <- R6Class(
  "probid",
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
    #' alternatives <- c('BLR', 'BOM', 'DEL', 'MNL', 'HYD', 'GRU', 'DUB',
    #'   'KRK', 'MAA', 'EZE')
    #' criteria <- c('tlnt', 'stab', 'cost', 'infl', 'tm-zn', 'infr', "life")
    #' M <- rbind(
    #'   c(0.8181818, 0.1814159, 1.0000000, 0.1198582, 0, 0.6, 0.750),
    #'   c(1.0000000, 0.1814159, 0.6666667, 0.1198582, 0, 0.6, 0.375),
    #'   c(1.0000000, 0.1814159, 0.8333333, 0.1198582, 0, 0.6, 0.125),
    #'   c(0.8181818, 0.0000000, 1.0000000, 0.3482143, 0, 0.6, 0.375),
    #'   c(0.1818182, 0.1814159, 1.0000000, 0.1198582, 0, 0.2, 0.375),
    #'   c(0.1818182, 0.1814159, 0.5000000, 0.1198582, 0, 0.2, 0.125),
    #'   c(0.0000000, 1.0000000, 0.0000000, 0.5741667, 1, 1.0, 1.000),
    #'   c(0.3636364, 0.7787611, 0.6666667, 1.0000000, 1, 0.0, 0.500),
    #'   c(0.4545455, 0.1814159, 0.9166667, 0.1198582, 0, 0.4, 0.000),
    #'   c(0.1818182, 0.6283186, 0.5833333, 0.0000000, 0, 0.4, 0.125)
    #' )
    #' rownames(M) <- alternatives
    #' colnames(M) <- criteria
    #' w <- c(0.125, 0.2, 0.2, 0.2, 0.175, 0.05, 0.05)
    #' t <- probid$new(M, w)
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
        if (nalt %% 2 == 0) { # even number of variants
          for (i in 1:(nalt/2)) { #positive-ideal
            si_pid[k] <- si_pid[k] + (1/k) * sik[k, i]
          }
          for (i in ((nalt/2)+1):nalt) { # negative-ideal
            si_nid[k] <- si_nid[k] + sik[k, i] / (nalt - i + 1)
          }
        } else {
          for (i in 1:((nalt+1)/2)) {
            si_pid[k] <- si_pid[k] + (1/k) * sik[k, i]
          }
          for (i in (((nalt+1)/2)+1):nalt) {
            si_nid[k] <- si_nid[k] + sik[k, i] / (nalt - i + 1)
          }
        }
      }
      names(si_pid) <- alt
      names(si_nid) <- alt
      
      Ri <- si_pid/si_nid
      Pi <- 1/(1 + Ri^2) + si_avg
      rank <- rank(Pi)
      
      result <- data.frame(si_pid, si_nid, Ri, Pi, rank)
      rownames(result) <- alt
      self$result <- result
    },

    #' @description
    #' summary of the PROBID count method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "PROBID:\n",
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