#' Simultaneous Evaluation of Criteria and Alternatives method
#'
#' @description
#' The SECA (Simultaneous Evaluation of Criteria and Alternatives) method is an
#'  objective Multi-Criteria Decision-Making (MCDM) tool designed to determine
#'  alternative rankings and criteria weights concurrently. Unlike methods that
#'  rely on subjective human input for weighting, SECA utilizes a mathematical
#'  optimization model that balances two key vectors: the internal consistency
#'  of the alternatives (captured through standard deviation) and the conflict
#'  between criteria (captured through correlation). By maximizing these two
#'  measures, the method minimizes the bias inherent in decision-maker
#'  preferences, making it particularly effective for complex engineering or
#'  management problems where an impartial, data-driven perspective is
#'  required.
#'
#' The procesure start with normalization of decision matrix using max
#'  normaliation (see \link{mcda_norm}). Then standard deviation of the criteria
#'  and correlation between each pair of vectors of criteria (r_ij) and is used
#'  for computing a degree of conflict:
#'
#' \mjsdeqn{\pi_j = \sum_{l=1}^n(1 - r_{jl})}
#'
#' Values of standard deviation and degree of conflict are vector normalized,
#'  thus establishing reference points.
#'
#' The formulate solution to the decision problem multi-objective non-linar
#'  problem needs to be solved:
#'
#' \mjsdeqn{max S_j = \sum_{j=1}^n w_j\sigma_j \sum_{j=1}^n w_j \pi_j}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @references
#' Shankar Chakraborty, Prasenjit Chatterjee, Partha Protim Das. SIMULTANEOUS
#'  EVALUATION OF CRITERIA AND ALTERNATIVES(SECA) METHOD. In Multi-Criteria
#'  Decision-Making Methods in Manufacturing Environments. Apple Academic
#'  Press: New York, 2023, pp. 325-335, ISBN 9781003377030.
seca <- R6Class(
  "seca",
  public = list(
    #' @field pm performance matrix
    pm = NULL,

    #' @field w vector of weights
    w = NULL,

    #' @field score to evaluate the results
    score = NULL,

    #' @field minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    minmax = NULL,

    #' @field beta beta (0.5 by default)
    beta = 0.5,

    #' @field ref_points dataframe with information on computed reference points
    ref_points = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm normalized performance matrix
    #' @param minmax minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    #' @param beta beta
    initialize = function(pm, minmax = "max", beta = 0.5) {
      # validity check
      ncri <- ncol(pm)
      self$pm <- pm
      validation$validate_pm(pm)
      # validation$validate_no_elements_vs_cri(w, ncri, "weights")
      # validation$validate_w_sum_eq_1(w)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_scalar_numeric(beta, "beta")
      if (beta < 0.1) {
        stop("Beta expected to be greater or equal to 0.1.")
      }
      # end of validaty check

      # self$w <- w
      self$beta <- beta
      self$compute()
      self
    },

    #' @description
    #' performs computation of SECA model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      cri <- colnames(self$pm)
      alt <- rownames(self$pm)
      epsilon <- 1e-4

      pm_norm <- self$pm
      pi <- st_dev <- rep(0, times = ncri)
      r <- matrix(0, ncol = ncri, nrow = ncri)
      for (i in 1:ncri) {
        pm_norm[, i] <- mcda_norm(
          self$pm[, i],
          method = "tobest",
          minmax = self$minmax[i]
        )
        st_dev[i] <- sd(pm_norm[, i])
      }
      r <- cor(pm_norm)
      r[is.na(r)] <- 0
      diag(r) <- 0
      pi <- rowSums(1 - r)

      ref_sd <- st_dev / sum(st_dev)
      ref_pi <- pi / sum(pi)

      ref_points <- cbind(st_dev, pi, ref_sd, ref_pi)
      colnames(ref_points) <- c(
        "st. dev.",
        "deg. of conflict",
        "ref. st. dev",
        "reg. deg. of conf."
      )
      rownames(ref_points) <- cri
      self$ref_points <- ref_points

      # Dmat
      Dmat <- matrix(0, ncri + 1, ncri + 1)
      diag(Dmat)[1:ncri] <- 4 * self$beta
      Dmat[ncri + 1, ncri + 1] <- 1e-8

      # dvec
      dvec <- c(
        2 * self$beta * (ref_sd + ref_pi),
        1
      )

      # Constraints
      A_alt <- cbind(pm_norm, -1)
      b_alt <- rep(0, nalt)

      A_eq <- rbind(
        c(rep(1, ncri), 0),
        c(rep(-1, ncri), 0)
      )
      b_eq <- c(1, -1)

      # A_lb <- diag(ncri)
      A_lb <- cbind(
        diag(ncri), # w_j
        rep(0, ncri) # φ_a
      )
      b_lb <- rep(epsilon, ncri)

      Amat <- t(rbind(A_alt, A_eq, A_lb))
      bvec <- c(b_alt, b_eq, b_lb)

      # Solve
      sol <- solve.QP(Dmat, dvec, Amat, bvec)

      w_seca <- sol$solution[1:ncri]
      names(w_seca) <- cri
      phi_a <- sol$solution[ncri + 1]
      scores <- as.vector(pm_norm %*% w_seca)
      names(scores) <- alt
      self$w <- w_seca
      self$score <- scores
    },

    #' @description
    #' prepares summary of the SECA method resutls and outputs them
    #'  to the console.
    summary = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      cat(paste(
        "SECA:\nProcessed, ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria with beta = ",
        self$beta,
        " used.\nComputed wegiths:\n"
      ))
      print(self$w, pretty = TRUE)
      cat("\nComputed score:\n")
      print(self$score, pretty = TRUE)
    }
  )
)