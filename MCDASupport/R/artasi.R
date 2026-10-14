#' Alternative Ranking Technique Based on Adaptive Standardized Intervals
#'
#' @description
#' Decision making method using unique standardization method for criteria based
#'  on establishment of absolute minimum and maximum for the criteria. Considering
#'  performance matrix P the absolute maximum would be computed:
#'
#' \mjsdeqn{P_j^{max} = \max_i (P_{ij}) + [\max_i(P_{ij})]^{1/m}}
#'
#' and absolute minimum:
#'
#' \mjsdeqn{P_j^{min} = \min_i (P_{ij}) - [\min_i(P_{ij})]^{1/m}}
#'
#' where m is number of criteria. Standardization proces then alows the decision
#'  maker to set the interval the criteria will be standardized into, i.e.
#'  [1, 100]. The standardisation is two step process, first:
#'
#' \mjsdeqn{P_{ij}^{norm} = \frac{\psi^{(u)} - \psi^{(l)}}{P_j^{max} - P_j^{min}} P_{ij} + \frac{P_j^{max}\psi^{(l)} - P_j^{min}\psi^{(u)}}{P_j^{max} - P_j^{min}}}
#'
#' where psi (u) and (l) are upper and lower bounds of the interval we are
#'  standardising into. First step is performed on whole performance matrix
#'  regardles of nature of the criteria (for both cost and benefit criteria).
#'  For cost criteria further transformation is required:
#'
#' \mjsdeqn{P_{ij}^{norm2} = - P_{ij}^{norm} + \max_i (P_{ij}^{norm}) + \min_i(P_{ij}^{norm}}
#'
#' Next the method computes the degree of usefulness of alternatives concerning the
#'  ideal and anti-ideal value. Concerning ideal value the degree is computed:
#'
#' \mjsdeqn{\delta_{ij}^+ = \frac{P_{ij}^{norm2}}{\max_i (P_{ij}^{norm2})} w_j \psi^{(u)}}
#'
#' Concerning anti-ideal value the degree is computed:
#'
#' \mjsdeqn{\delta_{ij} = \frac{\min_i(P_{ij}^{norm2})}{P_{ij}^{norm2}} w_j \psi^{(u)}}
#'
#' \mjsdeqn{\delta_{ij}^- = - \delta_{ij} + \max_i (\delta_{ij}) + \min_i (\delta_{ij})}
#'
#' The degrees need to be summed:
#'
#' \mjsdeqn{\Delta_i^+ = \sum_{j=1}^n \delta_{ij}^+, \Delta_i^- = \sum_{j=1}^n \delta_{ij}^-}
#'
#' where n is number of alternatives.
#'
#' Final utility is computed:
#'
#' \mjsdeqn{U_i = (\Delta_i^+ + \Delta_i^-)\{\alpha f(\Delta_i^+)^\varphi + (1-\alpha) f(\Delta_i^-)^\varphi\}^{1/\varphi}; \varphi \in <1;+\infty);\alpha \in [0;1]}
#'
#' where alpha sets influence of ideal and antiideal concerns. Use 0.5 for balanced
#'  approach. Phi is stabilization parameter, 1 by default and
#'
#' \mjsdeqn{f(\Delta_i^+) = \frac{\Delta_i^+}{\Delta_i^+ + \Delta_i^-}, f(\Delta_i^-) = \frac{\Delta_i^-}{\Delta_i^+ + \Delta_i^-}}
#'
#' Utility U can be used directly to derive ranks of evaluated alternatives.
#'
#' @references
#' PAMUCAR, Dragan, SIMIC, Vladimir, GÖRÇÜN, Ömer Faruk, KÜÇÜKÖNDER, Hande.
#'  Selection of the best Big Data platform using COBRAC-ARTASI methodology
#'  with adaptive standardized intervals. Expert Systems with Applications.
#'  2024, vol. 239, article no. 122312, available from:
#'  https://doi.org/10.1016/j.eswa.2023.122312, ISSN 0957-4174.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ARTASI
artasi <- R6Class(
  "artasi",
  public = list(
    #' @field pm performance matrix of alternatives in criteria
    pm = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field w vector containing the weights of the criteria
    w = NULL,

    #' @field limits lower and upper limits for standardization of performance
    #'  matrix (1, 100 by default)
    limits = c(1,100),

    #' @field alpha 0.5 by default, balances ideal and antiideal degrees considerations
    alpha = 0.5,

    #' @field phi 1 by default, computation stabilization factor
    phi = 1,

    #' @field result matrix with utility and assigned ranks for the alternatives
    result = NULL,

    #' @description
    #' public constructor allowing the user to construct Borda count decision
    #'  analysis problem and compute it.
    #'
    #' @param pm Matrix or data frame containing the performance table. Each
    #'  row corresponds to an alternative, and each column to a criterion. only
    #'  numeric values expected. Rows and columns are expected to be named.
    #' @param minmax value or vector of values 'min' or 'max' specifying
    #'  optimization direction for the criterion
    #' @param w vector containing the weights of the criteria
    #' @param limits lower and upper limits for standardization of performance
    #'  matrix (1, 100 by default)
    #' @param alpha 0.5 by default, balances ideal and antiideal degrees considerations
    #' @param phi 1 by default, computation stabilization factor
    #'
    #' @return initialized R6 class with computed results for ARTASI
    #'
    #' @examples
    #' cri <- c("C1", "C2", "C3")
    #' alt <- c("A1", "A2", "A3", "A4", "A5")
    #' pm <- cbind(
    #'   c(5, 10, 4, 8, 1),
    #'   c(4, 8, 2, 16, 4),
    #'   c(7, 7, 6, 8, 5)
    #' )
    #' rownames(pm) <- alt
    #' colnames(pm) <- cri
    #' minmax <- c("max", "max", "min")
    #' result <- artasi$new(pm, minmax)
    initialize = function(pm, minmax = "max", w, limits = c(1,100), alpha = 0.5, phi = 1) {
      ## Validation
      self$pm <- pm
      validation$validate_pm(pm)
      ncri <- ncol(pm)
      self$minmax <- validation$validate_minmax(minmax, ncol(pm))
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_value_in_interval(alpha, 0, 1, "alpha")
      if (!is.numeric(phi) || phi < 0) stop("values of phi expected to be positive")
      if (length(limits) != 2) stop("only 2 element vector expected for limits")
      validation$validate_vector_progression(limits)
      ## end of validation

      self$w <- w
      self$limits <- limits
      self$alpha <- alpha
      self$phi <- phi
      self$compute()
      self
    },

    #' @description
    #' Computes ARTASI model based on parameters specified in constructor.
    #'  Normally this methods does not need to be run manually as constructor
    #'  calls it automatically.
    #'
    #' Manual re-computation is required only if the user changes class' fields
    #'  without using the constructor.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      alt <- rownames(self$pm)

      # standardisation
      # P_{ij}^{norm} = \frac{\psi^{(u)} - \psi^{(l)}}{P_j^{max} - P_j^{min}} P_{ij} + \frac{P_j^{max}\psi^{(l)} - P_j^{min}\psi^{(u)}}{P_j^{max} - P_j^{min}}
      P_max <- P_min <- rep(0, times = ncri)
      pnorm <- self$pm
      d_plus <- d_minus <- d <- pnorm
      for (i in 1:ncri) {
        # absolute min and max
        P_max[i] <- max(self$pm[, i]) + max(self$pm[, i])^(1/ncri)
        P_min[i] <- min(self$pm[, i]) - min(self$pm[, i])^(1/ncri)
        # standardization
        pnorm[, i] <- ((self$limits[2] - self$limits[1]) / (P_max[i] - P_min[i])) * self$pm[, i] + (P_max[i] * self$limits[1] - P_min[i] * self$limits[2]) / (P_max[i] - P_min[i])
        if (self$minmax[i] == "min") {
          # for cost criteria P_{ij}^{norm2} = - P_{ij}^{norm} + \max_i (P_{ij}^{norm}) + \min_i(P_{ij}^{norm}
          # pnorm[, i] <- - pnorm[, i] + (P_max[i] + P_min[i])
          pnorm[, i] <- - pnorm[, i] + max(pnorm[, i]) + min(pnorm[, i])
        }
        # degree of usefulness \delta_{ij}^+ = \frac{P_{ij}^{norm2}}{\max_i (P_{ij}^{norm2})} w_j \psi^{(u)}
        d_plus[, i] <- (pnorm[, i] / max(pnorm[, i])) * self$w[i] * self$limits[2]
        # \delta_{ij} = \frac{\min_i(P_{ij}^{norm2})}{P_{ij}^{norm2}} w_j \psi^{(u)}
        #d[, i] <- (P_min[i] / pnorm[, i]) * self$w[i] * self$limits[2]
        d[, i] <- (min(pnorm[, i]) / pnorm[, i]) * self$w[i] * self$limits[2]
        # \delta_{ij}^- = - \delta_{ij} + \max_i (\delta_{ij}) + \min_i (\delta_{ij})
        d_minus[, i] <- - d[, i] + max(d[, i]) + min(d[, i])
      }

      A_plus <- rowSums(d_plus)
      A_minus <- rowSums(d_minus)
      U <- rep(0, times = nalt)
      names(U) <- alt
      for (i in 1:nalt) {
        U[i] <- (A_plus[i] + A_minus[i]) * (self$alpha * (A_plus[i] / (A_plus[i] + A_minus[i]))^self$phi + (1 - self$alpha) * (A_minus[i] / (A_plus[i] + A_minus[i]))^self$phi)^(1 / self$phi)
      }
      rank <- rank(-U, ties.method = "min")
      result <- data.frame(
        U,
        rank
      )
      colnames(result) <- c("utility", "rank")
      rownames(result) <- alt
      self$result <- result
    },

    #' @description
    #' summary of the ARTASI count method results.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "ARTASI:\n",
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
