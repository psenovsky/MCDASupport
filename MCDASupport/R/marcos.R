#' Measurement of Alternatives and Ranking According to Compromise Solution
#'  (MARCOS) Method
#'
#' @description
#' MACROS compares different clusters of alternatives in different criteria to
#'  achieve ranking of the alternatives.
#'
#' Marcos uses 7 step approach outlined bellow to compute the results.
#'
#' step 1) construction of initial decision matrix.
#'
#' In this matrix the normal performance matrix of the altrnatives in criteria
#'  is used. As in other functions in the package PM has numeric values only
#'  and m alternatives are in rows and n criteria are in columns.
#'
#' Initial PM is taken as the input of the model.
#'
#' step 2) Obtained extended decision matrix
#'
#' In original MARCOS implementation two rows are added to performance matrix
#'  PM representing ideal (AI) and anti-ideal (AAI) values in criteria, but
#'  this implementation works slightly differently, leaving ai and aai as
#'  separate vectors. Computation proces for AI and AAI remains same:
#'
#' For benefit criteria
#'
#' \mjsdeqn{AAI = min_i x_{ij}}
#'
#' \mjsdeqn{AI = max_i x_{ij}}
#'
#' For cost criteria
#'
#' \mjsdeqn{AAI = max_i x_{ij}}
#'
#' \mjsdeqn{AI = min_i x_{ij}}
#'
#' step 3) standardizing extended decision matrix
#'
#' In this step obtained AI values are used to standardize performance matrix
#'  for cost criteria
#'
#' \mjsdeqn{n_{ij} = \frac{ai_{j}}{x_{ij}} = \frac{x_{ij}}{aai_j}}
#'
#' and benefit criteria
#'
#' \mjsdeqn{n_{ij} = \frac{x_{ij}}{ai_{j}} = \frac{aai_j}{x_{ij}}}
#'
#' where j corresponds to columns (criteria) of the matrix.
#'
#' step 4) weigted standardized decision matrix
#'
#' is obtained by multiplying standarzized performance matrix by criteria
#'  weights.
#'
#' \mjsdeqn{v_{ij} = n_{ij} w_j}
#'
#' step 5) calculation of benefits of the alternatives
#'
#' is derived from ratio between summed weighted performance of the alternative
#'  and summed weighted ideal and anti-ideal alternatives:
#'
#' \mjsdeqn{K^- = \frac{S_i}{S_{aai}}}
#'
#' \mjsdeqn{K^+ = \frac{S_i}{S_{ai}}}
#'
#' \mjsdeqn{S_i = \sum_{i=1}^n v_{ij}}
#'
#' step 6) calculation of benefit functions of alternatives
#'
#' Computerd ratios are then used to compute benefit function f(Ki). This
#'  function represents a solution of the decision problem.
#'
#' \mjsdeqn{f(K_i) = \frac{K_i^+ + K_i^-}{1 + \frac{1-f(K_i^+)}{f(K_i^+)} + \frac{1-f(k_i‘^-)}{f(K_i^-)}}}
#'
#' \mjsdeqn{f(K_i^-) = \frac{K_i^+}{K_i^+ + K_i^-}}
#'
#' \mjsdeqn{f(K_i^+) = \frac{K_i^-}{K_i^+ + K_i^-}}
#'
#' step 7) ranking of alternatives
#'
#' Is directly derived from value of f(Ki) from step 6). Ranking of the
#'  alternatives is obtained by ordering the alternatives decreasingly using
#'  f(Ki). Lazgest value of f(Ki) represents best solution of the decision
#'  problem.
#'
#' @references
#' Demir, G. et al.: Measurement of Alternatives and Ranking According to
#'  Compromise Solution (MARCOS) Method: A Comprehensive Bibliometric Analysis,
#'  Decision Making: Applications in MAnagement and Engineering, vol. 7, no. 2
#'  (2024), pp. 313-336, ISSN 2620-0104, DOI: 10.31181/dmame7220241137.
#'
#' Thinh, Hoang Xuan and Trung, Do Duc, A Research on Application of the
#'  Measurement of Alternatives and Ranking According to Compromise Solution
#'  Method for Multi-Criteria Decision Making in the Grinding Process (March
#'  31, 2022). EUREKA: Physics and Engineering, (2), 101–110, 2022. doi:
#'  \url{https://doi.org/10.21303/2461-4262.2022.002120}, Available at SSRN:
#'  \url{https://ssrn.com/abstract=4073069}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords MARCOS
marcos <- R6Class("marcos",
  public = list(

    #' @field pm original (not normalized) performance matrix
    pm = NULL,

    #' @field w weights of the criteria
    w = NULL,

    #' @field minmax vector of criteria direction to be used during
    #'  performance matrix normalization. Can be replaced with single max or
    #'  min if all criteria are to be maximized or minimized. Implicitly set
    #'  to max.
    minmax = NULL,

    #' @field ranking ordered results expresed as benefit function f(K), from
    #'  best to worst
    ranking = NULL,

    #' @description
    #' Public constructor of the class. Validates the input parameters and
    #'  runs the model computation.
    #'
    #' @param pm original (not normalized) performance matrix
    #' @param w weights of the criteria
    #' @param minmax vector of criteria direction to be used during
    #'  performance matrix normalization. Can be replaced with single max or
    #'  min if all criteria are to be maximized or minimized. Implicitly set
    #'  to max.
    #'
    #' @return
    #' initialized R6 class representing MARCOS model inputs and solution
    #'
    #' @examples
    #' PM <- rbind(
    #'   c(0.446, 1.785, 6.643, 6.843, 50),
    #'   c(1.113, 2.425, 10.525, 2.902, 250),
    #'   c(1.246, 3.321, 14.224, 3.885, 600),
    #'   c(1.935, 3.678, 17.852, 4.406, 1000),
    #'   c(0.446, 3.062, 5.238, 3.112, 200),
    #'   c(1.064, 3.814, 14.558, 4.121, 250),
    #'   c(1.654, 4.581, 17.888, 4.886, 1600),
    #'   c(1.924, 5.226, 22.224, 5.702, 1500),
    #'   c(0.337, 4.444, 24.708, 4.123, 450),
    #'   c(0.998, 5.12, 18.012, 5.206, 1500),
    #'   c(1.622, 5.886, 22.226, 6.226, 600),
    #'   c(1.844, 6.234, 26.128, 6.786, 1500),
    #'   c(0.531, 5.6, 18.883, 5.405, 800),
    #'   c(1.023, 6.123, 21.987, 6.501, 1500),
    #'   c(1.664, 7.244, 27.012, 7.421, 1600),
    #'   c(2.012, 7.345, 28.021, 7.923, 1000)
    #' )
    #' rownames(PM) <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9",
    #'                   "A10", "A11", "A12", "A13", "A14", "A15", "A16")
    #' colnames(PM) <- c("Ra", "Ax", "Ay", "Az", "Q")
    #' w <- rep(0.2, times = 5) #equal weights
    #' minmax <- "max"
    #' t <- marcos$new(PM, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      # validation of the parameters
      self$minmax <- param_check_marcos(pm, w, minmax)
      # end of validation

      self$pm <- pm
      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' computes MARCOS model based on input parameters. Usualy it is not
    #'  neccessary to run this function as it is run automaticaly by the
    #'  class constructor
    compute = function() {
      ncri <- length(self$w)
      pm2 <- self$pm
      # 2) obtain ai and aai
      aai <- ai <- rep(0, times = ncri)
      n_aai <- n_ai <- rep(0, times = ncri)
      for (i in 1:ncri) {
        if (self$minmax[i] == "max") { # benefit criteria
          ai[i] <- max(self$pm[, i])
          aai[i] <- min(self$pm[, i])
          # step 3) standardization
          pm2[, i] <- pm2[, i] / ai[i]
          n_ai[i] <- ai[i] / ai[i]
          n_aai[i] <- aai[i] / ai[i]
        } else { # cost criteria
          ai[i] <- min(self$pm[, i])
          aai[i] <- max(self$pm[, i])
          # step 3) standardization
          pm2[, i] <- ai[i] / pm2[, i]
          n_ai[i] <- ai[i] / ai[i]
          n_aai[i] <- ai[i] / aai[i]
        }
      }
      # step 4) weighted standardized PM
      w_pm <- pm2 %*% diag(self$w)
      w_ai <- n_ai * self$w
      w_aai <- n_aai * self$w
      # step 5) calculation of benefits of alternatives
      r_sum <- rowSums(w_pm)
      K_minus <- r_sum / sum(w_aai)
      K_plus <- r_sum / sum(w_ai)
      # step 6) calculation of benefit functions of alternatives
      fK_minus <- K_plus / (K_plus + K_minus)
      fK_plus <- K_minus / (K_plus + K_minus)
      fK <- (K_plus + K_minus) / (1 + ((1 - fK_plus) / fK_plus) + ((1 - fK_minus) / fK_minus))
      names(fK) <- rownames(pm2)

      order_indices <- order(fK, decreasing = TRUE)
      self$ranking <- fK[order_indices]
    },

    #' @description
    #' summary of the MACROS method resutls.
    #' @return basic information on the model including ranking.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- length(self$w)
      cat(paste0("MARCOS method\n processed ", nalt, " alternatives in ", ncri,
                 " criteria\n\nRanking using benefit function:\n"))
      print(self$ranking, pretty = TRUE)
    }

  )
)