#' EVAluation of MIXed data method
#'
#' @description
#' Method has been developed in 1982 by Voogd. It is intended to help with
#'  evaluation of mixed ordinal (O) and numeric - cardinal (C) criteria. The
#'  methods evaluates ordinal and cardinal criteria separately and then
#'  integrates the results to derive final ranking.
#'
#' There are some limitations for the method. It is compensatory method and it
#'  presumes that the criteria are independent.
#'
#' We start by formulating performance matrix of alternatives (rows) in
#'  criteria (columns). Criteria weights need to be provided for computation
#'  purposes. Original EVAMIX method proposes two approaches for weight
#'  derivation, but implementation of EVAMIX does NOT support these, so the
#'  weights are required as a input for the method.
#'
#' In first step we divide original performance matrix into two subsets only
#'  consisting of ordinal or cardinal criteria. We compute supperiority rate of
#'  the alternatives. For ordinal criteria:
#'
#' \mjsdeqn{\alpha_{ii'} = [ \sum_{j \in O} (w_j \cdot sgn(e_{ij} - e_{i'j}))^{\gamma}]^{1/\gamma}}
#'
#' where \mjseqn{e_{ij}} indicates evaluation of alternative Ai based on
#' criterium \mjseqn{c_j} against \mjseqn{e_{i'j}} which evaluates alternative
#' Ai', \mjseqn{w_j} is weight of the criterium j and \mjseqn{\gamma} is
#' arbitrary scaling parameter. Any positive number will work for it, but is
#'  set to 1 by default.
#'
#' In ordinal scales no true distance between the points on this scale are
#'  exactly known. Sgn (signum) part of the equation evaluates only if the
#'  alternative Ai is better then Ai' in that criterium with +1 for yes, 0
#'  for are equal and -1 for Ai' performs better then Ai.
#'
#' Cardinal criteria are evaluated using true difference between alternatives
#' Ai and Ai':
#'
#' \mjsdeqn{a_{ii'} = [ \sum_{j \in C} (w_j \cdot (e_{ij} - e_{i'j}))^{\gamma}]^{1/\gamma}}
#'
#' Since the results for cardinal and ordinal superiority retes are computed
#'  in differnt scale, they need to be normalized first using min-max method
#'  before the total dominance is computed:
#'
#' \mjsdeqn{D_{ii'} = w_O \delta_{ii'} + w_C d_{ii'}}
#'
#' where \mjseqn{w_O = \sum_{j \in O} w_j} and
#'  \mjseqn{w_C = \sum_{j \in C} w_j}.
#'
#' Finally evaluation score of the alternatives is calculated. When ordered
#'  from max to min it provides ranking of the alternatives.
#'
#' \mjsdeqn{S_i = [\sum_{i'} \frac{D_{i'i}}{D_{ii'}}]^{-1}}
#'
#' @references
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords QUALIFLEX
evamix <- R6Class("evamix",
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

    #' @field crit_type vector of criteria types (O or C)
    crit_type = NULL,

    #' @field gamma scaling factor, positive number, 1 by default
    gamma = NULL,

    #' @field finalRank final ranking of the alternatives
    finalRank = NULL,

    #' @field evaluation_score Evaluation score of the alternative
    evaluation_score = NULL,

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
    #' @param crit_type vector of criteria types (O or C)
    #' @param gamma scaling factor, positive number, 1 by default
    #'
    #' @return instance of the class including computed model
    #'
    #' @examples
    #' alternatives <- c("A1", "A2", "A3")
    #' criteria <- c("C1", "C2", "C3", "C4", "C5", "C6")
    #' pm <- rbind(
    #'   c(30, 2, 4, 24, 4, 12.5),
    #'   c(12, 3, 2, 25, 2, 22),
    #'   c(15, 4, 1, 32, 1, 10)
    #' )
    #' rownames(pm) <- alternatives
    #' colnames(pm) <- criteria
    #' minmax <- c("min", "max", "max", "max", "min", "max")
    #' crit_type <- c("C", "O", "O", "C", "O", "C")
    #' w <-c(0.1, 0.175, 0.25, 0.15, 0.125, 0.2)
    #' gamma <- 1
    #' t <- evamix$new(pm, minmax, w, crit_type, gamma)
    initialize = function(pm, minmax = "max", w, crit_type, gamma = 1) {
      # validation of the parameters
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_no_elements_vs_cri(w, ncri, "weights", TRUE)
      validation$validate_no_elements_vs_cri(crit_type, ncri,
                                             "types o criteria", FALSE)
      validation$validate_invalid_val(crit_type, c("C", "O"), "criteria type")
      validation$validate_value_in_interval(gamma, 0, 1, "gamma needs to be in interval <0, 1>")
      # end of validation

      self$pm <- pm
      self$w <- w / sum(w)
      self$gamma <- gamma
      self$crit_type <- crit_type
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
      # splic ordinal (O) and cardinal (C) criteria
      C <- self$pm[, which(self$crit_type == "C")]
      O <- self$pm[, which(self$crit_type == "O")]
      w_c <- self$w[which(self$crit_type == "C")]
      w_o <- self$w[which(self$crit_type == "O")]
      minmax_c <- self$minmax[which(self$crit_type == "C")]
      minmax_o <- self$minmax[which(self$crit_type == "O")]
      # superiority rate
      t <- matrix(0, nrow = nalt, ncol = nalt)
      rownames(t) <- colnames(t) <- alt
      alpha_cl <- list()
      alpha_c <- t
      for (i in 1:ncol(C)) { # criteria in cardinal matrix
        a_t <- t
        for (j in 1:nalt) {
          for (k in 1:nalt) {
            if (minmax_c[i] == "max") {
              a_t[j, k] <- C[j, i] - C[k, i]
            } else {
              a_t[j, k] <- C[k, i] - C[j, i]
            }
          }
        }
        a_t <- (a_t * w_c[i]) ^ self$gamma
        alpha_cl[[i]] <- a_t
      }
      alpha_c <- Reduce("+", alpha_cl) ^ (1 / self$gamma)
      alpha_ol <- list()
      alpha_o <- t
      for (i in 1:ncol(O)) { # criteria in ordina matrix
        a_t <- t
        for (j in 1:(nalt - 1)) {
          for (k in (j + 1):nalt) {
            if (minmax_o[i] == "max") {
              if (O[j, i] > O[k, i]) {
                a_t[j, k] <- 1
                a_t[k, j] <- -1
              } else if (O[j, i] < O[k, i]) {
                a_t[j, k] <- -1
                a_t[k, j] <- 1
              }
            } else {
              if (O[j, i] > O[k, i]) {
                a_t[j, k] <- -1
                a_t[k, j] <- 1
              } else if (O[j, i] < O[k, i]) {
                a_t[j, k] <- 1
                a_t[k, j] <- -1
              }
            }
          }
        }
        a_t <- (a_t * w_o[i]) ^ self$gamma
        alpha_ol[[i]] <- a_t
      }
      alpha_o <- Reduce("+", alpha_ol) ^ (1 / self$gamma)
      # normalization step
      d_c <- private$norm(alpha_c)
      d_o <- private$norm(alpha_o)
      # Total domination
      td <- sum(w_o) * d_o + sum(w_c) * d_c
      # evaluation of alternatives (evaluation score)
      dii <- t
      for (i in 1:(nalt - 1)) {
        for (j in (i + 1):nalt) {
          dii[i, j] <- td[i, j] / td[j, i]
          dii[j, i] <- td[j, i] / td[i, j]
        }
      }
      diag(dii) <- 1
      self$evaluation_score <- colSums(dii)^(-1)
      self$finalRank <- rank(-self$evaluation_score, ties.method = "min")
    },

    #' @description
    #' prepares summary of the EVAMIX method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("EVAMIX results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n\nEvaluation score:\n"))
      print(self$evaluation_score, pretty = TRUE)
      cat(paste("\nFinal rank:\n"))
      print(self$finalRank, pretty = TRUE)
    }
  ),
  private = list(
    # normalization
    #
    # @description
    # normalize the matrix
    #
    # @param to_norm matrix to normalize
    # @return normalized matrix
    norm = function(to_norm) {
      t_min <- min(to_norm)
      t_max <- max(to_norm)
      t <- (to_norm - t_min) / (t_max - t_min)
      return(t)
    }
  )
)