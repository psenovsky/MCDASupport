#' Operational Competitiveness  Ratings  Analysis
#'
#' @description
#' MCDA ranking method based on separate evaluation of the cost and benefit
#'  criteria.
#'
#' After formulating performance matrix, preference ratings with respect to
#'  cost criteria are determined:
#'
#' \mjsdeqn{\overline{I_i} = \sum_{k=1}^q w_k \frac{max(x_i^k) - x_i^k}{min(x_i^k)}}
#'
#' Where q is number fo cost criteria, xik is performance score of i-th
#'  alternative in k-th criterium and w is the weight of criterium.
#'
#' Then linear preference rating for cost criteria is computed:
#'
#' \mjsdeqn{\overline{\overline{I_i}} = \overline{I_i} - min(\overline{I_i})}
#'
#' Same approach is used for benefit criteria:
#'
#' \mjsdeqn{\overline{O_i} = \sum_{h=1}^b w_h \frac{x_i^h - min(x_i^h)}{min(x_i^h)}}
#'
#' \mjsdeqn{\overline{\overline{O_i}} = \overline{O_i} - min(\overline{O_i})}
#'
#' Finally overall performance ratings of competetive alternatives is computed:
#'
#' \mjsdeqn{P_i = \overline{\overline{I_i}} + \overline{\overline{O_i}} - min(\overline{\overline{I_i}} + \overline{\overline{O_i}})}
#'
#' The alternative with highest overal performance is best.
#'
#' @references
#' Miloš Madić, Dušan Petković and Miroslav Radovanović. SELECTION OF
#'  NON-CONVENTIONAL MACHINING PROCESSES USING THE OCRA METHOD. Serbian Journal
#'  of Management 10 (1) (2015) 61 - 73, DOI:10.5937/sjm10-6802
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords OCRA
ocra <- R6Class(
  "ocra",
  public = list(
    #' @field pm performance matrix of alternatives (rpws) in criteria (columns)
    pm = NULL,

    #' @field minmax direction (max or min) of criteria optimization
    minmax = NULL,

    #' @field w weight vector
    w = NULL,

    #' @field Pi overall preference rating (higher better)
    Pi = NULL,

    #' @field rank ranked alternatives, based on Pi
    rank = NULL,

    #' @field result data frame with preference rating and ranking
    result = NULL,

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
    #' res <- ocra$new(pm, w = w, minmax = minmax)
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
    #' performs computation of OCRA model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$pm)
      nalt <- nrow(self$pm)
      cri <- colnames(self$pm)
      alt <- rownames(self$pm)

      # 2. normalization
      cri_max <- cri_min <- rep(0, times = ncri)
      for (j in 1:ncri) {
        cri_max[j] <- max(self$pm[, j])
        cri_min[j] <- min(self$pm[, j])
      }
      cost <- which(self$minmax == "min")
      benefit <- which(self$minmax == "max")
      if (length(cost) > 1) {
        t <- matrix(0, nrow = nalt, ncol = length(cost))
        j2 <- 0
        for (j in cost) {
          j2 <- j2 + 1
          for (i in 1:nalt) {
            t[i, j] <- self$w[j] * (cri_max[j] - self$pm[i, j]) / cri_min[j]
          }
        }
        I <- rowSums(t)
      } else if (length(cost) == 1) {
        I <- self$w[cost] * self$pm[, cost]
      } else {
        I <- rep(0, times = nalt)
      }
      I2 <- I - min(I)
      if (length(benefit) > 1) {
        t <- matrix(0, nrow = nalt, ncol = length(benefit))
        j2 <- 0
        for (j in benefit) {
          j2 <- j2 + 1
          for (i in 1:nalt) {
            t[i, j2] <- self$w[j] * (self$pm[i, j] - cri_min[j]) / cri_min[j]
          }
        }
        O <- rowSums(t)
      } else if (length(benefit) == 1) {
        O <- self$w[benefit] * self$pm[, benefit]
      } else {
        O <- rep(0, times = nalt)
      }
      O2 <- O - min(O)
      Pi <- I2 + O2 - min(I2 - O2)
      names(Pi) <- alt
      rank <- rank(-Pi, ties.method = "min")
      names(rank) <- alt
      
      # results
      self$Pi <- Pi
      self$rank <- rank
      self$result <- data.frame(Pi, rank)
      colnames(self$result) <- c("pref. rating", "rank")
      rownames(self$result) <- alt
    },

    #' @description
    #' summary of the OCRA method resutls.
    #'
    #' @return basic information on the model.
    summary = function() {
      nalt <- nrow(self$pm) # no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "OCRA:\n",
        "processed ", nalt, " alternatives in ", ncri, " criteria\n\nResults"
      ))
      print(self$result, pretty = TRUE)
    }
  )
)