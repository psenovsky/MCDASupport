#' Root Assesment Method
#'
#' @description
#' A simple method similar to ones like \link{moora}, \link{moosra} and
#'  \link{copras}.
#'
#' The method first normalizes all values using max version of vector
#'  normalization, then the weights are applied.
#'
#' Method then separately computes scores for beneficial and cost criteria
#'  (S+i, S-i). Aggregation procedure is what makes the method differenc to
#'  the likes of MOORA, etc.:
#'
#' \mjsdeqn{RI_i = \sqrt[2+S_{-i}]{2 + S_{+i}}}
#'
#' The RI measdure is durectly usable for ranking purposes.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @references
#' SOTOUDEH-ANVARI, Alireza. Root Assessment Method (RAM): A novel
#'  multi-criteria decision making method and its applications in sustainability
#'  challenges. Journal of Cleaner Production. 2023, Vol. 423, pp. 138695,
#'  available from: https://doi.org/10.1016/j.jclepro.2023.138695, ISSN 0959-6526.
ram <- R6Class(
  "ram",
  public = list(
    #' @field pm performance matrix
    pm = NULL,

    #' @field w vector of weights
    w = NULL,

    #' @field minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    minmax = NULL,

    #' @field result data frame consolidating results
    result = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm normalized performance matrix
    #' @param w weights vector
    #' @param minmax minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    #' @param beta beta
    initialize = function(pm, w, minmax = "max") {
      # parameters validation
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of parameter validation

      self$pm <- as.data.frame(pm)
      self$w <- w / sum(w)
      self$compute()
      self
    },

    #' @description
    #' computes the model bas of class properties. Usually we do not run the
    #'  computation manually (it is run from class' constructor).
    compute = function() {
      c_max <- colSums(self$pm)
      pm_w <- self$pm %>%
        sweep(2, c_max, FUN = "/") %>%
        sweep(2, self$w, FUN = "*")
      min_indices <- which(self$minmax == "min")
      max_indices <- which(self$minmax == "max")
      s_plus <- rowSums(pm_w[, max_indices])
      s_minus <- rowSums(pm_w[, min_indices])
      q <- (2 + s_plus)^(1 / (2 + s_minus))
      t <- data.frame(q, rank(-q))
      colnames(t) <- c("RI", "rank")
      self$result <- t
    },

    #' @description
    #' summary of the RAM method resutls.
    #' @return basic information on the model including ranking.
    summary = function() {
      nalt <- nrow(self$pm) #no. of alternatives
      ncri <- ncol(self$pm)
      cat(paste0(
        "\nRAM:\n",
        "processed ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria\n\nResults:\n"
      ))
      print(self$result, pretty = TRUE)
    }
  )
)