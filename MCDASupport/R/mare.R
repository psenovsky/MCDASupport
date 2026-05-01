#' Multi Attribute Range Evaluation
#'
#' @description
#' Interesting extension of \link{wsm} method. Internally it uses WSM for its
#'  recommendations, it allows to express the uncertainties as the range the
#'  true performance measure is in.
#'
#' Other methods use fuzzy logic for the purpose, MARE approaches the problem
#'  by just manually setting the range manually. The method then computes WSM
#'  3-times: 1) for normal measures, 2) minimal measures and 3) maximal
#'  measures.
#'
#' @references
#' Richard E. Hodgett, Elaine B. Martin, Gary Montague, Mark
#' Talford (2014). Handling uncertain decisions in whole process design.
#' Production Planning & Control, Volume 25, Issue 12, 1028-1038.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
mare <- R6Class(
  "mare",
  public = list(
    #' @field pm performance matrix
    pm = NULL,

    #' @field pm_min minimal bounds for sample in pm
    pm_min = NULL,

    #' @field pm_max maximal bounds for sample in pm
    pm_max = NULL,

    #' @field w vector of weights
    w = NULL,

    #' @field minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    minmax = NULL,

    #' @field result data frame with results of the method
    result = NULL,

    #' @field resultPlot visualisation of the results with uncertainties
    resultPlot = NULL,

    #' @description
    #' Public constructor for the class. Checks validity of input parameters
    #'  and performs computation of of the model based on them.
    #'
    #' @param pm normalized performance matrix
    #' @param pm_min minimal bounds for sample in pm
    #' @param pm_max maximal bounds for sample in pm
    #' @param w vector of weights, its sum must be equal to 1
    #' @param minmax minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    initialize = function(pm, pm_min, pm_max, w, minmax = "max") {
      # validity check
      ncri <- ncol(pm)
      nalt <- nrow(pm)
      validation$validate_pm(pm)
      validation$validate_pm(pm_min)
      validation$validate_pm(pm_max)
      self$pm <- pm
      self$pm_min <- pm_min
      self$pm_max <- pm_max
      validation$validate_no_elements_vs_cri(w, ncri, "weights")
      self$minmax <- validation$validate_minmax(minmax, ncri)
      if (
        ncri != ncol(pm_min) ||
          ncri != ncol(pm_max) ||
          nalt != nrow(pm_min) ||
          nalt != nrow(pm_max)
      ) {
        stop("size of all 36 performance matrixes must be same.")
      }
      for (i in 1:ncri) {
        for (j in 1:nalt) {
          if (pm_min[j, i] > pm[j, i] || pm[j, i] > pm_max[j, i]) {
            stop(
              "inconsistency in bounds found (either pm_min > pm or pm > pm_max"
            )
          }
        }
      }
      # end of validaty check

      self$w <- w / sum(w)
      self$compute()
      self
    },

    #' @description
    #' performs computation of MARE model based on class properties. Usually
    #'  is not run manually as constructor calls this method automatically.
    compute = function() {
      ncri <- ncol(self$pm)
      pmn_min <- pmn_max <- pmn <- self$pm
      # normalize 3 PMs
      for (i in 1:ncri) {
        if (self$minmax[i] == "max") {
          maximum <- max(self$pm_max[, i])
          pmn_min[, i] <- self$pm_min[, i] / maximum
          pmn[, i] <- self$pm[, i] / maximum
          pmn_max[, i] <- self$pm_max[, i] / maximum
        } else {
          minimum <- min(self$pm_min[, i])
          pmn_min[, i] <- minimum / self$pm_min[, i]
          pmn[, i] <- minimum / self$pm[, i]
          pmn_max[, i] <- minimum / self$pm_max[, i]
        }
      }
      t <- wsm$new(pmn, self$w)
      t_min <- wsm$new(pmn_min, self$w)
      t_max <- wsm$new(pmn_max, self$w)

      result <- data.frame(
        t_min$result_table$weighted_sum,
        t$result_table$weighted_sum,
        t_max$result_table$weighted_sum,
        rank(-t$result_table$weighted_sum)
      )
      colnames(result) <- c("min", "estimate", "max", "rank")
      rownames(result) <- rownames(self$pm)
      self$result <- result
      self$plotResult()
    },

    #' @description
    #' plot visualisation of the estimate with min and max as error estimates
    plotResult = function() {
      alt <- rownames(self$pm)
      self$resultPlot <- plot_ly(
        self$result,
        x = alt,
        y = ~estimate,
        type = 'scatter',
        mode = 'markers',
        marker = list(color = 'blue', size = 10),
        error_y = list(
          type = 'data',
          symmetric = FALSE,
          array = ~ max - estimate, # Horní odchylka
          arrayminus = ~ estimate - min, # Spodní odchylka
          color = '#000000'
        )
      ) %>%
        layout(
          title = "Results with uncertainties visualisation",
          xaxis = list(title = "alternatives"),
          yaxis = list(title = "score")
        )
    },

    #' @description
    #' prepares summary of the MARE method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste0(
        "MARE:\nprocessed ",
        nalt,
        " alternatives in ",
        ncri,
        " criteria\n"
      ))
      print(self$result, pretty = TRUE)
    }
  )
)