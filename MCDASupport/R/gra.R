#' Grey relational analysis
#'
#' @description
#' One of most well known application of grey theory. It defines situations
#'  with no information as black, and those with perfect information as white.
#'  However, neither of these idealized situations ever occurs in real world
#'  problems. In fact, situations between these extremes, which contain partial
#'  information, are described as being grey. (from Wikipedia).
#'
#' Computationally the GRA expects performace matrix to be in numeric form.
#'  Values of performance of the alternatives in criteria are normalized using
#'  min-max normalization.
#'
#' Then deviation sequence k is computed
#'
#' \mjsdeqn{k = max_i - n_{ij} = 1 - n_{ij}}
#'
#' Computatioon of k is simplified due to usage of min-max normalization. Using
#'  this type of normalization leads always to max = 1 and min = 0, n
#'  represents normalized value.
#'
#' Then Grey relational coefficient (GRC) is computed as
#'
#' \mjsdeqn{GRC = \frac{\Delta_{min} + DC \Delta_{max}}{k_{ij} + DC \Delta_{max}} = \frac{0 + DC \cdot 1}{k_{ij} + DC \cdot 1} = \frac{DC}{k_{ij} + DC}}
#'
#' DC is distinguished coefficient (default value: 0.5). Provided
#'  simplification of the above equation holds only if min-max normalization is
#'  used. If other form of normalization is used then min and max values can be
#'  expected to differ from 0 and 1.
#'
#' GRC then can be processed into gray relational grade
#'
#' \mjsdeqn{GRA = \frac{\sum_{i=1}^m GRC_i}{m}}
#'
#' Where m is number of criteria.
#'
#' @references
#' How to use Grey Relational Analysis to decide? Available from
#'  \url{https://www.youtube.com/watch?v=J63Q5IbK-cs}.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords GRA
gra <- R6Class("gra",
  public = list(
    #' @field pm_orig original (not normalized) performance matrix
    pm_orig = NULL,

    #' @field pm normalized performance matrix
    pm = NULL,

    #' @field w vector of weights (sum of weights = 1)
    w = NULL,

    #' @field dc distinguished coefficient (0.5 by default)
    dc = NULL,

    #' @field gra results of GRA analysis (average Grey relational coefficient)
    gra = NULL,

    #' @field gra_sorted  ordered results of GRA analysis (average Grey
    #'  relational coefficient). Sorted form best to worst.
    gra_sorted = NULL,

    #' @field grc Grey relational coefficient
    grc = NULL,

    #' @field minmax vector specifying optimalization direction for the
    #'  criteria. Values max/min are expected. If all criteria are optimalized
    #'  in same direction the vector can be replaced by single value. Max value
    #'  is default.
    minmax = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix (alternatives in rows, criteria in columns).
    #' @param minmax vector of optimization direction for criteria (min/max).
    #'  Can use single min/max if optimization direction of all criteria is
    #'  same.
    #' @param dc distinguished coefficient (0.5 by default)
    #'
    #' @return instance of the class included computed model
    #'
    #' @examples
    #' # from: https://www.youtube.com/watch?v=J63Q5IbK-cs
    #' alternatives <- c("car 1", "car 2", "car 3", "car 4", "car 5", "car 6",
    #'  "car 7", "car 8", "car 9", "car 10")
    #' criteria <- c("quality", "condition", "security", "delivery days",
    #'  "fuel consumption", "price")
    #' M <- rbind(
    #'   c(3, 6, 4, 20, 2, 30000),
    #'   c(4, 4, 6, 15, 2.2, 32000),
    #'   c(6, 5, 9, 18, 3, 32100),
    #'   c(5, 6, 3, 23, 2.8, 28000),
    #'   c(4, 8, 7, 30, 1.5, 29000),
    #'   c(8, 3, 6, 35, 1.9, 27000),
    #'   c(7, 2, 5, 33, 1.7, 28500),
    #'   c(3, 8, 3, 34, 1.6, 30500),
    #'   c(8, 4, 8, 40, 2.5, 33000),
    #'   c(9, 3, 7, 34, 2, 29800)
    #' )
    #' rownames(M) <- alternatives
    #' colnames(M) <- criteria
    #' minmax <- c("max", "max", "max", "min", "min", "min")
    #' t <- gra$new(M, minmax)
    initialize = function(pm, minmax = "max", dc = 0.5) {
      # validate parameters
      ncri <- ncol(pm)
      self$pm_orig <- pm
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      validation$validate_value_in_interval(dc, 0, 1,
                                            "Distinguished coefficient")
      # end of validation

      self$dc <- dc
      self$compute()
      self
    },

    #' @description
    #' Computed the GRA using params provided in constructor. Usually run
    #'  automatically by constructor.
    compute = function() {
      self$pm <- self$pm_orig
      ncri <- ncol(self$pm)
      for (i in 1:ncri) {
        self$pm[, i] <- mcda_norm(self$pm_orig[, i], self$minmax[i], "minmax")
      }
      k <- 1 - self$pm
      self$grc <- 0.5 / (k + 0.5)
      self$gra <- rowSums(self$grc) / ncri
      self$gra_sorted <- sort(self$gra, decreasing = TRUE)
    },

    #' @description
    #' prepares summary of the GRA method resutls and outputs them
    #'  to the console.
    summary = function() {
      nalt <- nrow(self$pm)
      ncri <- ncol(self$pm)
      cat(paste("GRA method results:\nProcessed ", nalt, " alternatives in ",
                ncri, " criteria\n\nResults:\n"))
      print(self$gra_sorted, pretty = TRUE)
    },

    #' @description
    #' Plots interaction diagram  between the criteria based on Grey
    #'  relational coefficient (GRC).
    #'
    #' Not usefull for visializing GRA results using large ammount of criteria.
    #'  Reasonable maximal number of criteria is around 6. Increasing number
    #'  of criteria increases number of diagrams ploted into constant size plot
    #'  and decrease in readibility as result.
    interaction_diagram = function() {
      t <- as.data.frame(self$grc)
      plot(t)
    }
  )
)