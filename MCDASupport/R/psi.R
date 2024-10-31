#' Preference Selection Index
#'
#' @description
#' Decision support method in family of weighted sum methods such as \link{wsm}
#'  and many other. The difference is, that the method does not take weights as
#'  parameter but derives them from provided performance information in manner
#'  which is similar to \link{EWM}.
#'
#' The process is following. First the performance matrix (alternatives in
#'  rows and criteria in colums is provided). These values are normalized. For
#'  benefit criteria we use:
#'
#' \mjsdeqn{R_{ij} = \frac{x_{ij}}{max_j x_{ij}}}
#'
#' and for cost criteria we use:
#'
#' \mjsdeqn{R_{ij} = \frac{min_j x_{ij}}{x_{ij}}}
#'
#' next we compute performance variation value (PV) computation by comparing
#'  normalized performance value and mean performance in the criterium.
#'
#' \mjsdeqn{PV = \sum_{j=1}^m (R_{ij} - mean(R_j))^2}
#'
#' We estimate overal performance
#'
#' \mjsdeqn{\Psi = 1 - PV_j}
#'
#' Weight vector can be then computed as
#'
#' \mjsdeqn{w = \frac{\Psi}{\sum \Psi}}
#'
#' Overal performace selection index (PSI) can be computed using normal WSM
#'  approach
#'
#' \mjsdeqn{PSI = \sum_{i = 1}^n R_{ij} \cdot w_i}
#'
#' Ranking of the alternatives is directly derivable from value of PSI.
#'
#' @references
#' PSI. How to use Preference Selection Index "PSI" to select the best
#'  alternatives? Available from
#'  \url{https://www.youtube.com/watch?v=jlxHKBkai_w}.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords WSM EWM PSI
psi <- R6Class("psi",
  public = list(
    #' @field pm_orig original performance matrix (not normalized)
    pm_orig = NULL,

    #' @field pm normalized performance matrix (alternatives in rows, criteria
    #'  in columns)
    pm = NULL,

    #' @field minmax vector with optimization direction of the criteria (min or
    #'  max), can be substituted by single min or max if optimization direction
    #'  for all criteria is the same.
    minmax = NULL,

    #' @field w computed weight vector for criteria
    w = NULL,

    #' @field result_table result table computed using WSM approach
    result_table = NULL,

    #' @field PSI weighted sum percentages
    PSI = NULL,
    #' @field scoreM graph visualizing contribution of the critteria to overall
    #'  PSI
    scoreM = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix (alternatives in rows, criteria in columns).
    #' @param minmax vector of optimization direction for criteria (min/max).
    #'  Can use single min/max if optimization direction of all criteria is
    #'  same.
    #'
    #' @return instance of the class included computed model
    #'
    #' @examples
    #' #from https://www.youtube.com/watch?v=jlxHKBkai_w
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
    #' t <- psi$new(M, minmax)
    initialize = function(pm, minmax = "max") {
      # validate parameters
      ncri <- ncol(pm)
      self$pm_orig <- pm
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of validation

      self$compute()
      self
    },

    #' @description
    #' Computed the PSI using params provided in constructor. Usually run
    #'  automatically by constructor.
    compute = function() {
      ncri <- ncol(self$pm_orig)
      self$pm <- self$pm_orig
      ncri <- ncol(self$pm)
      pv <- rep(0, times = ncri)
      pm2 <- self$pm
      for (i in 1:ncri) {
        if (self$minmax[i] == "max") {
          self$pm[, i] <- self$pm_orig[, i] / max(self$pm_orig[, i])
        } else {
          self$pm[, i] <- min(self$pm_orig[, i]) / self$pm_orig[, i]
        }
        pm2[, i] <- (self$pm[, i] - mean(self$pm[, i]))
      }
      pm3 <- pm2 * pm2
      pv <- colSums(pm3)
      #overal performance (op)
      op <- 1 - pv
      self$w <- op / sum(op)
      psi <- wsm$new(self$pm, self$w, "max")

      self$result_table <- psi$result_table
      self$PSI <- psi$result_table$weighted_sum
      names(self$PSI) <- rownames(self$pm)
      self$scoreM <- psi$scoreM
    },

    #' @description
    #' prepares summary of the PSI method resutls and outputs them
    #'  to the console.
    summary = function() {
      cat(paste("PSI method results:\n\nweights:\n"))
      print(self$w, pretty = TRUE)
      cat("\nPSI index:\n")
      print(self$PSI, pretty = TRUE)
      print(self$scoreM)
    }
  )
)