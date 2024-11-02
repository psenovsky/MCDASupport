#' Evaluation based on distance from avaerage solution
#'
#' @description
#' Another method loosely based on \link{wsm}. This method computes positive
#'  and negative distance of the performance to the average performance value.
#'  Then computes weighted sums of these averages and then computes average of
#'  these two metrics. Mathematically it looks like this:
#'
#' step 1) compute averages of the performance in the criteria (AVj)
#'
#' step 2) calculate positive distance from the average. For benefit criteria
#'
#' \mjsdeqn{PDA_{ij} = max(0; \frac{x_{ij}-AV_j}{AV_j})}
#'
#' and for cost criteria
#'
#' \mjsdeqn{PDA_{ij} = max(0; \frac{AV_j - x_{ij}}{AV_j})}
#'
#' Similarily we compute negative distance to the average performance. Formally
#'  we switch the statements, so for benefit criteria (step 3)
#'
#' \mjsdeqn{NDA_{ij} = max(0; \frac{AV_j - x_{ij}}{AV_j})}
#'
#' and for cost criteria
#'
#' \mjsdeqn{NDA_{ij} = max(0; \frac{x_{ij}-AV_j}{AV_j})}
#'
#' These two measures can be used to produce weighted positive (SPI) and
#'  negative (SNI) sums (steps 4 and 5)
#'
#' \mjsdeqn{SPI = \sum_{j=1}^m PDA_{ij} \cdot w_j}
#'
#' \mjsdeqn{SNI = \sum_{j=1}^m NDA_{ij} \cdot w_j}
#'
#' Computed values of the SPI and SNI are normalized against its maximal values
#'  (step 6)
#'
#' \mjsdeqn{NSPI = \frac{SPI}{max(SPI)}}
#'
#' \mjsdeqn{NSNI = 1 - \frac{SNI}{max(SNI)}}
#'
#' step 7) compute average of NSPI and NSNI
#'
#' \mjsdeqn{ASI = \frac{NSPI + NSNO}{2}}
#'
#' Value of ASI is directly usable for ranking of alternatives (largest value
#'  is best).
#'
#' @references
#' Use of Evaluation Based on the Distance from Average Solution 'EDAS" in
#'  decision making process. \url{https://www.youtube.com/watch?v=Kwq_ra6b6eU}.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords WSM EDAS
edas <- R6Class("edas",
  public = list(
    #' @field pm performance matrix (alternatives in rows, criteria in columns)
    pm = NULL,

    #' @field minmax vector with optimization direction of the criteria (min or
    #'  max), can be substituted by single min or max if optimization direction
    #'  for all criteria is the same.
    minmax = NULL,

    #' @field w weight vector for criteria
    w = NULL,

    #' @field spi_scoreM graphical representation of criteria contribution to
    #'  weighted sum of positive distances
    spi_scoreM = NULL,

    #' @field sni_scoreM graphical representation of criteria contribution to
    #'  weighted sum of negative distances
    sni_scoreM = NULL,

    #' @field result_table Table with computed weighted positive (SPI),
    #'  negative (SNI) sums, normalized positive (NSPI), negative (NSNI) sums
    #'  and average (ASI) of NSNI and NSPI
    result_table = NULL,

    #' @description
    #' class constructor, validates data and computes the model.
    #'
    #' @param pm performance matrix (alternatives in rows, criteria in columns).
    #' @param w weight vector for criteria
    #' @param minmax vector of optimization direction for criteria (min/max).
    #'  Can use single min/max if optimization direction of all criteria is
    #'  same.
    #'
    #' @return instance of the class included computed model
    #'
    #' @examples
    #' #from https://www.youtube.com/watch?v=Kwq_ra6b6eU
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
    #' w <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.3)
    #' t <- edas$new(M, w, minmax)
    initialize = function(pm, w, minmax = "max") {
      # validate inputs
      ncri <- ncol(pm)
      validation$validate_pm(pm)
      self$minmax <- validation$validate_minmax(minmax, ncri)
      # end of validation

      self$pm <- pm
      self$w <- w
      self$compute()
      self
    },

    #' @description
    #' Computed the EDAS using params provided in constructor. Usually run
    #'  automatically by constructor.
    compute = function() {
      avj <- colMeans(self$pm)
      ncri <- ncol(self$pm)
      pda <- nda <- self$pm
      for (i in 1:ncri) {
        if (self$minmax[i] == "max") {
          pda[, i] <- pmax(0, (self$pm[, i] - avj[i]) / avj[i])
          nda[, i] <- pmax(0, (avj[i] - self$pm[, i]) / avj[i])
        } else {
          pda[, i] <- pmax(0, (avj[i] - self$pm[, i]) / avj[i])
          nda[, i] <- pmax(0, (self$pm[, i] - avj[i]) / avj[i])
        }
      }
      t <- wsm$new(pda, self$w, "max")
      spi <- t$result_table$weighted_sum
      nspi <- spi / max(spi)
      self$spi_scoreM <- t$scoreM
      t <- wsm$new(nda, self$w, "max")
      sni <- t$result_table$weighted_sum
      nsni <- sni / max(sni)
      self$sni_scoreM <- t$scoreM
      asi <- (nspi + nsni) / 2
      self$result_table <- data.frame(spi, sni, nspi, nsni, asi)
      rownames(self$result_table) <- rownames(self$pm)
    },

    #' @description
    #' prepares summary of the EDAS method resutls and outputs them
    #'  to the console.
    summary = function() {
      cat(paste("Edas method results:\n\nResult table:\n"))
      print(self$result_table, pretty = TRUE)
      print(self$spi_scoreM)
      print(self$sni_scoreM)
    }
  )
)