#' computes preference degree for SIR and PROMETHEE functions
#'
#' @description
#' The function is being used in \code{\link{SIR}} and
#'  \code{\link{PROMETHEE}} functions to compute preferencee degree.
#'  degree is then further used by these methods to derive its
#'  recommendations.
#'
#' @param nalt number of alternatives
#' @param ncri number of criteria
#' @param DK result of paiweise comparison
#' @param d type of function (default, U-shape, V-shape, level), vector - sets
#'  the type for every criterion
#' @param qj indifference threshold vector
#' @param pj preference threshold vector
#' @param sj ntermediate threshold vector
#' @param alt vector of alternative's names
#' @param cri vector of criteria's names
#'
#' @return preference degree matrix
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords preference degree SIR PROMETHEE
preferenceDegree <- function(nalt, ncri, DK, d, qj, pj, sj, alt, cri) {
  Pj <- lapply(1:ncri, function(k) {
    DKf <- DK[[k]]
    PjK <- outer(1:nalt, 1:nalt, Vectorize(function(i, j) {
      if (d[k] == "default") {
        if (DKf[i, j] <= 0) 0 else 1
      } else if (d[k] == "U-shape") {
        if (DKf[i, j] <= qj[k]) 0 else 1
      } else if (d[k] == "V-shape") {
        if (DKf[i, j] <= 0) {
          0
        } else if (DKf[i, j] > 0 & DKf[i, j] <= pj[k]) {
          DKf[i, j] / pj[k]
        } else {
          1
        }
      } else if (d[k] == "level") {
        if (DKf[i, j] <= qj[k]) {
          0
        } else if (DKf[i, j] > qj[k] & DKf[i, j] <= pj[k]) {
          0.5
        } else {
          1
        }
      } else if (d[k] == "linear") {
        if (DKf[i, j] <= qj[k]) {
          0
        } else if (DKf[i, j] > qj[k] & DKf[i, j] <= pj[k]) {
          (DKf[i, j] - qj[k]) / (pj[k] - qj[k])
        } else {
          1
        }
      } else if (d[k] == "Gaussian") {
        if (DKf[i, j] <= 0) {
          0
        } else {
          1 - exp(-((DKf[i, j] * DKf[i, j]) / (2 * sj[k] * sj[k])))
        }
      }
    }))
    rownames(PjK) <- alt
    colnames(PjK) <- alt
    PjK
  })
  names(Pj) <- cri
  return(Pj)
}
