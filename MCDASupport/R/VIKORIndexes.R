#' subroutine to compute S & R index in VIKOR and FuzzyVIKOR methods
#'
#' @description
#' The acronym VIKOR stands for: VlseKriterijumska Optimizacija I Kompromisno
#'  Resenje, in Serbian multicriteria optimization and compromise solution.
#'
#' The method presents generalized solution for computation of S and R indexes
#'  using wights, performance matrix (crisp performance matrix if using fuzzy
#'  numbers) and information on best and worst values in criteria.
#'
#' @param car array with the performances (crisp alternative ratings in fuzzy
#'  variant). Alternatives are in rows, criteria in columns
#' @param bw_pref matrix  with the best, worst performances and differences
#'  between them. Has columns (best, worst, difference) and rows for criteria
#' @param cw vector containing the weights of the criteria.
#' @param v weight for strategy of majority of the criteria (0-1)
#'
#' @return
#' The function returns a list structured as follows:
#' \itemize{
#'    \item S - ordered list of alternatives using S-metric
#'    \item R -ordered list of alternatives using R-metric
#'    \item Q - ordered list of alternatives using Q-metric
#'    \item compromiseSolution - list of alternatives forming compromise
#'  solution (based on Q, S, R metrics)
#' }
#'
#' @references
#' ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca
#'  Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
#'
#' Papathanasiou, Jason, Ploskas, Nikolaos. Multiple Criteria Decision Aid
#'  Methods, Examples and Python Implementations. Springer, 173 p., ISBN
#'  978-3-319-91648-4.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords VIKOR FuzzyVIKOR
VIKORIndexes <- function(car, bw_perf, cw, v = NULL) {
  #validate params
  if (ncol(bw_perf) != 3) {
    stop("bw_perf parameter is expected to have 3 columns (best, worst
         and difference).")
  }
  nalt <- nrow(car)
  ncri <- ncol(car)
  alt <- rownames(car)
  if (nrow(bw_perf) != ncri) {
    stop("number of criteria detected in car and bw_perf differs")
  }
  if (length(cw) != ncri) {
    stop("number of criteria detected in car and cw differs")
  }
  #end of validation

  S <- sapply(1:nalt, function(i) {
    S_temp <- sapply(1:ncri, function(j) (cw[j] * (bw_perf[j, 1] - car[i, j])) / bw_perf[j, 3])
    sum(S_temp)
  })
  R <- sapply(1:nalt, function(i) {
    S_temp <- sapply(1:ncri, function(j) (cw[j] * (bw_perf[j, 1] - car[i, j])) / bw_perf[j, 3])
    max(S_temp)
  })

  S_min <- min(S) #S*
  S_max <- max(S) #S-
  R_min <- min(R) #R*
  R_max <- max(R) #R-
  Q <- v * (S - S_min) / (S_max - S_min) + (1 - v) * (R - R_min) / (R_max - R_min)

  names(S) <- alt
  names(R) <- alt
  names(Q) <- alt

  orderS <- sort(S)
  orderR <- sort(R)
  orderQ <- sort(Q)
  compromise <- list()
  if (orderQ[2] - orderQ[1] >= 1 / (nalt - 1)) { #C1 satisfied
    nS <- names(orderS)
    nR <- names(orderR)
    nQ <- names(orderQ)
    if ((v > 0.5 && nQ[1] == nS[1]) || (v < 0.5 && nQ[1] == nR[1]) ||
          (v == 0.5 && nQ[1] == nR[1] && nQ[1] == nS[1])) {
      compromise[1] <- nQ[1]
    } else { #C2 condition not satisfied
      compromise[1] <- nQ[1]
      compromise[2] <- nQ[2]
    }
  } else {#C1 condition is not satisfied
    for (i in 1:nalt) {
      if (i == 1) {
        compromise[1] <- nQ[1]
      } else {
        if (orderQ[i] - orderQ[1] < 1 / (nalt - 1)) {
          compromise[i] <- nQ[i]
        } else {
          break
        }
      }
    }
  }

  out <- list(
    S = orderS,
    R = orderR,
    Q = orderQ,
    compromiseSolution = compromise
  )
  return(out)
}
