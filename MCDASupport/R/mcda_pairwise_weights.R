#' Compute weights based on pairwise comparison
#' 
#' @description
#' Function allows to apply various method which derive weights based on stated
#'  preferences, when comparing every pair of criteria in decision problem.
#' 
#' At present time the function computes:
#' \tabular{ll}{
#'        \bold{constant} \tab \bold{method}\cr
#'        AHP \tab Analytic Hierarchy Process\cr
#'        binary \tab Binary pairwise camparison weight estimation\cr
#'        RANCOM \tab RANking COMparison\cr
#'    }
#' 
#' \bold{Binary pairwise camparison weight estimation}
#' 
#' Derives weights of the criteria using binary preferences of importance
#'  of the criteria by comparing each pair of criteria and choosing one more
#'  important of the pair.
#'
#' Idea is that the criteria, which are more important are preferred more often
#'  than those, which are less important.
#'
#' Weights are computed by counting number of times the criterium has been
#'  preferred and comparing the value to number of comparisons realized. The
#'  result is then normalized using vestor normalization to derive weights.
#'
#' The approach is very similar to other methods such as Fuller's triangle.
#' 
#' \bold{RANCOM - RANking COMparison}
#' 
#' Very similar method to binary pairwise comparison only with slightly
#'  differently expressed preferences. In RANCOM the preference is 1 if
#'  f(Ci) < f(Cj), 0 if f(Ci) < f(Cj) and 0.5 if f(Ci) == f(Cj).
#' 
#' The preferences are them summed up on per row basis and weights derived by
#'  vector normalizing the result.
#' 
#' \bold{AHP - Analytic Hierarchy Process}
#' 
#' AHP is one of most widely used methods for MCDA and weight derivation based
#'  on stated preferences. 
#' 
#' This function implements only single preference matrix computation, so it
#'  does not implement hierarchy at all. 
#' 
#' The preferences are stated in interval 1 (equal) to 9 (extremely more inportrant)
#'  and 1/9 (extremely less important) to 1.
#' 
#' One of interesting properties of AHP is ability to evaluate randomnes of
#'  stated preferences by comparing its consistency index (CI) to average random
#'  consistency index (RCI). 
#' 
#' The weights are derived using geometric mean method.
#' 
#' \mjsdeqn{CI = \frac{\lambda_{mmax} - n}{n - 1}}
#' 
#' Lambda max is largest eigenvalue of the preference matrix, n is number of criteria
#' 
#' The ratio (CR - Consistency Ration) of CI/RCI should be under 0.1 to reject
#'  hyphothesis, that the stated preferences has been stated randomly. Function
#'  allows to perform this evaluation maximally for 15 criteria in single matrix.
#'  In case you need more, use hierarchy.
#' 
#' @param pm preference matrix n:n, where n is number of criteria
#' 
#' @param method constant (see the table to identify the method to use)
#' 
#' @return list with named weights vector and CR (for AHP only)
#' 
#' @references
#' ŠENOVSKÝ, Pavel. Modelling of Decision Procesess. 4th edition, VŠB - TU
#'  Ostrava, Ostrava 2024, 195 str., available from
#'  \url{https://fbiweb.vsb.cz/~sen76/data/uploads/skripta/modelling4ed.pdf}
#'  [cit. 2024-09-15]
#' 
#' WIĘCKOWSKI, Jakub, KIZIELEWICZ, Bartłomiej, SHEKHOVTSOV, Andrii, SAŁABUN, Wojciech.
#'  RANCOM: A novel approach to identifying criteria relevance based on inaccuracy
#'  expert judgments. Engineering Applications of Artificial Intelligence. 2023,
#'  vol. 122, p. 106114, available from: \url{https://doi.org/10.1016/j.engappai.2023.106114},
#'  ISSN 0952-1976.
#' 
#' Řehák, D., Šenovský, P. Preference risk assessment of electric power critical
#'  infrastructure. In Chemical Engineering Transactions, Vol. 36, pp. 469-474,
#'  DOI 10.3303/CET1436079, ISSN: 1974-9791
#' 
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#' 
#' @examples
#' PM <- rbind(
#'    c(0.5,1,1,0,1,1),
#'    c(0,0.5,0,0,1,1),
#'    c(0,1,0.5,0,1,1),
#'    c(1,1,1,0.5,1,1),
#'    c(0,0,0,0,0.5,1),
#'    c(0,0,0,0,0,0.5)
#' )
#' rownames(PM) <- colnames(PM) <- c("C1", "C2", "C4", "C5", "C6", "C8")
#' t <- mcda_pairwise_weights(PM, method = "RANCOM")
mcda_pairwise_weights <- function(pm, method) {
  # validate
  validation$validate_pm(pm)
  validation$validate_pm_rows_columns_same(pm)
  nmethods <- c(
    "AHP",
    "binary",
    "RANCOM"
  )
  validation$validate_invalid_val(
    method,
    nmethods,
    "pairwise weighting methods"
  )
  # end of validate

binary <- function(pm) {
  # validation
  validation$validate_invalid_val(pm, c(0, 1), "Preference matrix")
  diag(pm) <- 0
  validation$validate_pm_01_symetry(pm)
  #end of validation
  rs <- rowSums(pm)
  w <- rs / sum(rs)
  names(w) <- colnames(pm)
  result <- list(w = w)
  return(result)
}

rancom <- function(pm) {
  # validation
  validation$validate_invalid_val(pm, c(0, 1, 0.5), "Preference matrix")
  diag(pm) <- 0.5
  ncri <- ncol(pm)
  for (i in 2:ncri) { # validate symetry of matrix
    for (j in i:ncri) {
      if ((pm[i, j] == 0 && pm[j, i] != 1) || (pm[i, j] == 1 && pm[j, i] != 0) || (pm[i, j]) == 0.5 && pm[j, i] != 0.5) {
        t <- paste(
          "Inconsistencies detected in preference matrix in index [",
          i, ", ", j, "] = ", pm[i, j],
          " vs [", j, ", ", i, "] = ", pm[j, i])
        stop(t)
      }
    }
  }
  # end of validation
  rs <- rowSums(pm)
  w <- rs / sum(rs)
  names(w) <- colnames(pm)
  result <- list(w = w)
  return(result)
}

ahp2 <- function(pm) {
  # validation
  validation$validation_vector_in_interval(c(pm), 0, 9, "preference matrix elements")
  ncri <- ncol(pm)
  if (ncri < 3 || ncri > 15) {
    stop("The consistency ratio is computable for matrixes of size 3 - 15.")
  }
  diag(pm) <- 1
  # end of validation
  # constants
  RCI <- c(NULL, NULL, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49, 1.51,
    1.48, 1.56, 1.57, 1.59)

  # AHP procedure
  multiplication <- rep(1, times = ncri)
  weights <- rep(0, times = ncri)
  nroot <- rep(0, times = ncri)
  for (i in 1:ncri) {
    multiplication <- multiplication * pm[, i]
  }
  for (i in 1:ncri) {
    nroot[i] <- multiplication[i]^0.17
  }
  sum_nroot <- sum(nroot)
  for (i in 1:ncri) {
    weights[i] <- nroot[i] / sum_nroot
  }
  t <- weights
  for (i in 1:ncri) {
    t[i] <- sum(weights * pm[i, ])
  }
  lambda_max <- t / weights
  t2 <- sum(lambda_max) / ncri
  CI <- (t2 - ncri) / (ncri - 1)
  CR <- CI / RCI[ncri]
  results <- list(
    w = weights,
    CR = CR
  )
  return(results)
}

result <- switch(
  method,
  "AHP" = ahp2(pm), # Analytic Hierarchy Process
  "binary" = binary(pm), # binary pairwise comparison
  "RANCOM" = rancom(pm)
)

}