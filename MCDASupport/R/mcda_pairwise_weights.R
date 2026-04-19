#' Compute weights based on pairwise comparison
#' 
#' @description
#' Function allows to apply various method which derive weights based on stated
#'  preferences, when comparing every pair of criteria in decision problem.
#' 
#' At present time the function computes:
#' \tabular{ll}{
#'        \bold{constant} \tab \bold{method}\cr
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
#' @param pm preference matrix n:n, where n is number of criteria
#' 
#' @param method constant (see the table to identify the method to use)
#' 
#' @return named weights vector
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
    "binary",
    "RANCOM"
  )
  validation$validate_invalid_val(
    method,
    nmethods,
    "pairwise weighting methods"
  )
  if (method == "RANCOM") {
    validation$validate_invalid_val(pm, c(0, 1, 0.5), "Preference matrix")
    diag(pm) <- 0.5
    # validate symetry of matrix
    ncri <- ncol(pm)
    for (i in 2:ncri) {
      for (j in i:ncri) {
        if (
          (pm[i, j] == 0 && pm[j, i] != 1) ||
            (pm[i, j] == 1 && pm[j, i] != 0) ||
            (pm[i, j]) == 0.5 && pm[j, i] != 0.5
        ) {
          t <- paste(
            "Inconsistencies detected in preference matrix in index [", i, ", ", j, "] = ",
            pm[i, j], " vs [", j, ", ", i, "] = ", pm[j, i]
          )
          stop(t)
        }
      }
    }
  } else if (method == "binary") {
    validation$validate_invalid_val(pm, c(0, 1), "Preference matrix")
    diag(pm) <- 0
    validation$validate_pm_01_symetry(pm)
  }
  # end of validate

  rs <- rowSums(pm)
  w <- rs / sum(rs)
  names(w) <- colnames(pm)
  return(w)

}