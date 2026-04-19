#' RANking COMparison
#' 
#' @description
#' Very similar method to binary pairwise comparison (see \link{binary_pairwise_comp})
#'  only with slightly differently expressed preferences. In RANCOM the
#'  preference is 1 if f(Ci) < f(Cj), 0 if f(Ci) < f(Cj) and 0.5 if
#'  f(Ci) == f(Cj).
#' 
#' The preferences are them summed up on per row basis and weights derived by
#'  vector normalizing the result.
#' 
#' @param pm preference matrix n:n, where n is number of criteria
#' 
#' @return
#' list:
#' \itemize{
#'    \item preferenceGraph - preference relation between criteria expressed as
#'  a graph
#'    \item w - derived weights (sum(w) = 1)
#' }
#' 
#' @references
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
#' t <- rancom(PM)
rancom <- function(pm) {
  # param validation
  validation$validate_pm(pm)
  validation$validate_pm_rows_columns_same(pm)
  validation$validate_invalid_val(pm, c(0, 1, 0.5), "Preference matrix")
  diag(pm) <- 0.5
  # validate symetry of matrix
  ncri <- ncol(pm)
  for (i in 2:ncri) {
    for (j in i:ncri) {
      if ((pm[i, j] == 0 && pm[j, i] != 1) || (pm[i, j] == 1 && pm[j, i] != 0) || (pm[i, j]) == 0.5 && pm[j, i] != 0.5) {
        t <- paste("Inconsistencies detected in preference matrix in index [", i, ", ", j, "] = ", pm[i, j], " vs [", j, ", ", i, "] = ", pm[j, i])
        stop(t)
      }
    }
  }
  # end of validation
  rs <- rowSums(pm)
  w <- rs / sum(rs)
  t <- list(
    preferenceGraph = plot.prefM(pm),
    w = w
  )
  return(t)
}