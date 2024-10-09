#' Binary pairwise camparison weight estimation
#'
#' @description
#' Derives weights of the criteria using binary preferences of importance
#'  of the criteria by comparing each pair of criteria and choosing one more
#'  important of the pair.
#'
#' Idea is that the criteria, which are more important are preferred more often
#'  than those, which are less important.
#'
#' Weights are computed by counting number of times the criterium has been
#'  preferred and comparing the value to number of comparisons realized.
#'
#' The approach is very similar to other methods such as Fuller's triangle.
#'
#' @param pm preference matrix n:n, where n is number of criteria, 1 means the
#'  cretrium in row is more important than the one in column, other elements
#'  are 0.
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
#' ŠENOVSKÝ, Pavel. Modelling of Decision Procesess. 4th edition, VŠB - TU
#'  Ostrava, Ostrava 2024, 195 str., available from
#'  \url{https://fbiweb.vsb.cz/~sen76/data/uploads/skripta/modelling4ed.pdf}
#'  [cit. 2024-09-15]
#'
#' @keywords weights binary pair-wise
#'
#' @examples
#' PM <- rbind(
#'    c(0,1,1,0,1,1),
#'    c(0,0,0,0,1,1),
#'    c(0,1,0,0,1,1),
#'    c(1,1,1,0,1,1),
#'    c(0,0,0,0,0,1),
#'    c(0,0,0,0,0,0)
#' )
#' rownames(PM) <- colnames(PM) <- c("C1", "C2", "C4", "C5", "C6", "C8")
#' t <- binary_paiwise_comp(PM)
binary_paiwise_comp <- function(pm) {
  # param validation
  validation$validate_pm(pm)
  validation$validate_pm_rows_columns_same(pm)
  validation$validate_invalid_val(pm, c(0, 1), "Preference matrix")
  diag(pm) <- 0
  validation$validate_pm_01_symetry(pm)
  # end of validation

  rs <- rowSums(pm)
  w <- rs / sum(rs)
  t <- list(
    preferenceGraph = plot.prefM(pm),
    w = w
  )
  return(t)
}