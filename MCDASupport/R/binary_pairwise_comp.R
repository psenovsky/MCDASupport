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

  # check consistency of pairwise comparison
  #
  # @description
  # checks that if element of the matrix [i,j] == 0 then [j,i] == 1 and wice
  #  versa.
  #
  # @param mat criteria preference matrix
  # @return true if the preferences are consisten
  check_consistency <- function(mat) {
    n <- nrow(mat)
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        if ((mat[i, j] == 1 && mat[j, i] != 0) ||
              (mat[i, j] == 0 && mat[j, i] != 1)) {
          return(FALSE)
        }
      }
    }
    return(TRUE)
  }

  # param validation
  ncri <- ncol(pm)
  if (ncri != nrow(pm)) {
    stop("number of criteria in rows and colums of preference matrix must be
         same.")
  }
  if (ncri < 2) {
    stop("must have more than 2 criteria")
  }
  if (any(pm != 0 & pm != 1)) {
    stop("preference matrix must have only value 0 or 1.")
  }
  diag(pm) <- 0
  if (!check_consistency(pm)) {
    stop("detected inconsistency in stated preferences. If [i,j] == 1 then
         [j,i] == 0 and vice-versa.")
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