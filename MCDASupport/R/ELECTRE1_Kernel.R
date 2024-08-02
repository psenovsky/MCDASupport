#' computes so called kernell of the decision for ELECTRE I, II and 1S methods
#'
#' @description
#' Computes kernell of the solution as the set of alternatives which are not
#'  dominated by any other alternative. Other alternatives then for example can
#'  be excluded from decision making as they are clearly suboptimal.
#'
#' The results are presented in graphical form as network diagram with flows
#'  representing domination relation and two vectors with alternatives in
#'  kernel and dominated alternatives.
#'
#' The function is beiung utilized for \link{electre1}, \link{electre2} and
#'  \link{electre1s} methods.
#'
#' @param am adjacancy matrix or credibility matrix (ELECTRE 1S)
#'
#' @return
#' list of:
#' \itemize{
#'   \item graph - graphical representation of domination of one alternative
#'  over another
#'   \item dominated - vector of alternatives identified as dominated
#'   \item kernel - oposite to dominated vector. Consist for alternatives not
#'  dominated by other alternatives, forming kernel of the solution.
#' }
#'
#' @references
#' Balamurali, M.: pyDecisions - A Python Library of management decision making
#'  techniques. Avilable on-line from
#'  \url{https://github.com/Valdecy/pyDecisions}
#'
#' Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support:
#'  Methods and Applications in Engineering and Infrastructure investment.
#'  Springer 2000, 208 p., ISBN 978-1-4757-5057-7
#'
#' Prombo, M. Package OutrankingTools, CRAN: 2015, available from:
#'  \url{https://cran.r-project.org/web/packages/OutrankingTools/}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @seealso \link{electre1}
#' @seealso \link{electre2}
#' @seealso \link{electre1s}
#'
#' @keywords ELECTRE methods
#' @keywords ELECTRE I
#' @keywords ELECTRE II
#' @keywords ELECTRE 1S
#' @keywords kernel
ELECTRE1_Kernel <- function(am) {
  graph <- plot.prefM(am)
  alt <- rownames(am)
  row_sum <- rowSums(am)
  names(row_sum) <- alt
  kernel  <- names(which(row_sum == 0))
  if (is.null(kernel)) kernel <- "empty kernel"
  dominated <- setdiff(alt, kernel)

  out <- list(
    graph = graph,
    dominated = dominated,
    kernel = kernel
  )
  return(out)
}
