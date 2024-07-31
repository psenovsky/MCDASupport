#' Method used compute discordance matrix in Electre methods
#'
#' @description
#' \loadmathjax
#' Internal method for computing discordance matrix for \code{\link{electre1}}
#'  and \code{\link{electre2}} methods. Discordance matrix is one of two
#'  approaches ELECTRE methods use to derive preference for the alternatives,
#'  the other being concordance matrix.
#'
#' Concordancce matrix (index) measures strength of the statement that
#'  alternative a outranks alternative b, while discordance matrix (index)
#'  together with discordance threshold (exceeding this threshold) can prevent
#'  such outranking.
#'
#' Code is inspired by pyDecisions package.
#'
#' Computationally discoradce matrix D(a,b) is defined for:
#'
#' \mjsdeqn{g_j(a) \ge g_j(b) \forall j: D(a,b) = 0}
#'
#' and for everything else:
#'
#' \mjsdeqn{D(a,b) = \max_{j} \frac{g_j(b)-g_j(a)}{\delta_j}}
#'
#' where
#'
#' \mjsdeqn{\delta_j = \max_{j} g_j(a)-g_j(b)}
#'
#' Where a, b ... are alternatives to be compared, \mjseqn{g_j(x)} ...
#'  performance of alternative x in criterium j and \mjseqn{\delta_j} is
#'  maximal difference of alternatives a and b preformance across the criteria.
#'
#' @param pm matrix or data frame containing the performance table. Each row
#'  corresponds to an alternative, and each column to a criterion. only numeric
#'  values expercted. Rows and columns are expected to be named and criteria
#'  are expected to be maximized (you can use function util_prepare_minmax to
#'  do that)
#'
#' @return computed discordancce matrix
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
#' @examples
#' PM <- cbind(
#'   c(103000,101300,156400,267400,49900,103600,103000,170100,279700,405000),
#'   c(171.3,205.3,221.7,230.7,122.6,205.1,178.0,226.0,233.8,265.0),
#'   c(7.65,7.90,7.90,10.50,8.30,8.20,7.20,9.10,10.90,10.30),
#'   c(352,203,391,419,120,265,419,419,359,265),
#'   c(11.6,8.4,8.4,8.6,23.7,8.1,11.4,8.1,7.8,6.0),
#'   c(88.0,78.3,81.5,64.7,74.1,81.7,77.6,74.7,75.5,74.7),
#'   c(69.7,73.4,69.0,65.6,76.4,73.6,66.2,71.7,70.9,72.0))
#' rownames(PM) <- c("CBX16","P205G","P405M","P605S",
#'   "R4GTL","RCLIO","R21TS","R21TU","R25BA","ALPIN")
#' colnames(PM) <- c("Prix","Vmax","C120","Coff","Acce","Frei","Brui")
#' minmaxcriteria <-c("min","max","min","max","min","min","min")
#' PMmax <- util_pm_minmax(PM, minmaxcriteria)
#' dm <- ELECTRE_DiscordanceMatrix(PMmax)
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ELECTRE I
#' @keywords ELECTRE II
#' @keywords discordance matrix
#'
#' @seealso \code{\link{electre1}}
#' @seealso \code{\link{electre2}}
#' @seealso \code{\link{ELECTRE_ConcordanceMatrix}}
ELECTRE_DiscordanceMatrix <- function(pm) {

  # with < 2 criteria or alternatives, there is no MCDA problem
  if (is.null(dim(pm))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(pm) || (is.data.frame(pm)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  ## End of checking the validity of the "inputs"

  nalt <- nrow(pm)  #no. of alternatives
  ncri <- ncol(pm)  #no. of criteria
  alt  <- rownames(pm)

  dm <- matrix(data = 0, nrow = nalt, ncol = nalt)
  for (i in 1:nalt) {
    for (j in 1:nalt) {
      if (i != j) {
        m_pm <- 0
        for (k in 1:ncri) {
          range <- max(pm[, k]) - min(pm[, k])
          d_pm <- ifelse(range == 0, 1, (pm[j, k] - pm[i, k]) / range)
          m_pm <- max(m_pm, d_pm)
        }
        dm[i, j] <- m_pm
      }
    }
  }
  rownames(dm) <- alt
  colnames(dm) <- alt
  dm <- round(dm, 2)
  return(dm)
}
