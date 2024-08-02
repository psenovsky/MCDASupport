#' Method used to compute concordance matrix for ELECTRE III and 1S methods
#'
#' @description
#' \loadmathjax
#' Internal method for computing concordance matrix for ELECTRE III
#'  (\link{electretri}) and ELECTRE 1S (\link{electre1s}) methods. Concordance
#'  matrix is one of two angles ELECTRE methods use to derive preference for
#'  the alternatives, the other being Discordance matrix.
#'
#' Concordancce matrix (index) measures strength of the statement that
#'  alternative a outranks alternative b, while discordance matrix (index)
#'  together with discordance threshold (exceeding this threshold) can prevent
#'  such outranking.
#'
#' To compute concordance matrix, partial concordance matrix \mjseqn{c_j} of
#'  the criteria needs to be computed first. The partial concordance matrix
#'  considers concordance by comparing performances of the alternatives in
#'  criterium \mjseqn{j}.
#'
#' \mjsdeqn{c_j(a,b) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(a) + Q_j(a) \ge PM_j(b) \cr 0 & \;if\; PM_j(a) + P_j(a) < PM_j(b)) \cr \frac{PM_j(a) - PM_j(b) + P_j(a)}{P_j(a) - Q_j(a)} & else\end{array}\right.}
#'
#' Based on partial concordance matrixes we compute concordence matrix
#'  \mjseqn{C} as:
#'
#' \mjsdeqn{C(a,b) = \frac{\sum (w_j \cdot c_j(a,b))}{\sum w_j}}
#'
#' where
#'
#' PM ... performance of alternative in criterion, Q ... indifference
#'  threshold, P ... prefference threshold, w ... weights.
#'
#' @param PM Matrix or data frame containing the performance table. Each row
#'  corresponds to an alternative, and each column to a criterion. only numeric
#'  values expercted. Rows and columns are expected to be named and criteria
#'  are expected to be maximized (you can use function util_prepare_minmax to
#'  do that).
#' @param P preference threshold vector
#' @param Q indefference threshold
#' @param w vector containing the weights of the criteria.
#'
#' @return Returns computed concordancce matrix.
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
#' Roy B. : "The outranking approach and the foundations of ELECTRE methods",
#'  Theory and Decision 31, 1991, 49-73.
#'
#' Vallée, D.; Zielniewicz, P. 1994. ELECTRE III-IV, version 3.x, Aspects
#'  Méthodologiques (tome 1), Guide d’utilisation (tome 2). Document du LAMSADE
#'  85 et 85bis, Université Paris Dauphine
#'
#' Prombo, M. Package OutrankingTools, CRAN: 2015, available from:
#'  \url{https://cran.r-project.org/web/packages/OutrankingTools/}
#'
#' Meyer, P. at al. MCDA package. GitHub: 2021, available from:
#'  \url{https://github.com/paterijk/MCDA/blob/master/}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @examples
#' # the performance table
#' PM <- cbind(
#'   c(-14,129,-10,44,-14),
#'   c(90,100,50,90,100),
#'   c(0,0,0,0,0),
#'   c(40,0,10,5,20),
#'   c(100,0,100,20,40)
#' )
#' rownames(PM) <- c("Project1","Project2","Project3","Project4","Project5")
#' colnames(PM) <- c( "CR1","CR2","CR3","CR4","CR5")
#' Q <- c(25,16,0,12,10) #Indifference thresholds
#' P <- c(50,24,1,24,20) #Preference thresholds
#' w <- c(1,1,1,1,1) #weights
#' v <- Electre3_ConcordanceIndex(PM, P, Q, w)
#'
#' @seealso \link{electretri}
#' @seealso \link{electre1s}
#'
#' @keywords ELECTRE III
#' @keywords ELECTRE 1S
#' @keywords concordance matrix
Electre3_ConcordanceIndex <- function(PM, P, Q, w) {
  nalt <- nrow(PM)
  ncri <- ncol(PM)
  alt  <- rownames(PM)
  cm   <- matrix(data = 0, nrow = nalt, ncol = nalt)  #concordance matrix
  cjw  <- list()   #temp variable holds cj (concordance matrix for the criteria)
  d <- matrix(data = 0, nrow = nalt, ncol = nalt)  #template for empty matrix
  colnames(d) <- alt
  rownames(d) <- alt
  for (k in 1:ncri) { #index for criteria
    c  <- d  #temp var for cj
    diff_pq <- P[k] - Q[k]
    for (i in 1:nalt) {    #index for a
      diff_piqk <- P[i] - Q[k]
      for (j in 1:nalt) {  #index for b
        #concordance matrix
        #represents percentage of weights of criteria that concord with the
        # proposition 'a outranks b'
        # eq C(a,b) = 1/W * SUM(wj*cj(a,b))
        # eq W = SUM(wj)
        # cj(a,b) = 1 if PMj(a) + Qj(a) >= PMj(b)
        # cj(a,b) = 0 if PMj(a) + Pj(a) < PMj(b)
        # else c(a,b) = (PMj(a) - PMj(b) + Pj(a)) / (Pj(a) - Qj(a))
        if (PM[i, k] + Q[k] >= PM[j, k]) {
          c[i, j] <- 1
        } else if (PM[i, k] + P[k] < PM[j, k]) {
          c[i, j] <- 0
        } else {
          c[i, j] <- ifelse((diff_piqk != 0),
                            (PM[i, k] - PM[j, k] + P[k]) / diff_pq, 1)
        }
      }
    }
    cjw[[k]] <- c * w[k]
  }
  cm <- Reduce("+", cjw) / sum(w)
  colnames(cm) <- alt
  rownames(cm) <- alt

  out <- list(
    ConcordanceMatrix = cm
  )
  return(out)
}
