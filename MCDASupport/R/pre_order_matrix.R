#' function to create preorder matrix
#'
#' @description
#' Function takes outputs of descending and ascending distilation pre-order and
#'  created pre-order matrix describing outranking relation between the
#'  alternatives.
#'
#' The relation can be P+ (a outranks b), P- (b outranks a), I (indifference)
#'  and R (incomparable). Information from preorder matrix can be utilized to
#'  create adjancancy matrix and construct final ranking.
#'
#' This function is being utilized by \link{electre3} and \link{electre4}.
#'
#' @param rank_D descending distilation ranking in the form of ordered dataframe
#' @param rank_A ascending distilation ranking in the form of ordered dataframe
#' @param alt vector of alternatives names
#'
#' @return pre-order matrix
#'
#' @references
#' Meyer, P. at al. MCDA package. GitHub: 2021, available from:
#'  \url{https://github.com/paterijk/MCDA/blob/master/}
#'
#' Prombo, M. Package OutrankingTools, CRAN: 2015, available from:
#'  \url{https://cran.r-project.org/web/packages/OutrankingTools/}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords electre3 electre4 pre-order matrix
pre_order_matrix <- function(rank_D, rank_A, alt) {
  nalt <- length(alt)
  po_string <- matrix("-", nalt, nalt)
  rownames(po_string) <- alt
  colnames(po_string) <- alt
  for (i in 1:nalt) {
    Da <- filter(rank_D, action == alt[i]) #descending ranks
    Aa <- filter(rank_A, action == alt[i]) #ascending ranks
    for (j in 1:nalt) {
      if (i < j) {
        Db <- filter(rank_D, action == alt[j]) #descending ranks
        Ab <- filter(rank_A, action == alt[j]) #ascending ranks

        Da_less_Db <- Da$rank < Db$rank
        Da_eq_Db <- Da$rank == Db$rank
        Aa_less_Ab <- Aa$rank < Ab$rank
        Aa_eq_Ab <- Aa$rank == Ab$rank

        if ((Da_less_Db && Aa_less_Ab) ||
              (Da_eq_Db && Aa_less_Ab) ||
              (Da_less_Db && Aa_eq_Ab)) {
          po_string[i, j] <- "P+"
          po_string[j, i] <- "P-"
        } else if ((!Da_less_Db && !Aa_less_Ab) ||
                     (Da_eq_Db && !Aa_less_Ab) ||
                     (!Da_less_Db && Aa_eq_Ab)) {
          po_string[i, j] <- "P-"
          po_string[j, i] <- "P+"
        } else if (Da_eq_Db && Aa_eq_Ab) {
          po_string[i, j] <- "I"
          po_string[j, i] <- "I"
        } else {
          #if((alts_D[i] > alts_D[j] && alts_A[i] < alts_A[j]) ||
          #(alts_D[i] < alts_D[j] && alts_A[i] > alts_A[j])){
          po_string[i, j] <- "R"
          po_string[j, i] <- "R"
        }
      }
    }
  }
  return(po_string)
}
