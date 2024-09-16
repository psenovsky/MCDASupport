#' function to unpack list of ranks into ordered dataframe of alternatives in
#'  ranks
#'
#' @description
#' function to create dataframe with ordered, ranked alternatives ranklist -
#'  ranked list, each element in the list represents one rank. Each element in
#'  rank list can consist of multiple alternatives. This list needs to be
#'  flattened into dataframe in structure action and rank.
#'
#' Example of such approach would be to take list of ranks (ie. [1] "A1" [2]
#'  "A4" "A5" [3] "A2" [4] "A3") and unpacks them into dataframe with columns
#'  action and rank.
#'
#' This example leads to dataframe: (action, rak) (A1, 1), (A4, 2), (A5, 2),
#'  (A2, 3), (A3, 4).
#'
#' @param ranklist ordered list of ranked alternatives.
#'
#' @return dataframe wtih ranked alternatives from worst-to best.
#'
#' @references
#' Meyer, P. at al. MCDA package. GitHub: 2021, available from:
#'  \url{https://github.com/paterijk/MCDA/blob/master/}
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ELECTRE electre3 electre4 descending ascending distillation
#' @keywords pre-order
rankDF <- function(ranklist) {
  rank <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(rank) <- c("action", "rank")
  for (i in seq_along(ranklist)) { #for all ranks
    ls <- ranklist[[i]]
    for (j in seq_along(ls)) { #for all alternatives in rank
      rank[nrow(rank) + 1, ] <- list(ls[j], i)
    }
  }
  return(rank)
}
