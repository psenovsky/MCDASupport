#' algorithm for ascending distilation
#'
#' @description
#' Algorithm to establish partial preorder by the means of ascending
#'  distilation. Preorder is achieved by distilling alternatives using
#'  progresively lower cut-off thresholds.
#'
#' This aproach is complementary to descending distilation process which
#'  creates second pre-order.
#'
#' The algorithm is used in Electre III (\code{\link{electre3}}) and Electre IV
#'  (\code{\link{electre4}}) methods.
#'
#' @param sm confidence matrix
#'
#' @return list of alternatives in ranks from worst-to best.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @references 
#' Meyer, P. at al. MCDA package. GitHub: 2021, available from:
#'  \url{https://github.com/paterijk/MCDA/blob/master/}
#'
#' @keywords ELECTRE III
#' @keywords ELECTRE IV
Electre_asc_dist <- function(sm) {
  distilationasc <- list()
  alt <- rownames(sm)
  nalt <- nrow(sm)
  remaining <- 1:nalt
  lambda <- unique(sort(sm, decreasing = TRUE))
  l <- 1
  while (length(remaining) > 0 && l < length(lambda)) {
    cut <- apply(sm, 1:2, function(x) if (x >= lambda[l]) 1 else 0)
    scores <- sapply(remaining, function(x) {
      sum(cut[x, remaining]) - sum(cut[remaining, x])
    })
    minscore <- min(scores)
    distilationasc[[l]] <- alt[remaining[sapply(seq_along(scores), function(x) {
      scores[x] == minscore
    })]]
    remaining <- remaining[!sapply(seq_along(scores), function(x) {
      scores[x] == minscore
    })]
    l <- l + 1
  }
  if (length(remaining) > 0) distilationasc[[l]] <- alt[remaining]
  distilationasc <- rev(distilationasc)
  return(distilationasc)
}
