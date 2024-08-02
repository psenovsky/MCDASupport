#' algrithm for descending distilation
#'
#' @description
#' Algorithm to establish partial preorder by the means of descending
#'  distilation. Preorder is achieved by distilling alternatives using
#'  progresively lower cutoff thresholds.
#'
#' This aproach is complementary to ascending distilation process
#'  \code{\link{Electre_asc_dist}} which creates second preorder.
#'
#' The algorithm is used in \code{\link{electre3}} and
#' \code{\link{electre4}} methods.
#'
#' @param sm confidence matrix
#'
#' @return list of alternatives in ranks from worst to best.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @references
#' Meyer, P. at al. MCDA package. GitHub: 2021, available from:
#'  \url{https://github.com/paterijk/MCDA/blob/master/}
#'
#' @keywords Electre methods
#' @keywords ELECTRE III
#' @keywords ELECTRE IV
#' @keywords descending distillation
#' @keywords pre-order
#'
#' @seealso \code{\link{Electre_asc_dist}}
#' @seealso \code{\link{electre3}}
#' @seealso \code{\link{electre4}}
Electre_desc_dist <- function(sm) {
  distilationdesc <- list()
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
    maxscore <- max(scores)
    distilationdesc[[l]] <- alt[remaining[sapply(seq_along(scores), function(x) {
      scores[x] == maxscore
    })]]
    remaining <- remaining[!sapply(seq_along(scores), function(x) {
      scores[x] == maxscore
    })]
    l <- l + 1
  }
  if (length(remaining) > 0) distilationdesc[[l]] <- alt[remaining]
  return(distilationdesc)
}
