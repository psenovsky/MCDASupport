#' Transforms graph (a->b format) to adjncancy matrix
#'
#' @description
#' Function transforms back graph specified by edges to adjancancy matrix, with
#'  1 where edge exists and 0 where it does not.
#'
#' @param G Graph which should be transformed
#' @param alt alternatives names
#'
#' @return adjancancy matrix for the graph with 0/1 representing graph.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords graph adjancancy matrix
Graph2AdjancancyMatrix <- function(G, alt) {
  d <- as_adjacency_matrix(G, sparse = FALSE)
  t <- as.data.frame(d)
  if (ncol(t) != length(alt)) {
    stop("Number of alternatives is different then size of adj. matrix")
  }
  colnames(t) <- alt
  rownames(t) <- alt
  return(t)
}
