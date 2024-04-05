# Takes graph and transforms it back to adjancancy matrix
# parameters:
#   G - Graph which should be transformed}
#   alt - alternatives names
Graph2AdjancancyMatrix <- function(G, alt){
  d <- as_adjacency_matrix(G, sparse = FALSE)
  t <- as.data.frame(d)
  if(ncol(t) != length(alt)) stop('Number of alternatives is different then size of adj. matrix')
  colnames(t) <- alt
  rownames(t) <- alt
  return(t)
}
