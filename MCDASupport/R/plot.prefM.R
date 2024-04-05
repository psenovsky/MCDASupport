# plot.prefM
#
# Specific function for an object of class prefM for the generic function plot()
#
# Arguments:
#    x - adjacency matrix representing outranking relation to be visualized by graph
#
# Returns
#   plot generated as visNetwork object
plot.prefM <- function(x) {

  PM <- x

  # consistency check
  if (!(is.matrix(PM) || (is.data.frame(PM)))) stop('wrong performance matrix, should be a matrix or a data frame')
  nr <- ncol(PM)
  if (nr != nrow(PM)) stop('Different number of alternatives in columns and rows')
  if (nr < 2) stop('less than 2 alternatives')
  if (!is.numeric(unlist(PM))) stop('Only numeric values in performance matrix expected')

  # construct resulting network graph
  if(all(PM == 0)) {
    alt <- rownames(PM)
    df <- data.frame(id = 1:length(alt), label = alt)
    out <- visNetwork(nodes = df)
  }else{
    # prepare network graph
    G <- graph_from_adjacency_matrix(as.matrix(PM), mode = 'directed', diag = FALSE, weighted = TRUE)
    G.visn <- toVisNetworkData(G)
    # following line adds weights to the network graph ... in ELECTRE II it looks bad and is unnecessary as the weight = 1 for all edges anyway, further experimentation with more advanced decision support method
    # required to cover more edge cases and make it look good
    #G.visn$edges$value <- G.visn$edges$weight
    out <- visNetwork(G.visn$nodes, G.visn$edges) %>%
      visNodes(shape = 'elipse',
               color = list(background = "lightblue",
                            border = "darkblue",
                            highlight = "yellow"),
               shadow = list(enabled = TRUE, size = 10)) %>%
      visEdges(shadow = TRUE,
               width = 1,
               arrows = 'middle') %>%
      visIgraphLayout(layout = 'layout_in_circle')
  }
  return(out)
}
