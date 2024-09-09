#' creates network diagram for visualizing preference directions of alternatives
#'
#' @description
#' Takes preference matrix (generally adjacancy matrix) and uses it to prepare
#'  directed circular network diagram showing outrank relation between the
#'  alternatives. The networke is then returned to calling object.
#'
#' The function has been revritten in 2023 as part of preparation for
#'  sensitivity analysis functionality addition to the package. Originaly it
#'  used plotmat fuction, which worked fine visually but instead of returning
#'  graph object it ploted it to screen, which was undesirable behavior for
#'  sensitivity analyses generating such graphs by dozens.
#'
#' This version of the function uses visNetwork package to solve this problem,
#'  generated networks also looks better, and the newrok is interactive by
#'  adding capability to highlight nodes to ease up visual analysis of the
#'  network.
#'
#' @param x preference matrix - values in matrix used to descibe strength of
#'  the relation. Generally it is adjacancy matrix.
#'
#' @return directed circular newrok diagram (visNetwork object) visualizing
#'  outranking relations between the alternatives
#'
#' @references
#' De Brouwer, Philippe J. S.: "The The Big R-Book: From Data Science to
#'  Learning Machines and Big Data ", Wiley, 2020, 928 p., ISBN  978-1119632726.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @examples
#' alternatives <- c('BLR', 'BOM', 'DEL', 'MNL', 'HYD', 'GRU', 'DUB', 'KRK',
#'   'MAA', 'EZE')
#'   M <- rbind(
#'     c(0, 0, 0, 1, 1, 0, 0, 0, 1, 0),
#'     c(0, 0, 1, 1, 1, 1, 0, 0, 1, 0),
#'     c(0, 0, 0, 1, 1, 0, 0, 0, 1, 0),
#'     c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#'     c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#'     c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
#'     c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#'     c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#'     c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#'     c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0)
#'   )
#'   rownames(M) <- alternatives
#'   colnames(M) <- alternatives
#'   plot.prefM(M)
#'
#' @keywords MCDA domination graph
plot.prefM <- function(x) {

  pm <- x

  # consistency check
  if (!(is.matrix(pm) || (is.data.frame(pm)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  nr <- ncol(pm)
  if (nr != nrow(pm)) {
    stop("Different number of alternatives in columns and rows")
  }
  if (nr < 2) stop("less than 2 alternatives")
  if (!is.numeric(unlist(pm))) {
    stop("Only numeric values in performance matrix expected")
  }

  # construct resulting network graph
  if (all(pm == 0)) {
    alt <- rownames(pm)
    df <- data.frame(id = seq_along(alt), label = alt)
    out <- visNetwork(nodes = df)
  } else {
    # prepare network graph
    G <- graph_from_adjacency_matrix(as.matrix(pm), mode = "directed",
                                     diag = FALSE, weighted = TRUE)
    G.visn <- toVisNetworkData(G)
    # following line adds weights to the network graph ... in ELECTRE II it
    # looks bad and is unnecessary as the weight = 1 for all edges anyway,
    # further experimentation with more advanced decision support method
    # required to cover more edge cases and make it look good
    #G.visn$edges$value <- G.visn$edges$weight
    out <- visNetwork(G.visn$nodes, G.visn$edges) %>%
      visNodes(shape = "elipse",
               color = list(background = "lightblue",
                            border = "darkblue",
                            highlight = "yellow"),
               shadow = list(enabled = TRUE, size = 10)) %>%
      visEdges(shadow = TRUE,
               width = 1,
               arrows = "middle") %>%
      visIgraphLayout(layout = "layout_in_circle")
  }
  return(out)
}
