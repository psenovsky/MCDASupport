# identifies kernel of the decision based an adjacancy matrix presented in the am parameter
# function used in Electre_1 and Electre_1S functions.
#
# parameters
#   - am - adjacancy matrix
#
# returns network graph of the adjacancy matrix, ald vectors of kernel and dominated alternatives
ELECTRE1_Kernel <- function(am){
  graph <- plot.prefM(am)
  alt <- rownames(am)
  row_sum <- rowSums(am)
  names(row_sum) <- alt
  kernel  <- names(which(row_sum == 0))
  dominated <- setdiff(alt, kernel)

  out <- list(
    graph = graph,
    dominated = dominated,
    kernel = kernel
  )
  return(out)
}
