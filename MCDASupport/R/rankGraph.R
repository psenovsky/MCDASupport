# generalization of graph plotting function for plotting changes in ranking of
# the alternatives as the result of changes of the parameter
#
# parameters
#   data ... data frame of alternatives ranks
#   x ... hyperparameter values
#   param ... name of the hyper parameter we are testing sensitivity of
rankGraph <- function(data, x, param = "") {

  # consistency check
  if (is.null(dim(data))) stop("no data presented to rankGraph")
  if (!is.data.frame(data)) stop("data must be data frame")
  if (!is.numeric(unlist(data))) stop("only numeric values in expected in data")
  if (nrow(data) > length(x)) {
    stop("data have more rows then number of elements in x labels vector")
  }
  # end of consistency check

  title <- paste0("Sensitivity to changes of ", param, " parameter")
  alt <- colnames(data)
  nalt <- length(alt)
  fig <- plot_ly(data = data,
                 y = ~data[, 1],
                 x = ~x[1:nrow(data)],
                 name = alt[1],
                 mode = "lines",
                 type = "scatter") %>%
    plotly::layout(
      title = title,
      xaxis = list(title = param),
      yaxis = list(title = "ranks",
                   tickmode = "linear",
                   dtick = 1,
                   tick0 = 1))
  for (i in 2:nalt) {
    fig <- fig %>%
      add_trace(y = data[, i], name = alt[i], mode = "lines")
  }
  return(fig)
}
