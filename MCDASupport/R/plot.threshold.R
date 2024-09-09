#' Function to plot alternatives participation kernel when changing concordance
#'  or discordance threshold
#'
#' @description
#' Plot visualized participation of the alternatives on the kernel in
#'  \code{\link{electre1}} function.
#'
#' Alternatives are organized on x axis. Visually each alternative has its own
#'  column to explore the participation. Participation is by point in the
#'  graph. If the alternative at given threshold does not participate, the
#'  point will not be rendered in the graph.
#'
#' The graph is ploting only one threshold at the time: either concordance or
#'  discordance threshold.
#'
#' @param df dataframe with alternatives and threshold
#' @param title main title of the graph
#' @param y_label label of the y axis, usually concordance or discordance
#'  threshold, default value threshold
#'
#' @return plot of alternatives participation kernel when changing concordance
#'  or discordance threshold
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords kernel plot concordance discordance threshold sensitivity electre1
#' @keywords electre2
plot.threshold <- function(df, title = "Kernel participation",
                           y_label = "threshold") {
  plot_data <- df %>%
    pivot_longer(cols = -threshold,
                 names_to = "alternative",
                 values_to = "value") %>%
    filter(value == 1)
  fig <- plot_ly(plot_data, x = ~alternative, y = ~threshold, type = "scatter",
                 mode = "markers") %>%
    plotly::layout(yaxis = list(title = y_label),
                   xaxis = list(title = "alternatives"),
                   title = title)
  return(fig)
}