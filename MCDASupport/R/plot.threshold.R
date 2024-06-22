# function to visualize alternatives kernel participation
#
# parameters
#   df ... dataframe with alternatives (boolean values for kernel participation)
#          and threshold with actual shreshold being used for computation.
#   title - main heading of the graph
#   y_label - label for the Y axis of the graph
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