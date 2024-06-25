# function to visualize alternatives kernel participation
#
# parameters
#   df ... dataframe with alternatives (boolean values for kernel participation)
#          and threshold with actual shreshold being used for computation.
#   title - main heading of the graph
#   x_label - label for X axis (usually concordance)
#   y_label - label for Y axis (usually discordance)
plot.threshold.3d <- function(df, title = "Kernel participation",
                              x_label = "concordance",
                              y_label = "discordance") {
  plot_data <- df %>%
    pivot_longer(cols = -c(concordance, discordance),
                 names_to = "alternative",
                 values_to = "value") %>%
    filter(value == 1)
  fig <- plot_ly(plot_data, x = ~concordance, y = ~discordance,
                 z = ~alternative, type = "scatter3d", mode = "markers") %>%
    layout(scene = list(xaxis = list(title = x_label),
                        yaxis = list(title = y_label),
                        zaxis = list(title = "Alternatives")),
           title = title)
  return(fig)
}