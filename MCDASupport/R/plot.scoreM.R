# plot.scoreM
#
# Specific function for an object of class scoreM for the generic function plot()
#
# Arguments:
#    PM - weighted preference matrix
#
# Returns
#   plot
plot.scoreM <- function(x, ...) {
  PM <- x
  #1. order the rows according to rowSums
  PM <- PM[order(rowSums(PM), decreasing = T),]
  PM$variant <- rownames(PM)
  tPM <- tidyr::pivot_longer(PM, cols = -variant)
  fig <- plot_ly(tPM,
                 x = ~variant,
                 y = ~value,
                 type = 'bar',
                 name = ~name,
                 text = ~name,
                 color = ~name) %>%
    plotly::layout(yaxis = list(title = 'Score'),
                   xaxis = list(title = 'variants',
                                categoryorder = 'array',
                                categoryarray = ~variant),
                   barmode = 'stack',
                   title = 'Overal score of the variants')
  return(fig)
}
