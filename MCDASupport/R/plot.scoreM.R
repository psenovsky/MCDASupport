# plot.scoreM
#
# Specific function for an object of class scoreM for the generic function
# plot
#
# Arguments:
#    PM - weighted preference matrix
#
# Returns
#   plot
plot.scoreM <- function(x, ...) {
  pm <- x
  #1. order the rows according to rowSums
  pm <- pm[order(rowSums(pm), decreasing = TRUE), ]
  pm$variant <- rownames(pm)
  t_pm <- tidyr::pivot_longer(pm, cols = -variant)
  fig <- plot_ly(t_pm,
                 x = ~variant,
                 y = ~value,
                 type = "bar",
                 name = ~name,
                 text = ~name,
                 color = ~name) %>%
    plotly::layout(yaxis = list(title = "Score"),
                   xaxis = list(title = "variants",
                                categoryorder = "array",
                                categoryarray = ~variant),
                   barmode = "stack",
                   title = "Overal score of the variants")
  return(fig)
}
