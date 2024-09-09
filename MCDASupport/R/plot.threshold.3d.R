#' Plots alternatives participation in kernel with changing concordance and
#'  discordance thresholds
#'
#' @description
#' Plot visualized participation of the alternatives in the kernel in
#'  \code{\link{electre1}} function.
#'
#' Alternatives are organized on z axis. X represents concordance and y axis
#'  represents discordance threshold. Visually each alternative has its own
#'  row which allows us to explore how the participation in kernel changes in
#'  3D.
#'
#' Look for missing dots for indication that the alternative is not
#'  participating in the kernel at that level of concordance and discordance
#'  threshold.
#'
#' Function is constructed similarly to \code{\link{plot.threshold}}, but
#'  produces 3D graph. The altarnatives participation is indicated only by 0/1.
#'
#' @param df dataframe with alternatives (0/1) and concordance, discordance
#'  columns
#' @param title main title of the graph
#' @param x_label label of the x axis representing concordance threshold
#' @param y_label label of the y axis representing discordance threshold
#'
#' @return a graph representing participation of the alternative in kernel with
#'  changing concordance and discordance threshold
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords concordance discordance threshold electre1 electre2 sensitivity
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