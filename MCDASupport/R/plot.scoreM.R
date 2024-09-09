#' Function to plot contribution of the criteria to overal performance of the
#'  alternatives.
#'
#' @description
#' Takes weighted preference matrix and uses it to plot stacked bar chart
#'  describing how the criteria (forming the stocks in graph) contribute to the
#'  overal score of the alternatives. Visualized in form of the ordered bar
#'  plot.
#'
#' This is usefull especially for methods such as \link{wsm}, which derives
#'  results directly from performance matrix.
#'
#' @param x weighted preference matrix - values in matrix used to descibe
#'  strength of the relation.
#'
#' @return stocked bar chart of criteria contributions to overal performance
#' of the alternative
#'
#' @references
#' De Brouwer, Philippe J. S.: "The The Big R-Book: From Data Science to
#'  Learning Machines and Big Data ", Wiley, 2020, 928 p., ISBN  978-1119632726.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @examples
#' KoboGlo <- c(80, 97, 50, 100, 100, 94)
#' SonyPRST3 <- c(80, 88, 100, 20, 70, 70)
#' KindlePaperwhite2 <- c(100, 85, 50, 100, 70, 100)
#' PBTouchLux <- c(80, 90, 100, 100, 70, 94)
#' BookeenCybookOdyssey <- c(80, 100, 100, 100, 85, 50)
#' criteria <- c("Display", "weight", "HWButtons", "FrontLight", "batery",
#'   "price")
#' PM <- as.data.frame(
#'   rbind(KoboGlo,
#'         SonyPRST3,
#'         KindlePaperwhite2,
#'         PBTouchLux,
#'         BookeenCybookOdyssey))
#' names(PM) <- criteria
#' w <- c(5, 3, 4, 5, 2, 1)
#' preferences <- mcda_wsm(PM, w, 'max')
#' plot.scoreM(preferences$weightedPM)
#'
#' @keywords WSM performance score MCDA
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
