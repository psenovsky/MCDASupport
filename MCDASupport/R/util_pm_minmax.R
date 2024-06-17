util_pm_minmax <- function(PM, minmaxcriteria = "max") {
  minmax <- util_prepare_minmaxcriteria(ncol(PM), minmaxcriteria)
  if (length(minmax[minmax %in% c("min")]) > 0) {
    # some criteria are minimalized - need to invert them
    i <- 0
    for (r in minmax) {
      i <- i + 1
      if (r == "min") { #invert scale
        valmax <- max(PM[, i])
        PM[, i] <- valmax - PM[, i]
      }
    }
  }
  PM <- as.data.frame(PM)
  return(PM)
}
