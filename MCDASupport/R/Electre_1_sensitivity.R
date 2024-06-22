# Sensitivity analysis of the Electre 1 function. Sensitivity test
# focuses on Concordance and Discordance thresholds.
#
# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   w  - weights
#   concordance_threshold ...c(from, to, step, default)
#   discordance_threshold ...c(from, to, step, default)
Electre_1_sensitivity <- function(PM, w,
                                  minmaxcriteria = "max",
                                  concordance_threshold,
                                  discordance_threshold) {
  # validate parameters
  if (!(is.matrix(PM) || (is.data.frame(PM)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (!(is.vector(concordance_threshold, mode = "numeric")) ||
        !(is.vector(discordance_threshold, mode = "numeric"))) {
    stop("thresholds expect numeric vectors only")
  }
  if (length(concordance_threshold) != 4 ||
        length(discordance_threshold) != 4) {
    stop("thresholds need to have 4 elements")
  }
  if (concordance_threshold[1] >= concordance_threshold[2] ||
        discordance_threshold[1] >= discordance_threshold[2]) {
    stop("thresholds: from must be < then to")
  }
  # end of validation

  alt <- rownames(PM)

  # internal function to prepare row of the dataframe
  df_row <- function(kernel, alt, val) {
    nalt <- length(alt)
    t <- rep(0, times = nalt + 1)
    for (k in kernel) {
      if (k %in% alt) {
        pos <- which(alt == k)
        t[pos] <- 1
      }
    }
    t[nalt + 1] <- val
    return(t)
  }

  c_t <- seq(from = concordance_threshold[1], to = concordance_threshold[2],
             by = concordance_threshold[3])
  c_def <- concordance_threshold[4]
  d_t <- seq(from = discordance_threshold[1], to = discordance_threshold[2],
             by = discordance_threshold[3])
  d_def <- discordance_threshold[4]

  # sensitivity of concordance threshold, c_t must be > d_def
  c_t2 <- c_t[c_t > d_def]
  df <- data.frame()
  for (ct in c_t2) {
    t <- Electre_1(PM = PM, w = w, minmaxcriteria = minmaxcriteria,
                   concordance_threshold = ct, discordance_threshold = d_def,
                   VERBOSE = FALSE, test = FALSE)
    df <- rbind(df, df_row(t$Kernel, alt, ct))
  }
  c_t_df <- df
  # discordance threshold sensitivity, dt must be < c_def
  d_t2 <- c_t[d_t < c_def]
  df <- data.frame()
  for (dt in d_t2) {
    t <- Electre_1(PM = PM, w = w, minmaxcriteria = minmaxcriteria,
                   concordance_threshold = c_def, discordance_threshold = dt,
                   VERBOSE = FALSE, test = FALSE)
    df <- rbind(df, df_row(t$Kernel, alt, dt))
  }
  d_t_df <- df
  colnames(c_t_df) <- colnames(d_t_df) <- c(alt, "threshold")
  c_vis <- plot.threshold(c_t_df, title = "Kernel - concordance sensitivity",
                          y_label = "concordance threshold")
  d_vis <- plot.threshold(d_t_df, title = "Kernel - discordance sensitivity",
                          y_label = "discordance threshold")

  t <- list(concordace_kernel = c_t_df,
            concordance_vis = c_vis,
            discordance_kernel = d_t_df,
            discordance_vis = d_vis)
  return(t)
}
