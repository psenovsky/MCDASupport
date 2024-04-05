# Sensitivity analysis of the Electre 1 function. Sensitivity test focuses on Concordance and Discordance thresholds.
#
# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   w  - weights
#   concordance_threshold = c(from, to, step, default)
#   discordance_threshold = c(from, to, step, default)
Electre_1_sensitivity <- function(PM, w,
                                  minmaxcriteria = 'max',
                                  concordance_threshold,
                                  discordance_threshold) {

  # validate parameters
  if (!(is.vector(concordance_threshold)) || !(is.vector(discordance_threshold))) stop('thresholds muset be set as vectors for the sensitivity analysis should be a vector')
  if (length(concordance_threshold) != length(discordance_threshold) || length(discordance_threshold) != 4) stop('thresholds need to have 4 elements')
  if (!is.numeric(concordance_threshold) || !is.numeric(discordance_threshold)) stop('threshold parameters need to be numbers')
  if (concordance_threshold[1] > concordance_threshold[2] || discordance_threshold[1] > discordance_threshold[2]) stop('thresholds: from must be < then to')
  if (concordance_threshold[1] == concordance_threshold[2] || discordance_threshold[1] == discordance_threshold[2]) stop('thresholds: from = to, no possible changes to test')

  alt <- rownames(PM)
  nalt <- length(alt)

  # internal function to prepare row of the dataframe
  df_row <- function(kernel, alt, sensitivity, val){
    nalt <- length(alt)
    t <- rep(0, times = nalt + 2)
    for(k in kernel){
      if(k %in% alt){
        pos <- which(alt == k)
        t[pos] <- 1
      }
    }
    t[nalt+1] <- sensitivity
    t[nalt+2] <- val
    return(t)
  }

  c_t <- seq(from = concordance_threshold[1], to = concordance_threshold[2], by = concordance_threshold[3])
  c_def <- concordance_threshold[4]
  d_t <- seq(from = discordance_threshold[1], to = discordance_threshold[2], by = discordance_threshold[3])
  d_def <- discordance_threshold[4]
  df <- data.frame()

  # sensitivity of concordance threshold, c_t must be > d_def
  for(ct in c_t){
    if(ct > d_def){
      t <- Electre_1(PM = PM, w = w, minmaxcriteria = minmaxcriteria, concordance_threshold = ct, discordance_threshold = d_def, VERBOSE = FALSE)
      df <- rbind(df, df_row(t$Kernel, alt, 'C', ct))
    }
  }
  # discordance threshold sensitivity, dt must be < c_def
  for(dt in d_t){
    if(dt < c_def){
      t <- Electre_1(PM = PM, w = w, minmaxcriteria = minmaxcriteria, concordance_threshold = c_def, discordance_threshold = dt, VERBOSE = FALSE)
      df <- rbind(df, df_row(t$Kernel, alt, 'D', dt))
    }
  }
  colnames(df) <- c(alt, 'sensitivity', 'treshold value')
  return(df)
}
