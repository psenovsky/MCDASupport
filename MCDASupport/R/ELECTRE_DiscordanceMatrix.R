#discordance matrix - pyDecisions
#D(a,b) = 0 if g_j(a) >= g_j(b) for all j
#otherwise
#D(a,b)=max{for all j}[(g_j(b)-g_j(a))/max(g_j(c)-g_j(d))]
ELECTRE_DiscordanceMatrix <- function(PM) {

  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop('less than 2 criteria or 2 alternatives')
  if (!(is.matrix(PM) || (is.data.frame(PM)))) stop('wrong performance matrix, should be a matrix or a data frame')
  ## End of checking the validity of the "inputs"

  nalt <- nrow(PM)  #no. of alternatives
  ncri <- ncol(PM)  #no. of criteria
  alt  <- rownames(PM)

  dm <- matrix(data = 0, nrow = nalt, ncol = nalt)
  for(i in 1:nalt) {
    for(j in 1:nalt){
      if(i != j){
        mPM <- 0
        for(k in 1:ncri){
          range <- max(PM[,k]) - min(PM[,k])
          dPM <- ifelse(range == 0, 1, (PM[j,k] - PM[i,k])/range)
          mPM <- max(mPM, dPM)
        }
        dm[i,j] <- mPM
      }
    }
  }
  rownames(dm) <- alt
  colnames(dm) <- alt
  dm <- round(dm, 2)
  return(dm)
}
