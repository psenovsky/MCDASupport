#Concordance matrix
#C(a,b) = (1/W) * SUM{for all j: g_j(a)>=g_j(b)}(w_j), where W = SUM{from j=1 to n}w_j
ELECTRE_ConcordanceMatrix <- function(PM, w){

  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop('less than 2 criteria or 2 alternatives')
  if (!(is.matrix(PM) || (is.data.frame(PM)))) stop('wrong performance matrix, should be a matrix or a data frame')
  if (!(is.vector(w))) stop('criteria weights should be a vector')
  if (!is.numeric(w)) stop('criteria weights should be a numeric vector')
  if (ncol(PM)!=length(w)) stop('length of criteriaWeights should be checked')
  ## End of checking the validity of the "inputs"

  nalt <- nrow(PM)  #no. of alternatives
  ncri <- ncol(PM)  #no. of criteria
  alt  <- rownames(PM)

  #Concordance matrix
  #C(a,b) = (1/W) * SUM{for all j: g_j(a)>=g_j(b)}(w_j), where W = SUM{from j=1 to n}w_j
  cm <- matrix(data = 0, nrow = nalt, ncol = nalt)
  for(i in 1:nalt) {
    for (j in 1:nalt) {
      if(i != j){
        value <- sum(ifelse(PM[i,] >= PM[j,], w, 0))
        cm[i,j] <- value
      }
    }
  }
  rownames(cm) <- alt
  colnames(cm) <- alt
  if(sum(w) != 0) cm = cm/sum(w)
  diag(cm) <- 1
  cm <- round(cm, 2)
  return(cm)
}
