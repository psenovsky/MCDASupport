#Concordance matrix
#C(a,b) = (1/W) * SUM{for all j: g_j(a)>=g_j(b)}(w_j),
# where W = SUM{from j=1 to n}w_j
ELECTRE_ConcordanceMatrix <- function(PM, w) {

  # with < 2 criteria or alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(PM) || (is.data.frame(PM)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (!(is.vector(w, mode = "numeric"))) {
    stop("criteria weights should be a numeric vector")
  }
  nalt <- nrow(PM)  #no. of alternatives
  ncri <- ncol(PM)  #no. of criteria
  alt  <- rownames(PM)
  if (ncri != length(w)) {
    stop("number of weights must be same as number of criteria")
  }
  ## End of checking the validity of the "inputs"

  # Concordance matrix
  # C(a,b) = (1/W) * SUM{for all j: g_j(a)>=g_j(b)}(w_j),
  # where W = SUM{from j=1 to n}w_j
  cm <- matrix(data = 0, nrow = nalt, ncol = nalt)
  for (i in 1:nalt) {
    for (j in 1:nalt) {
      if (i != j) {
        cm[i, j] <- sum(ifelse(PM[i, ] >= PM[j, ], w, 0))
      }
    }
  }
  rownames(cm) <- alt
  colnames(cm) <- alt
  if (sum(w) != 0) cm <- cm / sum(w)
  diag(cm) <- 1
  cm <- round(cm, 2)
  return(cm)
}
