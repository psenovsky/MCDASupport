# function to create dataframe with ordered, ranked alternatives
# ranklist - ranked list, each element in the list represents one rank. Each element in rank list can consist of
# multiple alternatives. This list needs to be flattened into dataframe in structure action and rank
rankDF <- function(ranklist){
  rank <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(rank) <- c('action', 'rank')
  for(i in 1:length(ranklist)){ #for all ranks
    ls <- ranklist[[i]]
    for(j in 1:length(ls)){ #for all alternatives in rank
      rank[nrow(rank)+1,] <- list(ls[j], i)
    }
  }
  return(rank)
}
