# Descending distillation algorithm used in Electre 3 and 4 methods
# parameters:
#   sm   - confidence matrix
Electre_asc_dist <- function(sm){
  distilationasc <- list()
  alt <- rownames(sm)
  nalt <- nrow(sm)
  remaining <- 1:nalt
  lambda <- unique(sort(sm, decreasing = TRUE))
  l <- 1
  while(length(remaining) > 0 && l < length(lambda))
  {
    cut <- apply(sm,1:2,function(x) if(x >= lambda[l]) 1 else 0)
    scores <- sapply(remaining,function(x) sum(cut[x,remaining]) - sum(cut[remaining,x]))
    minscore <- min(scores)
    distilationasc[[l]] <- alt[remaining[sapply(1:length(scores),function(x) scores[x] == minscore)]]
    remaining <- remaining[!sapply(1:length(scores),function(x) scores[x] == minscore)]
    l <- l+1
  }
  if(length(remaining) > 0) distilationasc[[l]] <- alt[remaining]
  distilationasc <- rev(distilationasc)
  return(distilationasc)
}
