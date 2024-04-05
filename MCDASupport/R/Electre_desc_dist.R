# Descending distillation algorithm used in Electre 3 and 4 methods
# parameters:
#   sm   - confidence matrix
Electre_desc_dist <- function(sm){
  distilationdesc <- list()
  alt <- rownames(sm)
  nalt <- nrow(sm)
  remaining <- 1:nalt
  lambda <- unique(sort(sm, decreasing = TRUE))
  l <- 1
  while(length(remaining) > 0 && l < length(lambda))
  {
    cut <- apply(sm,1:2,function(x) if(x >= lambda[l]) 1 else 0)
    scores <- sapply(remaining,function(x) sum(cut[x,remaining]) - sum(cut[remaining,x]))
    maxscore <- max(scores)
    distilationdesc[[l]] <- alt[remaining[sapply(1:length(scores),function(x) scores[x] == maxscore)]]
    remaining <- remaining[!sapply(1:length(scores),function(x) scores[x] == maxscore)]
    l <- l + 1
  }
  if(length(remaining) > 0) distilationdesc[[l]] <- alt[remaining]
  return(distilationdesc)
}
