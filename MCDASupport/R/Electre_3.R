# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   w  - weights
#   P  - preference threshold
#   Q  - indifference threshold
#   V  - veto threshold
#   alpha - first discrimination threshold
#   beta  - second discrimination threshold
#   VERBOSE - in verbose mode the function prints out higher amount of information on computation
#
# solution for the problem is dominance matrix which can be visualized into network graph
Electre_3 <- function(PM, w, P, Q, V, minmaxcriteria = 'max',
                      alpha = 0.3, beta = 0.15, VERBOSE = FALSE){

  Electre_3_paramCheck(PM, w, P, Q, V, minmaxcriteria, alpha, beta) # common consistency check (common with Electre_3_sensitivity function)
  PM <- util_pm_minmax(PM, minmaxcriteria) #validate minmax and invert scales if necessary
  ncri <- ncol(PM)  #no. of criteria
  nalt  <- nrow(PM)  #no. of alternatives
  alt   <- rownames(PM)
  alt_D <- rep(0, times=nalt)
  alt_A <- rep(0, times=nalt)

  t <- Electre3_ConcordanceIndex(PM, P, Q, w)
  cm <- t$ConcordanceMatrix
  dj <- list()  #discordance matrix for criteria (j)
  d  <- matrix(data = 0, nrow = nalt, ncol = nalt)  # template zero matrix of n x n, n = no. of alternatives
  colnames(d) <- alt
  rownames(d) <- alt
  for(k in 1:ncri){      #index for criteria
    d2 <- d #temp var for dj
    for(i in 1:nalt){    #index for a
      for(j in 1:nalt){  #index for b
        #discordance matrix
        # Dj(a,b) = 0 if PMj(b) <= PMj(a) + Pj(a)
        # Dj(a,b) = 1 if PMj(b) > PMj(a) + Vj(a)
        # otherwise Dj(a,b) = (PMj(b)-PMj(a) - Pj(a))/ (Vj(a) - Pj(a))
        if(PM[j,k] <= PM[i,k] + P[k]){
          d2[i,j] <- 0
        }else if(PM[j,k] > PM[i,k] + V[k]){
          d2[i,j] <- 1
        }else{
          if(V[k] - P[k] != 0){
            d2[i,j] <- (PM[j,k] - PM[i,k] - P[k])/(V[k] - P[k])
          }else{
            d2[i,j] <- 1
          }
        }
      }
    }
    dj[[k]]  <- d2
  }

  # degree of credibility
  # S(a,b) = C(a,b) if Dj(a,b) <= C(a,b)
  # otherwise S(a,b) = C(a,b) * PRODUCT{for j in J}((1-Dj(a,b))/(1-C(a,b)))
  # where J is set of criteria for which Dj(a,b) > C(a,b)
  sm <- d # credibility matrix
  # sm  <- matrix(data = 0, nrow = nalt, ncol = nalt)  # credibility matrix
  # colnames(sm) <- alt
  # rownames(sm) <- alt
  for(i in 1:nalt){
    for(j in 1:nalt){
      t <- 1 #temporary var. for computation of product part of the equation
      for(k in 1:ncri){
        dj_temp <- dj[[k]]
        if(dj_temp[i,j] > cm[i,j]) t <- t * ((1-dj_temp[i,j])/(1-cm[i,j]))
      }
      sm[i,j] <- cm[i,j] * t
    }
  }
  distilationdesc <- Electre_desc_dist(sm) #descending distillation
  rank_D <- rankDF(distilationdesc)
  distilationasc <- Electre_asc_dist(sm)       #ascending distillation
  rank_A <- rankDF(distilationasc)
  rank_P <- pre_order_matrix(rank_D, rank_A, alt)
  t_fr <- finalRanking(alt, rank_P)
  final_ranking <- t_fr$final_ranking
  #construct adjancancy matrix
  names_matrank_adj=final_ranking[,1]
  matrank_adj <- matrix(data = 0, nrow = nalt, ncol = nalt)
  rownames(matrank_adj)=names_matrank_adj
  colnames(matrank_adj)=names_matrank_adj
  PIR <- c('P+', 'I', 'R')
  for (i in 1:nalt){
    for (j in 1:nalt){
      if(final_ranking[j,3]-final_ranking[i,3] == 1 && rank_P[final_ranking[i,1],final_ranking[j,1]] %in% PIR){
        matrank_adj[i,j] = 1
      }
    }
  }
  graph <- plot.prefM(matrank_adj)

  if(VERBOSE){
    print('Concordance matrix')
    print(t(cm))
    print('Discordance matrixes of criteria')
    print(dj)
    print('Credibility matrix')
    print(sm)
    print('First pre-order (down)')
    print(rank_D)
    print('First pre-order (up)')
    print(rank_A)
    print('total')
    print(rank_P)
    print('Adjancancy matrix')
    print(matrank_adj)
    print('Final ranking')
    print(final_ranking)
    print('Unsorted final ranking')
    print(t_fr$finalRankingUnsorted)
  }

  out <- list(
    PerformanceMatrix = PM,
    ConcordanceMatrix = cm,
    DiscordanceMatrixCriteria = dj,
    CredibilityMatrix = sm,
    rank_D = rank_D,
    rank_A = rank_A,
    rank_P = rank_P,
    adjancancyMatrix = matrank_adj,
    graph = graph,
    final_ranking = final_ranking,
    finalRankingUnsorted = t_fr$finalRankingUnsorted
  )
  return(out)
}
