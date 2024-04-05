# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   P  - preference threshold
#   Q  - indifference threshold
#   V  - veto threshold
#   VERBOSE - TRUE to send function outputs to console
#
# solution for the problem is dominance matrix which can be visualized into network graph
Electre_4 <- function(PM, P, Q, V, minmaxcriteria = 'max', VERBOSE = FALSE){

  # check validity of the objects manipulated by the current function
  Electre_4_paramCheck(PM = PM, P = P, Q = Q, V = V, minmaxcriteria = minmaxcriteria)

  PM <- util_pm_minmax(PM, minmaxcriteria) #validate minmax and invert scales if necessary
  ncri  <- ncol(PM)  #no. of criteria
  nalt <- nrow(PM) #no. of alternatives
  alt  <- rownames(PM) #names of alternatives

  #count outranking relations
  d  <- matrix(data = 0, nrow = nalt, ncol = nalt)  # template zero matrix of n x n, n = no. of alternatives
  colnames(d) <- alt
  rownames(d) <- alt
  mp <- d #bFa: number of criteria for which option b is strictly preferred to a
  mq <- d #bSa: number of criteria for which option b is weakly preferred to a
  mi <- d #bIa: number of criteria for which option b is judged indifferent to a
  mo <- d #mo(b,a) = mo(a,b) : the number of criteria on which options a and b perform identically
  veto  <- d
  for(i in 1:nalt){
    for(j in 1:nalt){
      if (i != j){
        diff <- PM[j,] - PM[i,]
        mp[i,j] <- mp[i,j] + sum(diff > P)
        mq[i,j] <- mq[i,j] + sum(diff > Q & diff <= P)
        mi[i,j] <- mi[i,j] + sum(diff >= -Q & diff <= Q & diff > 0)
        mo[i,j] <- mo[i,j] + sum(diff == 0)
        veto[i,j] <- veto[i,j] + sum(diff >= V)
      }
    }
  }
  #credibility matrix
  cred_matrix <- d
  for(i in 1:nalt){
    for(j in 1:nalt){
      if (i != j){
        diff_mp <- mp[i,j]
        diff_mq <- mq[i,j]
        diff_mi <- mi[i,j]
        diff_ji <- mi[j,i] + mq[j,i] + mp[j,i]
        if (diff_mp + diff_mq == 0 && diff_mi < diff_ji){
          #Quasi-dominance Sq: b outranks a with quasi-dominance if b Sq a <=> mp(a,b) + mq(a,b) = 0 and mi(a,b) < mi(b,a) + mq(b,a) + mp(b,a)
          cred_matrix[i,j] = 1.0
        } else if(diff_mp == 0 && diff_mq <= mq[j,i] && diff_mq + diff_mi <= diff_ji + 1) {
          # Canonical dominance Sc: b outranks a with canonical dominance if b Sc a <=> mp(a,b) = 0, and mq(a,b) <= mq(b,a), and mq(a,b) + mi(a,b) <= mi(b,a) + mq(b,a) + mp(b,a) + 1
          cred_matrix[i,j] = 0.8
        } else if (diff_mp == 0 && diff_mq <= mq[j,i] + mp[j,i]) {
          # Pseudo-dominance Sp: b outranks a with pseudo-dominance if b Sp a <=> mp(a,b) = 0, and II1q(a,b) S; II1q(b,a) + mp(b,a)
          cred_matrix[i,j] = 0.6
        } else if (diff_mp == 0) {
          cred_matrix[i,j] = 0.4
        } else if(diff_mp == 1 && mp[j,i] >= ncri/2) {
          # Veto dominance Sv: b outranks a with veto-dominance if mp(a,b) = 1, b Sv a: mp(b,a) >= m/2, and gj(b) + vj[gj(b)] >= gj(a)
          le <- all(PM[j,] + V >= PM[i,])
          if(le) cred_matrix[i,j] = 0.2
        } else if(diff_mp == 0){
          # Veto dominance Sv: b outranks a with veto-dominance if mp(a,b) = 0
          cred_matrix[i,j] = 0.2
        }
      }
    }
  }

  distilationdesc <- Electre_desc_dist(cred_matrix)     #descending distillation
  rank_D <- rankDF(distilationdesc)
  distilationasc <- Electre_asc_dist(cred_matrix)       #ascending distillation
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

  if(VERBOSE){
    print('Credibility matrix')
    print(cred_matrix)
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
    CredibilityMatrix = cred_matrix,
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
