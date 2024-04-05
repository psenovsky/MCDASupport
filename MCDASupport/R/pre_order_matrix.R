# compute pre-order matrix for Electre 3 and 4 methods
# parameters:
#   rank_D - descending distillation ranking
#   rank_A - ascending distillation ranking
#   alt    - all alternatives
pre_order_matrix <- function(rank_D, rank_A, alt){
  nalt <- length(alt)
  po_string <- matrix('-', nalt, nalt)
  rownames(po_string) <- alt
  colnames(po_string) <- alt
  for(i in 1:nalt){
    Da <- filter(rank_D, action == alt[i]) #descending ranks
    Aa <- filter(rank_A, action == alt[i]) #ascending ranks
    for(j in 1:nalt){
      if(i < j){
        Db <- filter(rank_D, action == alt[j]) #descending ranks
        Ab <- filter(rank_A, action == alt[j]) #ascending ranks

        Da_less_Db <- Da$rank < Db$rank
        Da_eq_Db <- Da$rank == Db$rank
        Aa_less_Ab <- Aa$rank < Ab$rank
        Aa_eq_Ab <- Aa$rank == Ab$rank

        if((Da_less_Db && Aa_less_Ab) || (Da_eq_Db && Aa_less_Ab) || (Da_less_Db && Aa_eq_Ab)){
          po_string[i,j] <- 'P+'
          po_string[j,i] <- 'P-'
        }else if((!Da_less_Db && !Aa_less_Ab) || (Da_eq_Db && !Aa_less_Ab) || (!Da_less_Db && Aa_eq_Ab)){
          po_string[i,j] <- 'P-'
          po_string[j,i] <- 'P+'
        }else if(Da_eq_Db && Aa_eq_Ab){
          po_string[i,j] <- 'I'
          po_string[j,i] <- 'I'
        }else{
          #if((alts_D[i] > alts_D[j] && alts_A[i] < alts_A[j]) || (alts_D[i] < alts_D[j] && alts_A[i] > alts_A[j])){
          po_string[i,j] = 'R'
          po_string[j,i] = 'R'
        }
      }
    }
  }
  return(po_string)
}
