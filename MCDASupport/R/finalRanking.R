# final ranking for Electre 3 a 4 methods
# parameters:
#   alt - alternatives
#   rank_P - pre-rank matrix
#
# returns final_ranking and final ranking unsorted
finalRanking <- function(alt, rank_P) {

  compte <- function(symbole, seq) {
    seqbinaire <- rep(0, length(seq))
    seqbinaire[seq == symbole] <- 1
    nb <- sum(seqbinaire)
    return(nb)
  }

  nalt <- length(alt)
  nalt_zeros <- rep(0, times = nalt)
  final_ranking <- data.frame(alternative = alt,
                              sum_outrank = nalt_zeros,
                              ranking = nalt_zeros)
  final_ranking$stringsAsFactors <- FALSE
  for (i in 1:nalt) { #sum_outrank
    seq <- rank_P[final_ranking[i, 1], ]
    final_ranking[i, 2] <- compte("P+", seq)
  }
  final_ranking <- final_ranking[order(-final_ranking[, 2],
                                       final_ranking[, 1]), ]
  j <- 1
  for (i in 1:(nalt - 1)) { #ranking
    if (i == 1) {
      if (rank_P[final_ranking[i, 1], final_ranking[i + 1, 1]] == "P+") {
        final_ranking[i, 3] <- j
        final_ranking[i + 1, 3] <- j + 1
        j <- j + 1
      }
      if (rank_P[final_ranking[i, 1], final_ranking[i + 1, 1]] == "I") {
        final_ranking[i, 3] <- i
        final_ranking[i + 1, 3] <- i
        j <- j + 1
      }
    } else { # for i > 1
      if (rank_P[final_ranking[i, 1], final_ranking[i + 1, 1]] == "P+") {
        if (final_ranking[i, 2] > 1) {
          final_ranking[i, 3] <- j
          final_ranking[i + 1, 3] <- j + 1
          j <- j + 1
        } else if (final_ranking[i + 1, 2] == 0) {
          final_ranking[i, 3] <- j
          final_ranking[i + 1, 3] <- j + 1
          j <- j + 1
        }
      } else if (rank_P[final_ranking[i, 1], final_ranking[i + 1, 1]] == "R") {
        if (final_ranking[i + 1, 2] == 0) {
          final_ranking[i, 3] <- j
          final_ranking[i + 1, 3] <- final_ranking[i, 3] + 1
        } else {
          final_ranking[i + 1, 3] <- final_ranking[i, 3]
        }
      } else if (rank_P[final_ranking[i, 1], final_ranking[i + 1, 1]] == "I") {
        final_ranking[i + 1, 3] <- final_ranking[i, 3]
      }
    }
  }

  frs <- final_ranking$ranking
  names(frs) <- final_ranking$alternative
  finalRankingUnsorted <- rep(0, times = nalt)
  names(finalRankingUnsorted) <- alt
  for(i in 1:nalt){
    finalRankingUnsorted[names(frs[i])] <- frs[i]
  }

  out <- list(
    final_ranking = final_ranking,
    finalRankingUnsorted = finalRankingUnsorted
  )
  return(out)
}