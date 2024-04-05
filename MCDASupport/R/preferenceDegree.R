# preference degree is being computed in PROMETHEE and SIR functions
#
# parameters:
#   - nalt - number of alternatives
#   - ncri - number of criteria
#   - DK - result of paiweise comparison
#   - d - type of function (default, U-shape, V-shape, level)
#   - qj - indifference threshold
#   - pj - preference threshold
#   - sj - intermediate threshold
#   - alt - vector of alternative's names
#   - cri - vestor of criteria's names
preferenceDegree <- function(nalt, ncri, DK, d, qj, pj, sj, alt, cri) {
  Pj <- lapply(1:ncri, function(k) {
    DKf <- DK[[k]]
    PjK <- outer(1:nalt, 1:nalt, Vectorize(function(i, j) {
      if(d[k] == 'default'){
        if(DKf[i,j] <= 0) 0 else 1
      } else if(d[k] == 'U-shape'){
        if(DKf[i,j] <= qj[k]) 0 else 1
      } else if(d[k] == 'V-shape'){
        if(DKf[i,j] <= 0) 0 else if(DKf[i,j] > 0 & DKf[i,j] <= pj[k]) DKf[i,j]/pj[k] else 1
      } else if(d[k] == 'level'){
        if(DKf[i,j] <= qj[k]) 0 else if(DKf[i,j] > qj[k] & DKf[i,j] <= pj[k]) 0.5 else 1
      } else if(d[k] == 'linear'){
        if(DKf[i,j] <= qj[k]) 0 else if(DKf[i,j] > qj[k] & DKf[i,j] <= pj[k]) (DKf[i,j] - qj[k])/(pj[k] - qj[k]) else 1
      } else if(d[k] == 'Gaussian'){
        if(DKf[i,j] <= 0) 0 else 1 - exp(-((DKf[i,j]*DKf[i,j])/(2*sj[k]*sj[k])))
      }
    }))
    rownames(PjK) <- alt
    colnames(PjK) <- alt
    PjK
  })
  names(Pj) <- cri
  return(Pj)
}
