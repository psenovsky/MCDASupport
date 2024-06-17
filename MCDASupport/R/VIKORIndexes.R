# Function computes values of S_i and R_i
#
# parameters
#   car is the array with the performances (crisp alternative ratings),
#   bw_perf is the array with the best and worst performances, and differences
#           between them
#   cw is the criteria min/max array
#   v - weight for strategy of majority of the criteria (0-1)
#   return values of S_i (s) and R_i (r)
VIKORIndexes <- function(car, bw_perf, cw, v = NULL) {
  #validate params
  if (ncol(bw_perf) != 3) {
    stop("bw_perf parameter is expected to have 3 columns (best, worst
         and difference).")
  }
  nalt <- nrow(car)
  ncri <- ncol(car)
  alt <- rownames(car)
  if (nrow(bw_perf) != ncri) {
    stop("number of criteria detected in car and bw_perf differs")
  }
  if (length(cw) != ncri) {
    stop("number of criteria detected in car and cw differs")
  }
  #end of validation

  S <- sapply(1:nalt, function(i) {
    S_temp <- sapply(1:ncri, function(j) (cw[j] * (bw_perf[j, 1] - car[i, j])) / bw_perf[j, 3])
    sum(S_temp)
  })
  R <- sapply(1:nalt, function(i) {
    S_temp <- sapply(1:ncri, function(j) (cw[j] * (bw_perf[j, 1] - car[i, j])) / bw_perf[j, 3])
    max(S_temp)
  })

  S_min <- min(S) #S*
  S_max <- max(S) #S-
  R_min <- min(R) #R*
  R_max <- max(R) #R-
  Q <- v * (S - S_min) / (S_max - S_min) + (1 - v) * (R - R_min) / (R_max - R_min)

  names(S) <- alt
  names(R) <- alt
  names(Q) <- alt

  orderS <- sort(S)
  orderR <- sort(R)
  orderQ <- sort(Q)
  compromise <- list()
  if (orderQ[2] - orderQ[1] >= 1 / (nalt - 1)) { #C1 satisfied
    nS <- names(orderS)
    nR <- names(orderR)
    nQ <- names(orderQ)
    if ((v > 0.5 && nQ[1] == nS[1]) || (v < 0.5 && nQ[1] == nR[1]) ||
          (v == 0.5 && nQ[1] == nR[1] && nQ[1] == nS[1])) {
      compromise[1] <- nQ[1]
    } else { #C2 condition not satisfied
      compromise[1] <- nQ[1]
      compromise[2] <- nQ[2]
    }
  } else {#C1 condition is not satisfied
    for (i in 1:nalt) {
      if (i == 1) {
        compromise[1] <- nQ[1]
      } else {
        if (orderQ[i] - orderQ[1] < 1 / (nalt - 1)) {
          compromise[i] <- nQ[i]
        } else {
          break
        }
      }
    }
  }

  out <- list(
    S = orderS,
    R = orderR,
    Q = orderQ,
    compromiseSolution = compromise
  )
  return(out)
}
