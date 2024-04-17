# Electre 2 function
#
# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   w  - weights
#   minmaxcriteria - information on "direction" of the criteria
#                    (min/max, max is default)
#   c-, c0 and c+ fuzzy concordance threshold
#   VERBOSE = FALSE - do not output results to console
#
# solution for the problem is dominance matrix which can be visualized
# into network graph
Electre_2 <- function(PM, w,
                      minmaxcriteria = 'max',
                      c_minus = 0.65, c_zero = 0.75, c_plus = 0.85,
                      d_minus = 0.25, d_plus = 0.5, VERBOSE = FALSE) {

  # common consistency check (common with Electre_2_sensitivity function)
  Electre_2_paramCheck(PM, w) 
  ## check validity of the objects manipulated by the current function
  #validate minmax and invert scales if neccessary
  PM <- util_pm_minmax(PM, minmaxcriteria)
  t <- c(c_minus, c_zero, c_plus, d_minus, d_plus)
  if (length(t[t > 1]) > 0 || length(t[t < 0]) > 0) {
    stop("c & d thresholds need to be in <0;1>")
  }
  if (c_minus > c_zero) stop("Concordance level c- > c0 (must be c- <= c0)")
  if (c_zero > c_plus) stop("Concordance level c0 > c+ (must be c0 <= c+)")
  if (d_minus > d_plus) stop("Discordance level d- > d+ (must be d- <= d+)")
  ## End of checking validity of the inputs

  nalt  <- nrow(PM)  #no. of alternatives
  alt   <- rownames(PM)

  # computes preference ratio P+/P- and constructs strong or weak outranking
  # matrixes in dominance_s or dominance_w
  preference_ratio <- function(dom, pm) {
    nalt <- length(rownames(pm))
    s <- matrix(data = 0, nrow = nalt, ncol = nalt)
    for (i in 1:nalt) {
      for (j in 1:nalt) {
        if (dom[i, j] == 1) { #P+(a,b)/P-(a,b) >= 1
          w_plus <- sum(ifelse(pm[i, ] > pm[j, ], w, 0)) #P+(a,b)
          w_minus <- sum(ifelse(pm[i, ] < pm[j, ], w, 0)) #P-(a,b)
          if (w_minus == 0 || w_plus / w_minus >= 1) s[i, j] <- 1
        }
      }
    }
    return(s)
  }

  #function for ranking
  # dom_s - simplified graph of strong preferences
  # dom_d - simplified graph of weak preferences
  # alt   - alternatives
  ranking <- function(dom_s, dom_w, alt) {
    #py code: dominance = np.clip(2*dominance_s + dominance_w, 0, 2)
    #since dom_s and _w should consist of 0/1:
    dominance <- 2 * dom_s + dom_w
    dominance[dominance == 3] <- 2
    colnames(dominance) <- alt
    rownames(dominance) <- alt
    y <- alt
    nalt <- length(alt)
    rank <- list()
    while (length(y) > 0) {
      d <- NULL
      u <- NULL
      b <- NULL
      a <- NULL
      for (j in 1:nalt) {
        check_d <- sum(ifelse((dominance[, j] == 2), 1, 0))
        #all options not strongly outranked by any other option form the set D.
        if (check_d == 0 && sum(dominance[, j]) != -nalt) {
          d[length(d) + 1] <- alt[j]
        }
      }
      idx <- match(d, alt)  #get index of alternatives in D set
      for (m in seq_along(d)) {
        for (n in seq_along(d)) {
          if (dominance[idx[m], idx[n]] == 1) {
            # Those options within D that are linked to each other by a weak
            # outranking relationship constitute the set U
            if (!(alt[idx[m]] %in% u)) u[length(u) + 1] <- alt[idx[m]]
            if (!(alt[idx[n]] %in% u)) u[length(u) + 1] <- alt[idx[n]]
          }
        }
      }
      idx <- match(u, alt)
      if (length(idx) > 0) {
        for (m in seq_along(u)) {
          check_b <- 0
          for (n in seq_along(u)) {
            if (dominance[idx[n], idx[m]] == 1) check_b <- check_b + 1
          }
          if (check_b == 0) {
            #The set B consists of all options from U not weakly outranked
            # by any other from within U.
            if (!(alt[idx[m]] %in% b)) b[length(b) + 1] <- alt[idx[m]]
          }
        }
      }
      # The single option or group of equally ranked options is defined
      # by the union of the sets (D - U) and B
      a <- d[!(d %in% u)]
      #TODO review function of ELECTRE II ranking - how it processes results 
      #next 2 lines look weird
      if (length(b) > 0) a <- append(a, b)
      if (length(a) > 0) rank <- append(rank, a)
      idx <- match(a, alt)
      y <- y[!(y %in% a)]
      if (length(idx) > 0) {
        for (j in seq_along(idx)) {
          dominance[, idx[j]] <- -1
          dominance[idx[j], ]  <- -1
        }
      }
    }
    return(rank)
  }

  cm <- ELECTRE_ConcordanceMatrix(PM, w)  #Concordance matrix
  dm <- ELECTRE_DiscordanceMatrix(PM) #discordance matrix

  #dominance matrix (preference matrix) - basic
  dominance_s <- matrix(data = 0, nrow = nalt, ncol = nalt)
  dominance_w <- matrix(data = 0, nrow = nalt, ncol = nalt)
  dominance_s <- ifelse((cm >= t(cm) & lower.tri(cm) &
                        ((cm >= c_plus & dm <= d_plus) |
                        (cm >= c_zero & dm <= d_minus))), 1, dominance_s)
  dominance_w <- ifelse((cm >= t(cm) & lower.tri(cm) &
                        ((cm >= c_zero & dm <= d_plus) | 
                        (cm >= c_minus & dm <= d_minus))), 1, dominance_w)
  diag(dominance_s) <- 0
  diag(dominance_w) <- 0

  #P+/P- ratio evaluation
  S_strong  <- preference_ratio(dominance_s, PM)
  S_weak    <- preference_ratio(dominance_w, PM)
  G_strong  <- simplify(graph_from_adjacency_matrix(S_strong)) #remove loops
  AM_strong <- Graph2AdjancancyMatrix(G_strong, alt)
  G_weak    <- simplify(graph_from_adjacency_matrix(S_weak))
  AM_weak   <- Graph2AdjancancyMatrix(G_weak, alt)
  #ranking
  rank_A   <- ranking(AM_strong, AM_weak, alt)  #1st total preorder V1
  # The second pre-order is obtained using the inverse ranking procedure.
  # The same basic algorithm as above is utilised, with the following
  # modifications.
  # invert the direction of the strong and weak outranking graphs GF, and Gf,
  # the ranks obtained from the inverted graphs using the direct procedure
  # is adjusted as follows:
  # 'inverse procedure' rank (r2(a»=1+(the number of ranks) (r'2(a)max) 
  # -'direct procedure' ranking (r'2(a))
  rank_D   <- rev(ranking(t(AM_strong), t(AM_weak), alt)) #2nd total preorder V2
  #final partial pre-order
  pref <- matrix(data = 0, nrow = nalt, ncol = nalt)
  incompar <- matrix(data = 0, nrow = nalt, ncol = nalt)
  for (i in 1:nalt) {
    r1_a <- match(alt[i], rank_A)
    r1_d <- match(alt[i], rank_D)
    for (j in 1:nalt) {
      if (i != j) {
        r2_a <- match(alt[j], rank_A)
        r2_d <- match(alt[j], rank_D)
        # if a is preferred to b in 2 complete pre-orders, then this will also
        # be the case in final pre-order
        if (r1_a > r2_a && r1_d > r2_d) pref[i, j] <- 1
        # if a has an equivalent ranking to b in the one of the complete
        # pre-orders, but is preferred to b in the other, then a precedes
        # b in the final pre-order
        if ((r1_a == r2_a && r1_d > r2_d) || (r1_a > r2_a && r1_d == r2_d)) {
          pref[i,j] <- 1
        }
        # if a is preferred to b in the first complete pre-order, but b is
        # preferred to a in the second, then the two options are incomparable
        # in the final pre-order.
        if ((r1_a > r2_a && r1_d < r2_d) || (r1_a < r2_a && r1_d > r2_d)) {
          incompar[i,j] <- 1
        }
      }
    }
  }
  colnames(pref) <- alt
  rownames(pref) <- alt
  colnames(incompar) <- alt
  rownames(incompar) <- alt
  graph <- plot.prefM(pref)
  # does not provide rank but a degree of preference, which can be directly
  # translated to rank
  rank_F <- rowSums(pref)
  names(rank_F) <- alt
  rank_F_sorted <- sort(rank_F, decreasing = TRUE) + 1

  rank_F2 <- rank_F

  # final order
  for (i in 1:nalt) {
    if (i == 1) {
      rank_F2[names(rank_F_sorted[1])] <- 1
      prevRank <- 1
    } else {
      if (prevVal > rank_F_sorted[i]) prevRank <- prevRank + 1
      rank_F2[names(rank_F_sorted[i])] <- prevRank
    }
    prevVal <- rank_F_sorted[i]
  }

  rank_F_sorted2 <- sort(rank_F2, decreasing = TRUE)

  if (VERBOSE) {
    print("Concordance Matrix")
    print(cm)
    print("Discordance Matrix")
    print(dm)
    print("Strong outranking relation")
    print(AM_strong)
    print("weak outranking relation")
    print(AM_weak)
    print("First total pre-order, V1")
    print(rank_A)
    print("Secont total pre-order, V2")
    print(rank_D)
    print("Preference matrix (final partial preorder)")
    print(pref)
    print("Incomparable options")
    print(incompar)
    print("Final partial preorder")
    print(rank_F2)
    print("Final partial preorder - sorted")
    print(rank_F_sorted2)
  }

  out <- list(
    PerformanceMatrix   = PM,
    ConcordanceMatrix   = t(cm),
    DiscordanceMatrix   = t(dm),
    StrongOutranking    = AM_strong,
    WeakOutranking      = AM_weak,
    firstTotalPreorder  = rank_A,
    secondTotalPreorder = rank_D,
    finalPreorderMatrix = pref,
    incomparableAlternatives = incompar,
    graphResult         = graph,
    finalPreorder       = rank_F2,
    finalPreorderSorted = rank_F_sorted2)
  return(out)
}
