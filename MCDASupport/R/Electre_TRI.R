# ELECTRE TRI method
#
#   PM             - performance matrix
#   profiles       - Matrix containing, in each row, the lower profiles of
#                    the categories.
# The columns are named according to the criteria, and the rows are named
# according to the categories.
# The index of the row in the matrix corresponds to the rank of the category.
#   profile_names  - Vector containing profiles' names
#   w              - weights of the criteria
#   Q              - indifference threshold
#   P              - preference thresholds
#   V              - veto thresholds
#   minmaxcriteria - vector with identification of direction of the criteria
#   lambda         - cut-off criterion
#   VERBOSE        - should the result be printed to console, FALSE = only
#                    return the results (without printing them first)
# note pyDecisions sets lambda = 1
Electre_TRI <- function(PM, profiles, profiles_names,
                        w, Q, P, V, minmaxcriteria = "max", lambda=0.75,
                        VERBOSE = FALSE) {

  ## input data consistency check
  Electre_4_paramCheck(pm = PM, q = Q, p = P, v = V,
                       minmaxcriteria = minmaxcriteria)
  PM   <- util_pm_minmax(PM, minmaxcriteria) #validate minmax and invert scales if necessary
  ncri <- ncol(PM)  #no. of criteria
  nalt <- nrow(PM)  #no. of alternatives
  alt  <- rownames(PM)
  if (!(is.matrix(profiles) || is.data.frame(profiles))) {
    stop("wrong profiles, should be a matrix or a data frame")
  }
  if (!is.vector(profiles_names)) stop("profiles_names should be a vector")
  npr <- length(profiles_names) # number of profiles
  if (npr != nrow(profiles)) {
    stop("Number of stated profile names must correspond to number 
    of rows in profile matrix")
  }
  if (!is.vector(w, mode = "numeric")) {
    stop("criteria weights should be a vector")
  }
  if (ncri != length(w)) stop("length of criteria weights should be checked")
  if (!is.numeric(lambda)) stop("if lambda is set, it must be numeric value")
  if (lambda < 0.5 || lambda > 1) {
    stop("Lambda value must be in interval <0.5;1>")
  }
  ## End of checking the validity of the "inputs"

  cj     <- list() # list of partial concordance indexes for profiles cj(a,bh)
  cj_inv <- list() # inverse list of partial concordance
  #                  indexes for profiles: cj(bh,a)
  dj     <- list() # list of discordance indexes: dj(a,bh)
  dj_inv <- list() # inverse list of discordance indexes: dj(bh,a)
  # template zero matrix of n x m, n =
  # no. of alternatives, m = number of profiles
  d  <- matrix(data = 0, nrow = nalt, ncol = npr)
  colnames(d) <- profiles_names
  rownames(d) <- alt

  for (j in 1:ncri) {
    cj_ab <- d
    cj_ba <- d
    dj_ab <- d
    dj_ba <- d
    diff_pj <- P[j] - Q[j]
    diff_vp <- V[j] - P[j]
    for (i in 1:nalt) {
      for (k in 1:npr) {
        diff_ab <- profiles[k, j] - PM[i, j]
        diff_ba <- PM[i, j] - profiles[k, j]
        cj_ab[i, k] <- ifelse(diff_ab >= P[j], 0,
                              ifelse(diff_ab <= Q[j], 1,
                                     (P[j] + PM[i, j] - profiles[k, j]) / diff_pj))
        cj_ba[i, k] <- ifelse(diff_ba >= P[j], 0,
                              ifelse(diff_ba <= Q[j], 1,
                                     (P[j] + profiles[k, j] - PM[i, j]) / diff_pj))
        dj_ab[i, k] <- min(1, max(0, (diff_ab - P[j]) / diff_vp))
        dj_ba[i, k] <- min(1, max(0, (diff_ba - P[j]) / diff_vp))
      }
    }
    cj[[j]] <- cj_ab
    cj_inv[[j]] <- cj_ba
    dj[[j]] <- dj_ab
    dj_inv[[j]] <- dj_ba
  }

  # compute the overall concordance index c_abh
  # c(a,bh) = sum for all j (kj * cj(a,bh) / sum for all j kj
  oci <- d     #overall concordance index: c(a,bh)
  oci_inv <- d #inverse overall concordance index: c(bh,a)
  rownames(oci) <- alt
  colnames(oci) <- profiles_names
  rownames(oci_inv) <- alt
  colnames(oci_inv) <- profiles_names
  for (j in 1:ncri) {
    oci <- oci + w[j] * cj[[j]]
    oci_inv <- oci_inv + w[j] * cj_inv[[j]]
  }
  oci <- oci / sum(w)
  oci_inv <- oci_inv / sum(w)

  # partial credibility index
  # cr(a,b) = c(a,bh) * PRODUCT of all j in Fr (1 - dj(a,bh))/(1 - c(a,bh))
  # where Fr are all criteria for which dj(a,bh) > c(a,bh)
  cr <- oci         # partial credibility index: cr(a,bh)
  cr_inv <- oci_inv # partial inverse credibility index: cr(bh, a)
  for (j in 1:ncri) {
    dj_ab <- dj[[j]]
    dj_ba <- dj_inv[[j]]
    for (i in 1:nalt) {
      for (k in 1:npr) {
        if (dj_ab[i, k] > oci[i, k]) {
          cr[i, k] <- cr[i, k] * (1 - dj_ab[i, k]) / (1 - oci[i, k])
        }
        if (dj_ba[i, k] > oci_inv[i, k]) {
          cr_inv[i, k] <- cr_inv[i, k] * (1 - dj_ba[i, k]) / (1 - oci_inv[i, k])
        }
      }
    }
  }

  # preference relations
  pref <- matrix(0, nalt, npr)
  rownames(pref) <- alt
  colnames(pref) <- profiles_names
  for (i in 1:nalt) {
    for (k in 1:npr) {
      if (cr[i, k] >= lambda && cr_inv[i, k] >= lambda) {
        # cr(a,bh) >= lambda and cr(bh,a) >= lambda => aSbh and bhSa => a is
        # indifferent to bh
        pref[i, k] <- "I" # aIbh
      } else if (cr[i, k] >= lambda && cr_inv[i, k] < lambda) {
        # cr(a,bh) >= lambda and cr(bh,a) < lambda => aSbh and not bhSa
        # => a > bh, i.e. a is preferred to bh (weakly or strongly)
        pref[i, k] <- ">" # aPbh
      } else if (cr[i, k] < lambda && cr_inv[i, k] >= lambda) {
        # cr(a,bh) < lambda and cr(bh,a) >= lambda => not aSbh and bhSa
        # => bh > a, i.e. bh is preferred to a (weakly or strongly)
        pref[i, k] <- "<" # bhPa
      } else if (cr[i, k] < lambda && cr_inv[i, k] < lambda) {
        # cr(a,bh) < lambda and cr(bh,a) < lambda => not aSbh and not
        # bhSa => aRbh, i.e. a is incomparable to bh
        pref[i, k] <- "R" # aRbh
      }
    }
  }

  # pessimistic procedure
  # a) compare alternatives ("a") successively to "b(i)" , for i=p,p-1, ..., 0,
  # b) let "b(h)" = the first profile such that "a" outranks "b(h).",
  # c) assign "a" to the category C(h+1).
  # The direction of the ranking obtained from the pessimistic procedure is
  # from best to worst.
  pessimistic <- rep(0, times = nalt)
  names(pessimistic) <- alt
  for (i in 1:nalt) {
    for (k in npr:1) {
      if (pref[i, k] == ">") {
        pessimistic[i] <- k + 1
        break
      }
    }
  }
  pesimisticUnsorted <- pessimistic
  pesimistic <- sort(pessimistic)

  # optimistic procedure
  # a) compare a successively to bi, for i = 1, 2, ....,p,
  # b) bh being the first profile such that bh > a,
  # c) assign a to category C(h)
  # direction from worst to best
  optimistic <- rep(0, times = nalt)
  names(optimistic) <- alt
  for (i in 1:nalt) {
    for (k in 1:npr) {
      if (pref[i, k] == "<") {
        optimistic[i] <- k
        break
      }
    }
  }
  optimisticUnsorted <- optimistic
  optimistic <- sort(optimistic)

  # aggregation procedure
  # - No option can be indifferent to more than one reference option.
  # - Each option must be assigned to one reference category only
  #   (uniqueness/unicity).
  # - The assignment of any option to its allotted category is not dependent
  #   on the assignment of any of the other options (independence).
  # - The procedure for assigning options to categories must be entirely
  #   consistent with the design of the reference options themselves
  #   (conformity).
  # - When two options have the same outranking relationship with a given
  #   reference option, they must be assigned to the same category
  #   (homogeneity).
  # - If option a' outranks a, then a' must be assigned to a category at least
  #   as good as the one to which a is assigned (monotonicity).
  # - The grouping together of two neighbouring categories must not cause
  #   the alteration of options to categories not affected by the alteration
  #   (stability)

  # feedback of computation artifacts to screen
  if (VERBOSE) {
    print("elementary concordance index cj(a,bh)")
    print(cj)
    print("elementary (inverse) concordance index cj(bh,a)")
    print(cj_inv)
    print("overal concordance index c(a,bh)")
    print(oci)
    print("overal (inverse) concordance index c(bh,a)")
    print(oci_inv)
    print("elementary discordance index dj(a,bh)")
    print(dj)
    print("elementary (inverse) discordance index dj(bh,a)")
    print(dj_inv)
    print("partial credibility index cr(a,bh)")
    print(cr)
    print("partial (inverse) credibility index cr(bh,a)")
    print(cr_inv)
    print("Preference relation")
    print(pref)
    print("pesimistic procedure")
    print(pesimistic)
    print("pesimistic unsorted")
    print(pesimisticUnsorted)
    print("optimmistic procedure")
    print(optimistic)
    print("optimistic unsorted")
    print(optimisticUnsorted)
  }

  out <- list(
    performanceMatrix              = PM,
    partialConcordanceIndex        = cj,
    partialConcordanceIndexInverse = cj_inv,
    overallConcordanceIndex        = oci,
    overallConcordanceIndexInverse = oci_inv,
    discordanceIndex               = dj,
    discordanceIndexInverse        = dj_inv,
    credibilityIndex               = cr,
    credibilityIndexInverse        = cr_inv,
    preferenceRelation             = pref,
    pesimistic                     = pesimistic,
    pesimisticUnsorted             = pesimisticUnsorted,
    optimistic                     = optimistic,
    optimisticUnsorted             = optimisticUnsorted
  )
  return(out)
}
