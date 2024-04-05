# PROMETHEE I function
#
# parameters
#   PM - performance matrix
#   preferenceFunction - vector of preference functions types to derive preference when comparing PM
#   w - weights
#   minmax - min/max value or vector of mixed values for criteria orientation
#   indifferenceTreshold list of indifference thresholds
#   prefferenceThreshold list of prefference thresholds
#   intermediateThreshold lish of intermediate thresholds for Gaussian function type
PROMETHEE_I <- function(PM, preferenceFunction, w, minmax = 'max',
                        indifferenceTreshold = NULL, prefferenceThreshold = NULL,
                      intermediateThreshold = NULL, VERBOSE = FALSE){

  #params consistency check centralized in generalized PROMETHEE function
  nalt <- nrow(PM)  #no. of alternatives
  alt  <- rownames(PM) #list of alternatives
  #here is only minmax evaluation
  PM <- util_pm_minmax(PM, minmax) #validate minmax and invert scales if neccessary
  #end of parameter consistency check

  flow <- PROMETHEE(PM, preferenceFunction, w, indifferenceTreshold, prefferenceThreshold, intermediateThreshold)
  pf <- flow$positiveFlow
  nf <- flow$negativeFlow

  #establish preference system
  pref <- matrix(data = 0, nrow = nalt, ncol = nalt)
  rownames(pref) <- alt
  colnames(pref) <- alt
  k <- 2
  for(i in 1:nalt){
    for(j in k:nalt){
      pf_i_greater_pf_j <- pf[i] > pf[j]
      pf_i_eq_pf_j <- pf[i] == pf[j]
      nf_i_less_nf_j <- nf[i] < nf[j]
      nf_i_eq_nf_j <- nf[i] == nf[j]

      if((pf_i_greater_pf_j && (nf_i_less_nf_j || nf_i_eq_nf_j)) || (pf_i_eq_pf_j && nf_i_less_nf_j)) { #aPb (a preffered to b)
        pref[i,j] <- 'P+'
        pref[j,i] <- 'P-'
      }else if(pf_i_eq_pf_j && nf_i_eq_nf_j){ #aIb (a indifferent to b)
        pref[i,j] <- pref[j,i] <- 'I'
      }else if((pf_i_greater_pf_j && !nf_i_less_nf_j) || (!pf_i_greater_pf_j && nf_i_less_nf_j)){ #aRb (a incomparable to b)
        pref[i,j] <- pref[j,i] <- 'R'
      }else{ # bPa (b is prefered to a)
        pref[i,j] <- 'P-'
        pref[j,i] <- 'P+'
      }
    }
    if(k < nalt) k <- k + 1
  }
  diag(pref) <- 'I'

  if(VERBOSE){
    print('Positive flow')
    print(pf)
    print('Negative flow')
    print(nf)
    print('Preference degree')
    print(flow$preferenceDegree)
    print('Preference degree unweighted')
    print(flow$preferenceDegreeUnw)
    print('Preference matrix')
    print(pref)
  }

  out <- list(
    positiveFlow = pf,
    negativeFlow = nf,
    preferenceDegree = flow$preferenceDegree,
    preferenceDegreeUnw = flow$preferenceDegreeUnw,
    pairweiseComparison = flow$pairweiseComparison,
    preferenceMatrix = pref
  )
  return(out)
}
