# FuzzyTOPSIS function
#
# parameters
#   PM - performance matrix with n columns and no. criteria x no. of alternatives rows. I. e. with 3 criteria and 2
# alternatives the rows are: 1: 1.cri-1.alt., 2: 1.cri.-2.alt, 3: 2.cri-1.alt, 4: 2.cri-2.alt, ...
#   dictionaryPM - dictionary of linguistic variables for criteria (single matrix)
#   w - weights (matrix of weights - decision makers in columns and criteria in rows)
#   dictionaryW - dictionary for wights (single matrix|dataframe)
#   alt - vector with names of the alternatives
#   v - weight for strategy of majority of the criteria (0-1)
#   VERBOSE - TRUE if outputs should be dumped into console
FuzzyVIKOR <- function(PM, dictionaryPM, w, dictionaryW, alt, v = NULL, VERBOSE = FALSE){

  # function to check consistency of matrix againts dictionary of values
  #
  # parameters
  #   M - matrix to be checked
  #   dict - dictionary to check matrix against
  #   msg - message to guide user on what is being examined i.e. PM, weight matrix, etc.
  #   returns - TRUE if everything is OK, otherwise stops the computation (at first problem)
  fuzzyConsistency <- function(M, dict, msg = NULL){
    # check consistency of dictionary (needed to also check consistency of M later)
    if(ncol(dict) != 4) stop(paste('dictionary ', msg, ' needs to be defined as trapezoid fuzzy number (4 columns)'))
    if(!is.numeric(unlist(dict))) stop(paste('Only numeric values expected in ', msg))
    for(i in 1:nrow(dict)){ # check consistency of dictionary - numbers are expected to grow from left to right and from top to bottom
      for(j in 2:4){
        if(dict[i,j-1] > dict[i,j-1]) stop(paste('Problem with dictionaryPM inconsistency in row ', i))
      }
      if(i > 1){ # top to bottom check
        for(j in 1:4){
          if(dict[i,j] < dict[i-1,j]) stop(paste('Inconsistency in dict. ', msg, ' detected on [line, row]: [', i, ',', j, '], rows are expected to go from lowest value to highest.'))
        }
      }
    } # end check consistency of dictionary
    dictNames <- rownames(dict) #list of variables values used in dictionary
    # check consistency of matrix M
    if (!(is.matrix(M) || (is.data.frame(M)))) stop(paste('Problem with definition of matrix, ', msg, ': not matrix or dataframe.'))
    #check whether the M uses only fuzzy numbers from dictionary
    tM <- unlist(M)
    for(j in 1:length(tM)){ #check for consistency of preference function
      if(!(tM[j] %in% dictNames)) stop(paste('Dictionary ', msg, ' uses value not in the dictionary (', tM[j], ')'))
    }
    return(T) # every check passed, return TRUE
  } # end of fuzzyConsistency function

  # Convert the linguistic variables for the criteria weights or the ratings into fuzzy weights and fuzzy
  # decision matrix, respectively
  #
  # parameters
  #   dictionary - dictionary with the linguistic variables
  #   M - matrix with the criteria weights (or the ratings)
  #   n - number of the decision makers
  #   returns fuzzy weights/decision matrix
  agg_fuzzy_value <- function(dictionary, M, n){
    ncri <- nrow(M)
    f <- matrix(data = 0, nrow = ncri, ncol = 4)

    for(j in 1:ncri){
      mj <- which(rownames(dictionary) %in% M[j, ])
      if(length(mj) > 0){
        k0 <- min(dictionary[mj, 1])
        k1 <- sum(dictionary[mj, 2])
        k2 <- sum(dictionary[mj, 3])
        k3 <- max(dictionary[mj, 4])

        f[j,1] = round(k0, 3)
        f[j,2] = round(k1 / n, 3)
        f[j,3] = round(k2 / n, 3)
        f[j,4] = round(k3, 3)
      }
    }

    return(f)
  }

  # Function to deffuzify a trapezoidal fuzzy number into a crisp value
  #
  # parameters
  #   fN - trapezoidal fuzzy number matrix
  #   returns - crisp value for fuzzy number matrix
  defuzzM <- function(fN){
    t <- (-fN[,1] * fN[,2] + fN[,3] * fN[,4] + 1/3 * (fN[,4] - fN[,3])^2 - 1/3 * (fN[,2] - fN[,1])^2)/(-fN[,1] - fN[,2] + fN[,3] + fN[,4])
    return (t)
  }

  # Function to deffuzify a trapezoidal fuzzy number into a crisp value
  #
  # parameters
  #   fN - trapezoidal fuzzy number
  #   returns - crisp value for fuzzy number
  defuzz <- function(fN){
    t <- (-fN[1] * fN[2] + fN[3] * fN[4] + 1/3 * (fN[4] - fN[3])^2 - 1/3 * (fN[2] - fN[1])^2)/(-fN[1] - fN[2] + fN[3] + fN[4])
    return (t)
  }

  # Function determines best, worst values and difference between them for all criteria functions
  #
  # parameters
  #   car is the array with the performances (crisp alternative ratings)
  #   return matrix with max and min and difference values for the criteria
  best_worst_fij <- function(car){
    best <- apply(car, 2, max)
    worst <- apply(car, 2, min)
    difference <- best - worst

    f <- rbind(best, worst, difference)

    colnames(f) <- colnames(car)
    rownames(f) <- c('best', 'worst', 'difference')
    f <- t(f)

    return(f)
  }

  ## check validity of the objects manipulated by the current function
  n <- ncol(PM) #no. of decision makers
  criteria <- rownames(w)
  if(ncol(w) != ncol(PM)) stop('number of decission makers in w and PM is not same')
  ncri <- nrow(w)
  if(nrow(PM) %% ncri != 0) stop('no. of rows in PM must == to no. cri * no. alt.')
  nalt <- trunc(nrow(PM)/ncri)
  if(nalt != length(alt)) stop('No. of alternatives in alt param does not corespond to no. of alternatives in PM')
  if(!fuzzyConsistency(PM, dictionaryPM, 'PM')) stop('Unknown error when checking consistency of fuzzy numbers in PM')
  if(!fuzzyConsistency(w, dictionaryW, 'weights')) stop('Unknown error when checking consistency of fuzzy numbers in weights matrix')
  if(is.null(v)){
    v <- (ncri + 1)/(2*ncri)
  }else if(!is.numeric(v)){
    stop('weight of the strategy v must be numeric value (or NULL) for procedure to work')
  }else if(v < 0 || v > 1){
    stop('weight of the strategy v must be in interval 0-1 (or NULL).')
  }
  ## End of checking the validity of the "inputs"

  w2 <- agg_fuzzy_value(dictionaryW, w, n)
  rownames(w2) <- criteria
  f_rdm_all <- agg_fuzzy_value(dictionaryPM, PM, n)
  crisp_weights <- defuzzM(w2)
  names(crisp_weights) <- criteria
  crisp_alternative_ratings <- matrix(defuzzM(f_rdm_all), nrow=nalt)
  rownames(crisp_alternative_ratings) <- alt
  colnames(crisp_alternative_ratings) <- criteria
  bw <- best_worst_fij(crisp_alternative_ratings)
  sr_indexes <- VIKORIndexes(crisp_alternative_ratings, bw, crisp_weights, v)

  #print results
  if(VERBOSE){
    print('S metric')
    print(sr_indexes$S)
    print('R metric')
    print(sr_indexes$R)
    print('Q metric')
    print(sr_indexes$Q)
    print('Compromise solution')
    print(unlist(sr_indexes$compromiseSolution))
  }

  out <- list(
    S = sr_indexes$S,
    R = sr_indexes$S,
    Q = sr_indexes$Q,
    compromiseSolution = sr_indexes$compromiseSolution
  )
  return(out)
}
