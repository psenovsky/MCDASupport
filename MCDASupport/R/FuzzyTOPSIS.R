# FuzzyTOPSIS function
#
# parameters
#   PM - performance matrix with n columns and no. criteria x no. of alternatives rows. I. e. with 3 criteria and 2
# alternatives the rows are: 1: 1.cri-1.alt., 2: 1.cri.-2.alt, 3: 2.cri-1.alt, 4: 2.cri-2.alt, ...
#   dictionaryPM - dictionary of linguistic variables for criteria (single matrix)
#   w - weights (matrix of weights - decision makers in columns and criteria in rows)
#   dictionaryW - dictionary for wights (single matrix|dataframe)
#   alt - vector with names of the alternatives
#   VERBOSE - if TRUE dump outputs to console
FuzzyTOPSIS <- function(PM, dictionaryPM, w, dictionaryW, alt, VERBOSE = FALSE){

  # function to check consistency of matrix againts dictionary of values
  #
  # parameters
  #   M - matrix to be checked
  #   dict - dictionary to check matrix against
  #   msg - message to guide user on what is being examined i.e. PM, weight matrix, etc.
  #   returns - TRUE if everything is OK, otherwise stops the computation (at first problem)
  fuzzyConsistency <- function(M, dict, msg = NULL){
    # check consistency of dictionary (needed to also check consistency of M later)
    if(ncol(dict) != 3) stop(paste('dictionary ', msg, ' needs to be defined as stringle fuzzy number (3 columns)'))
    if(!is.numeric(unlist(dict))) stop(paste('Only numeric values expected in ', msg))
    for(i in 1:nrow(dict)){
      # check consistency of dictionary - numbers are expected to grow from left to right and from top to bottom
      if(dict[i,2] < dict[i,1] || dict[i,2] > dict[i,3]) stop(paste('Problem with dictionaryPM inconsistency in row ', i))
      if(i > 1){ # top to bottom check
        for(j in 1:3){
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

  # Convert the linguistic variables for the criteria weights or the ratings into fuzzy weights and fuzzy decision
  # matrix, respectively
  #
  # parameters
  #   dict - dictionary with the linguistic variables for criteria weights (or the linguistic variables for the ratings)
  #   M - matrix with criteria weights (or the ratings)
  #   n - number of the decision makers
  #   returns - fuzzy decision matrix or the fuzzy weights of the criteria
  cal <- function(dict, M, n){
    f <- list()
    for(i in 1:nrow(M)){
      c <- list()
      for(z in 1:3){
        x <- 0
        for(j in 1:n){
          x <- x + dict[M[i,j],z]
        }
        c[[length(c)+1]] <- round(x/n, 3)
      }
      f[[length(f)+1]] <- c
    }
    t <- do.call(rbind.data.frame, f)
    colnames(t) <- c('fn1', 'fn2', 'fn3') #def. fuzzy number
    rownames(t) <- rownames(M) #works fine for w, but line does nothing for PM
    return(t)
  }

  #calculate fuzzy normalized decision matrix
  #
  # parameters
  #   FDM - the fuzzy decision matrix
  #   n - the number of criteria
  #   m - the number of the alternatives
  #   returns fuzzy normalized decision matrix
  fndm <- function(FDM, n, m){
    x <- max(FDM[,2:3])
    f <- round(FDM/x, 3)
    colnames(f) <- c('fn1', 'fn2', 'fn3') #triangular fuzzy number
    return(f)
  }

  # Calculate the fuzzy weighted normalized decision matrix
  #
  # parameters
  #   FNDM - the fuzzy normalized decision matrix
  #   w - weights
  #   n - number of criteria
  #   m - number of alternatives
  #   returns fuzzy weighted normalized decision matrix
  weighted_fndm <- function(FNDM, w, n, m){
    w2 <- NULL
    for(i in 1:n) {
      t <- matrix(rep(w[i,], times = m), ncol = 3, byrow = T)
      w2 <- rbind(w2, t)
    }
    w2 <- as.numeric(w2)
    f <- FNDM * w2
    colnames(f) <- c('fn1', 'fn2', 'fn3')
    return(f)
  }

  # Calculate the distance between two fuzzy triangular numbers
  #
  # parameters
  #   a, b - bare fuzzy triangular number
  #   returns distance between a and b
  distance <- function(a, b){
    t <- sqrt(1/3 * ((a[1] - b[1])^2 + (a[2] - b[2])^2 + (a[3] - b[3])^2))
    return(t)
  }

  # Determine the fuzzy positive ideal solution (FPIS)
  #
  # parameters
  #   FWNDM is the fuzzy weighted normalized decision matrix
  #   n is the number of criteria
  #   m is the number of the alternatives
  #   returns ideal solution of each criterion
  func_dist_fpis <- function(FWNDM, n, m){
    fpis <- matrix(data = 1, nrow = 3, ncol = 1)
    dist_pis <- rep(0, times = m)
    #  p <- 0
    for(i in 1:m){ # iterate alternatives
      for(j in 1:n){ # iterate criteria
        if(j == 1) {
          t <- distance(FWNDM[i,], fpis)
        }else{
          t <- distance(FWNDM[i + (j-1)*m,], fpis)
        }
        dist_pis[i] <- dist_pis[i] + t
      }
    }
    return(as.numeric(dist_pis))
  }

  # Determine the fuzzy negative ideal solution (FNIS)
  #
  # parameters
  #   FWNDM is the fuzzy weighted normalized decision matrix
  #   n is the number of criteria
  #   m is the number of the alternatives
  #   returns anti-ideal solution of each criterion
  func_dist_fnis <- function(FWNDM, n, m){
    fnis <- matrix(data = 0, nrow = 3, ncol = 1)
    dist_nis <- rep(0, times = m)
    for(i in 1:m){ # iterate alternatives
      for(j in 1:n){ # iterate criteria
        if(j == 1) {
          t <- distance(FWNDM[i,], fnis)
        }else{
          t <- distance(FWNDM[i + (j-1)*m,], fnis)
        }
        dist_nis[i] = dist_nis[i] + t
      }
    }
    return(as.numeric(dist_nis))
  }

  ## check validity of the objects manipulated by the current function
  n <- ncol(PM) #no. of decision makers
  if(ncol(w) != ncol(PM)) stop('number of decission makers in w and PM is not same')
  ncri <- nrow(w)
  if(nrow(PM) %% ncri != 0) stop('no. of rows in PM must == to no. cri * no. alt.')
  nalt <- trunc(nrow(PM)/ncri)
  if(nalt != length(alt)) stop('No. of alternatives in alt param does not corespond to no. of alternatives in PM')
  if(!fuzzyConsistency(PM, dictionaryPM, 'PM')) stop('Unknown error when checking consistency of fuzzy numbers in PM')
  if(!fuzzyConsistency(w, dictionaryW, 'weights')) stop('Unknown error when checking consistency of fuzzy numbers in weights matrix')
  ## End of checking the validity of the "inputs"

  # Fuzzy TOPSIS procedure
  fuzzy_weights <- cal(dictionaryW, w, n)
  fuzzy_decision_matrix <- cal(dictionaryPM, PM, n)
  fuzzy_norm_decision_matrix <- fndm(fuzzy_decision_matrix, ncri, nalt)
  weighted_fuzzy_norm_decision_matrix <- weighted_fndm(fuzzy_norm_decision_matrix, fuzzy_weights, ncri, nalt)
  a_plus <- func_dist_fpis(weighted_fuzzy_norm_decision_matrix, ncri, nalt)
  names(a_plus) <- alt
  a_minus <- func_dist_fnis(weighted_fuzzy_norm_decision_matrix, ncri, nalt)
  names(a_minus) <- alt
  CC <- a_minus / (a_plus + a_minus)
  names(CC) <- alt

  if(VERBOSE){
    print('fuzzy weights')
    print(fuzzy_weights)
    print('fuzzy decision matrix')
    print(fuzzy_decision_matrix)
    print('weighted normalized fuzzy decision matrix')
    print(weighted_fuzzy_norm_decision_matrix)
    print('a+')
    print(a_plus)
    print('a-')
    print(a_minus)
    print('Alternatives sorted by closeness coeficient')
    print(sort(CC, decreasing = T))
  }

  out <- list(
    fuzzy_weights = fuzzy_weights,
    fuzzy_decision_matrix = fuzzy_decision_matrix,
    fuzzy_norm_decision_matrix = fuzzy_norm_decision_matrix,
    weighted_fuzzy_norm_decision_matrix = weighted_fuzzy_norm_decision_matrix,
    a_plus = a_plus,
    a_minus = a_minus,
    CC = CC
  )
  return(out)
}
