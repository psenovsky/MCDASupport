# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   w  - weights
#   minmaxcriteria - vector of min/max for the criteria to se direction of the optimisation
#   concordance_threshold = 1
#   discordance_threshold = 0
#   VERBOSE = FALSE
# solution for the problem is dominance matrix which can be visualized into network graph
Electre_1 <- function(PM, w,
                      minmaxcriteria = 'max',
                      concordance_threshold = 1,
                      discordance_threshold = 0,
                      VERBOSE = FALSE){

  ## check validity of the objects manipulated by the current function
  # if there are less than 2 criteria or 2 alternatives, there is no MCDA problem
  if (is.null(dim(PM))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(PM) || (is.data.frame(PM)))) stop('wrong performanceMatrix, should be a matrix or a data frame')
  if (!is.numeric(unlist(PM))) stop('Only numeric values in performance matrix expected')
  if (!(is.vector(w))) stop('criteriaWeights should be a vector')
  if (!is.numeric(w)) stop('criteriaWeights should be a numeric vector')
  if (ncol(PM)!=length(w)) stop('length of criteriaWeights should be checked')
  PM <- util_pm_minmax(PM, minmaxcriteria) #validate minmax and invert scales if necessary
  if(!is.numeric(concordance_threshold) || concordance_threshold < 0 || concordance_threshold > 1) stop('Concordance threshold out of bounds')
  if(!is.numeric(discordance_threshold) || discordance_threshold < 0 || discordance_threshold > 1) stop('Discordance threshold out of bounds')
  ## End of checking the validity of the "inputs"

  nalt <- nrow(PM)  #no. of alternatives
  ncri <- ncol(PM)  #no. of criteria
  alt  <- rownames(PM)
  cm <- ELECTRE_ConcordanceMatrix(PM, w)  #Concordance matrix
  dm <- ELECTRE_DiscordanceMatrix(PM) #discordance matrix

  #dominance matrix (preference matrix)
  dom <- matrix(data = 0, nrow = nalt, ncol = nalt)
  #aSb iff C(a,b) >= c_thres and D(a,b) <= d_thres
  dom <- ifelse((cm >= concordance_threshold & dm <= discordance_threshold), 1, dom)
  diag(dom) <- 0
  rownames(dom) <- alt
  colnames(dom) <- alt

  # establish kernel of the decision
  t <- ELECTRE1_Kernel(dom)
  if(is.null(t$kernel)){
    kernel <- 'empty kernel'
  }else{
    kernel <- t$kernel
  }
  dominated <- t$dominated
  graph <- t$graph

  # visualize results to the screen
  if(VERBOSE){
    print('Concordance Matrix')
    print(t(cm))
    print('Discordance Matrix')
    print(t(dm))
    print('Preference Matrix')
    print(dom)
    print('Kernel')
    print(kernel)
    print('Dominated')
    print(dominated)
  }

  #t(cm) - pyDecisions provide diagonaly mirrored matrixes (also for dm)
  out <- list(
    PerformanceMatrix = PM,
    ConcordanceMatrix = t(cm),
    DiscordanceMatrix = t(dm),
    PreferenceMatrix = dom,
    GraphResult = graph,
    Kernel = kernel,
    Dominated = dominated)
  return(out)
}
