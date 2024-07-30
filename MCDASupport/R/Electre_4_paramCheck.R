#' function for checking of the parameters fothe Electre 4 method
#'
#' @description
#' Checks parameters for ELECTRE 4 method (see \code{\link{Electre_4}}).
#'  It should stop processing of the inputs early when problem with
#'  parameters is detected by producing error message into the console.
#'
#' When no problem with the parameters is detected, it just ends without
#'  returning any value, allowing the calling function using it to continue.
#'
#' The function is also used by \code{\link{Electre_3_paramCheck}},
#'  \code{\link{electre1s}} and \code{\link{electre_TRI}}.
#'
#' @param pm performance of the alternatives in criteria. Alternatives are in
#'  rows, criteria in columns.
#' @param p vector of preference thresholds (each threshold is set separately
#'  for each criterium)
#' @param q vector of indifference thresholds
#' @param v vector of veto thresholds
#' @param minmaxcriteria vector of optimization direction (min/max) for the
#'  criteria
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords parameter validity
#' @keywords ELECTRE III
#' @keywords ELECTRE IV
#' @keywords ELECTRE 1s
#'
#' @examples
#' PM <- cbind(
#'   c(-14,129,-10,44,-14),
#'   c(90,100,50,90,100),
#'   c(40,0,10,5,20),
#'   c(40,0,10,5,20),
#'   c(100,0,100,20,40)
#' )
#' rownames(PM) <- c("Project1","Project2","Project3","Project4","Project5")
#' colnames(PM) <- c( "CR1","CR2","CR3","CR4","CR5")
#' minmaxcriteria <- "max"
#' Q <- c(25,16,0,12,10) #Indifference thresholds
#' P <- c(50,24,1,24,20) #Preference thresholds
#' V <- c(100,60,2,48,90) #Veto thresholds
#' Electre_4_paramCheck(PM, P, Q, V, minmaxcriteria)
Electre_4_paramCheck <- function(pm, p, q, v, minmaxcriteria = "max") {

  # with < 2 criteria or alternatives, there is no MCDA problem
  if (is.null(dim(pm))) stop("less than 2 criteria or 2 alternatives")
  if (!(is.matrix(pm) || (is.data.frame(pm)))) {
    stop("wrong performance matrix, should be a matrix or a data frame")
  }
  if (!is.numeric(unlist(pm))) {
    stop("Only numeric values in performance matrix expected")
  }
  ncri  <- ncol(pm)  #no. of criteria
  cri  <- colnames(pm)
  t <- c(p, q, v)
  if (!is.vector(t, mode = "numeric")) {
    stop("all thresholds must be a numeric vectors")
  }
  t <- c(length(p), length(v), length(q))
  if (!all(t == ncri)) {
    stop("number of elements in thresholds != number of criteria")
  }
  if (all(p == q)) {
    stop("Preference and indifference thresholds are defined same. 
          Revisit to distinguish them or use another method.")
  }

  # check consistency of the thresholds
  if (any(q < 0 | q > p | p >= v)) {
    print("Problem with consistency of thresholds:")
    print(paste0("  - Q < 0 for criteria:", cri[q < 0]))
    print(paste0("  - Q > P for criteria:", cri[q > p]))
    print(paste0("  - P >= V for criteria:", cri[p >= v]))
    stop("correct the thresholds please")
  }
}