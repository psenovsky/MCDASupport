#' Electre_3_paramCheck: function for checking of the parameters for the
#'  Electre 3 method
#'
#' @description
#' Checks parameters for ELECTRE 3 method used by \code{\link{electre3}} R6
#'  class. It should stop processing of the inputs early when problem with
#'  parameters is detected by producing error message into the console.
#'
#' When no problem with the parameters is detected, it just ends without
#'  returning any value, allowing the calling function using it to continue.
#'
#' @param pm performance of the alternatives in criteria. Alternatives are in
#'  rows, criteria in columns.
#' @param w weights of the criteria in decision problem
#' @param p vector of preference thresholds (each threshold is set separately
#'  for each criterium)
#' @param q vector of indiference thresholds
#' @param v vector of veto thresholds
#' @param minmaxcriterie vector of optimization direction (min/max) for the
#'  criteria
#' @param alpha first discrimination threshold (0.3 by default)
#' @param beta second discrimination threshold (0.15 by default)
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords parameter validity
#' @keywords ELECTRE III
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
#' minmaxcriteria <- 'max'
#' Q <- c(25,16,0,12,10) #Indifference thresholds
#' P <- c(50,24,1,24,20) #Preference thresholds
#' V <- c(100,60,2,48,90) #Veto thresholds
#' w <- c(1,1,1,1,1) #weights
#' Electre_3_paramCheck(PM, w, P, Q, V, minmaxcriteria)
Electre_3_paramCheck <- function(pm, w, p, q, v, minmaxcriteria = "max",
                                 alpha = 0.3, beta = 0.15) {

  # use shared checks for the method (Electre 4 uses subset of Electre 3
  # parameters)
  # only values checked - computationally the methods are different
  Electre_4_paramCheck(PM = pm, P = p, Q = q, V = v,
                       minmaxcriteria = minmaxcriteria)

  # specific checks for Electre 3
  if (!is.vector(w, mode = "numeric")) {
    stop("criteria weights should be a vector")
  }
  ncri <- ncol(pm)  #no. of criteria
  if (ncri != length(w)) {
    stop("number of criteria weights must be same as number of criteria")
  }
  if (!is.numeric(alpha)) {
    stop("alpha discrimination threshold should be a number")
  }
  if (!is.numeric(beta)) {
    stop("beta discrimination threshold should be a number")
  }
  if (alpha < beta) stop("alpha shoud be > beta (discrimination threshold)")
}
