#' Analytic Hierarchy process
#' 
#' @description
#' AHP is one of most widely used methods for MCDA and weight derivation based
#'  on stated preferences. 
#' 
#' This function implements only single preference matrix computation, so it
#'  does not implement hierarchy at all. 
#' 
#' The preferences are stated in interval 1 (equal) to 9 (extremely more inportrant)
#'  and 1/9 (extremely less important) to 1.
#' 
#' One of interesting properties of AHP is ability to evaluate randomnes of
#'  stated preferences by comparing its consistency index (CI) to average random
#'  consistency index (RCI). 
#' 
#' The weights are derived using geometric mean method.
#' 
#' \mjsdeqn{CI = \frac{\lambda_{mmax} - n}{n - 1}}
#' 
#' Lambda max is largest eigenvalue of the preference matrix, n is number of criteria
#' 
#' The ratio (CR - Consistency Ration) of CI/RCI should be under 0.1 to reject
#'  hyphothesis, that the stated preferences has been stated randomly. Function
#'  allows to perform this evaluation maximally for 15 criteria in single matrix.
#'  In case you need more, use hierarchy.
#' 
#' @param pm preference matrix n:n, where n is number of criteria
#' 
#' @return list of weights vector (w) and consistency ratio of the evaluation
#' 
#' @references
#' Řehák, D., Šenovský, P. Preference risk assessment of electric power critical
#'  infrastructure. In Chemical Engineering Transactions, Vol. 36, pp. 469-474,
#'  DOI 10.3303/CET1436079, ISSN: 1974-9791
#' 
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#' 
#' @examples
#' pm <- rbind(
#'  c(1, 5, 7, 5, 9, 9),
#'  c(0.2, 1, 5, 5, 7, 7),
#'  c(0.1429, 0.2, 1, 0.3333, 3, 3),
#'  c(0.2, 0.2, 3, 1, 5, 5),
#'  c(0.1111, 0.1429, 0.3333, 0.2, 1, 0.3333),
#'  c(0.1111, 0.1429, 0.3333, 0.2, 3, 1)
#' )
#' rownames(pm) <- colnames(pm) <- c("citizens",	"CI (elements)",	
#'   "public infrastrucutre (trasport)", "PI (technical infr.)",
#'   "PI (civil amenities)", "environment")
#' t <- ahp(pm)
#' t$w
#' t$CR
ahp <- function(pm) {
  # validate
  validation$validate_pm(pm)
  validation$validate_pm_rows_columns_same(pm)
  validation$validation_vector_in_interval(c(pm), 0, 9, "preference matrix elements")
  ncri <- ncol(pm)
  if (ncri < 3 || ncri > 15) stop("The consistency ratio is computable for matrixes of size 3 - 15.")
  diag(pm) <- 1
  # end of validation

  # constants
  RCI <- c(NULL,	NULL, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49, 1.51, 1.48, 1.56, 1.57, 1.59)
  
  # AHP procedure
  multiplication <- rep(1, times = ncri)
  weights <- rep(0, times = ncri)
  nroot <- rep(0, times = ncri)
  for (i in 1:ncri) multiplication <- multiplication * pm[, i]
  for (i in 1:ncri) nroot[i] <- multiplication[i]^0.17
  sum_nroot <- sum(nroot)
  for (i in 1:ncri) weights[i] <- nroot[i] / sum_nroot
  t <- weights
  for (i in 1:ncri) {
    t[i] <- sum(weights * pm[i, ])
  }
  lambda_max <- t / weights
  t2 <- sum(lambda_max) / ncri
  CI <- (t2 - ncri) / (ncri - 1)
  CR <- CI/RCI[ncri]
  results <- list(
    w = weights,
    CR = CR
  )
  return(results)
}