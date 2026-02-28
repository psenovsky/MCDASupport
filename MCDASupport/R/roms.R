#' Rank Ordering Methods (ROMs)
#'
#' @description
#' Group of methods for deriving weights based on known rank of the criteria.
#'
#' The process is generally same for all the methods. First we asign ranks to
#'  the criteria, then we compute the wweights using method's appropriate
#'  equation.
#'
#' Currently following methods are supported (bold are constant names to use
#'  as parameter of the function):
#'
#' \itemize{
#'   \item \bold{Rank Sum} Method
#'   \item \bold{Rank Exponent} Method
#'   \item \bold{Rank Reciprocal} Method
#' }
#'
#' \bold{Rank Sum Method}
#'
#' \mjsdeqn{w_j = \frac{n - r_j + 1}{\sum_{k=1}^n (n - r_k + 1)}}
#' 
#' \bold{Rank Exponent Method}
#'
#' \mjsdeqn{w_j = \frac{(n - r_j + 1)^p}{\sum_{k=1}^n (n - r_k + 1)^p}}
#' 
#' \bold{Rank Reciprocal Method}
#' 
#' \mjsdeqn{w_j = \frac{1 / r_j}{\sum_{k=1}^n (1 / r_k)}}
#' 
#' @param ranks vector of ranks for the criteria
#'
#' @param p rank exponent, must be p >= 0, only used in Rank Exponent Method
#'
#' @param method constant identifying method in ROMs, which should be applied.
#'  Currently: Rank Sum, Rank Exponent, Rank Reciprocal are supported.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#' 
#' @examples
#' r <- c(3, 4, 5, 1, 6, 2)
#' result <- roms(r, method = "Rank Sum")
roms <- function(ranks, p = 1, method = "Rank Sum") {
  # validate inputs
  if (!is.vector(ranks, mode = "numeric")) {
    stop("ranks paramerer expected to be numeric vector.")
  }
  nmethods <- c(
    "Rank Sum",
    "Rank Exponent",
    "Rank Reciprocal"
  )
  validation$validate_invalid_val(method, nmethods, "rank ordering method")
  if (method == "Rank Expoenent") {
    if (!is.numeric(p) || p < 0) {
      stop("p exponent must be number >= 0.")
    }
  }
  # end of validation

  # Rank Sum Method
  Rank_Sum <- function(ranks) {
    n <- length(ranks)
    wj <- (n - ranks + 1) / sum(n - ranks + 1)
    return(wj)
  }

  # Rank Exponent Method
  Rank_Exponent <- function(ranks, p) {
    n <- length(ranks)
    wj <- ((n - ranks + 1)^p) / sum((n - ranks + 1)^p)
    return(wj)
  }

  # Rank Reciprocal Method
  Rank_Reciprocal <- function(ranks) {
    n <- length(ranks)
    wj <- (1 / ranks) / sum(1 / ranks)
    return(wj) 
  }

  # perform normalization
  result <- switch(
    method,
    "Rank Sum" = Rank_Sum(ranks),
    "Rank Exponent" = Rank_Exponent(ranks, p),
    "Rank Reciprocal" = Rank_Reciprocal(ranks)
  )
  return(result)
}
