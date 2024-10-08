#' Entropy Weight Methed (EWM)
#'
#' @description
#' Allows us to establish weight system just based on evaluation of the entropy
#'  in input data of the performance matrix. Weights in this case are not
#'  established using input from decision makers.
#'
#' There are some oppinions in literature claiming that this approach to
#'  weights assessment is more objective because of that, but that is not the
#'  case. It is just another approach which can be valid (or not based on
#'  context of the decision).
#'
#' The computation starts with normalization of the performance matrix. The
#'  function uses only normalization for maximalization direction as the
#'  performace matrix scales are first transformed so that all criteria are
#'  maximalized using \code{\link{util_pm_minmax}} function.
#'
#' \mjsdeqn{EM_{ij} = \frac{PM_{ij}}{max(PM_{ij})_j}}
#'
#' Then we compute probability of criteria to occur.
#'
#' \mjsdeqn{p_{ij} = \frac{EM_{ij}}{\sum_{i=1}^n EM_{ij}}}
#'
#' Where j = criteria, i = alternatives, n = number of alternatives.
#'
#' Then we compute entropy E:
#'
#' \mjsdeqn{E_j = -P \sum_{i=1}^n p_{ij} * ln (p_{ij})}
#'
#' Where
#'
#' \mjsdeqn{P = \frac{1}{ln(n)}}.
#'
#' Value of E is then in interval of <0;1>.
#'
#' Then we compute degree of divergence:
#'
#' \mjsdeqn{div_j = | 1 - E_j|}
#'
#' and from it entropy weights which are returned as result of the function:
#'
#'\mjsdeqn{Ew_j = \frac{div_j}{\sum_{j=1}^m div_j}}
#'
#' @param PM Matrix or data frame containing the performance table. Each row
#'  corresponds to an alternative, and each column to a criterion. only numeric
#'  values expercted. Rows and columns are expected to be named.
#' @param minmaxcriteria Vector containing the preference direction on each of
#'  the criteria. "min" (resp."max") indicates that the criterion has to be
#'  minimized (maximized).
#' @param VERBOSE if set to TRUE it produces all the outputs in the console,
#'  otherwise the results are available only as values returned by function.
#'
#' @return
#' list:
#' \itemize{
#'   \item normalizedPM - normalized performance matrix (with all criteria
#'  transformed to maximalize direction)
#'   \item probabilityCriteria - probability of criteria to occur
#'   \item degreeDivergence - degree of divergence
#'   \item entropyWeight - weights derived by EWM method for the criteria
#' }
#'
#' @references
#' Kumar, Raman; Bilga, Paramjit Singh Singh, Sehijpal. Multi objective
#'  optimization using different methods of assigning weights to energy
#'  consumption responses, surface roughness and material removal rate during
#'  rough turning operation. Journal of Cleaner Production, vol. 16, pp. 45-57,
#'  DOI: 10.1016/j.jclepro.2017.06.077.
#'
#' @examples
#' PM <- cbind(
#'   c(103000,101300,156400,267400,49900,103600,103000,170100,279700,405000),
#'   c(171.3,205.3,221.7,230.7,122.6,205.1,178.0,226.0,233.8,265.0),
#'   c(7.65,7.90,7.90,10.50,8.30,8.20,7.20,9.10,10.90,10.30),
#'   c(352,203,391,419,120,265,419,419,359,265),
#'   c(11.6,8.4,8.4,8.6,23.7,8.1,11.4,8.1,7.8,6.0),
#'   c(88.0,78.3,81.5,64.7,74.1,81.7,77.6,74.7,75.5,74.7),
#'   c(69.7,73.4,69.0,65.6,76.4,73.6,66.2,71.7,70.9,72.0))
#' rownames(PM) <- c("CBX16","P205G","P405M","P605S","R4GTL",
#'   "RCLIO","R21TS","R21TU","R25BA","ALPIN")
#' colnames(PM) <- c("Prix","Vmax","C120","Coff","Acce","Frei","Brui")
#' minmaxcriteria <-c("min","max","min","max","min","min","min")
#' M <- EWM(PM, minmaxcriteria)
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords EWM Entropy Weight Method
EWM <- function(PM, minmaxcriteria = "max", VERBOSE = FALSE) {

  ## check validity of the objects manipulated by the current function
  # with < 2 criteria or alternatives, there is no MCDA problem
  if (!(is.matrix(PM) || (is.data.frame(PM)))) {
    stop("wrong performanceMatrix, should be a matrix or a data frame")
  }
  if (!is.numeric(unlist(PM))) {
    stop("Only numeric values in performance matrix expected")
  }
  PM <- util_pm_minmax(PM, minmaxcriteria) #validate minmax and invert scales if necessary
  nalt <- nrow(PM)
  ## End of checking the validity of the "inputs"

  # function computes maximum in columns in matrix or data frame
  col_max <- function(data) {
    t <- apply(data, 2, max)
    return(t)
  }

  em <- sweep(PM, 2, col_max(PM), "/") # 1. normalize PM: EM = PM_ij/max(PM_j)
  # sum_{i=1}^n EM_{ij}, where j = criteria, i = alternatives,
  # n = number of alternatives
  p <- sweep(em, 2, colSums(em), "/") # probability of criteria to occur
  p2 <- p * log(p)
  # required since if p = 0, then ln(0) = -inf, so 0 * ln(0) = NaN
  na_values <- is.na(p2)
  p2[na_values] <- 0
  # E_j = -P sum_{i=1}^n p_{ij} * log_e (p_{ij})
  ej <- -(1 / log(nalt)) * colSums(p2)
  divj <- abs(1 - ej) # degree of divergence
  # Entropy weight
  ewj <- divj / sum(divj) # Ew_j = div_j / sum_{j=1}^m div_j

  if (VERBOSE) {
    print("Normalized performance matrix")
    print(em)
    print("probability of the criteria to occur")
    print(p2)
    print("degree of divergence")
    print(divj)
    print("entropy weights for the criteria")
    print(ewj)
  }

  out <- list(
    normalizedPM = em,
    probabilityCriteria = p2,
    degreeDivergence = divj,
    entropyWeight = ewj
  )
  return(out)
}