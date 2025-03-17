#' Integrated Determination of Objective CRIteria Weights
#'
#' @description
#' Introfuced by Zavadskas and Podvezko in 2016. Formaly it combines two other
#'  methods: \link{EWM} and \link{cilos}, in following manner:
#'
#' \mjsdeqn{w_j = \frac{q_j \cdot ew_j}{\sum_{i = 1}^n q_j \cdot ew_j}}
#'
#' where q are weights established using CILOS method and ew are weights
#'  derived using EWM method.
#'
#' @param pm performance matrix with alternatives in rows and criteria in
#'  columns. Only numeric values expercted. Rows and columns are expected to be
#'  named.
#' @param minmaxc Vector containing the preference direction on each of
#'  the criteria. "min" (resp."max") indicates that the criterion has to be
#'  minimized (maximized). Can be substituted with single min or max if all
#'  criteria share same optimization direction.
#'
#' @return vector of weights for the criteria
#'
#' @references
#' Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
#'  Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
#'  233 p., ISBN 978-3-030-15009-9
#'
#' Zavadskas, E. K., Podvezko, V. Integrated Determination of Objective
#'  Criteria Weights in MCDM. International Journal of Information Technology &
#'  Decision Making, vol. 15, DOI: 10.1142/S0219622016500036.
#'
#' @examples
#' alt <- c("A1", "A2", "A3", "A4")
#' cri <- c("C1", "C2", "C3", "C4")
#' minmax <- c("min", "max", "min", "max")
#' pm <- rbind(
#'   c(3, 100, 10, 7),
#'   c(2.5, 80, 8, 5),
#'   c(1.8, 50, 20, 11),
#'   c(2.2, 70, 12, 9)
#' )
#' rownames(pm) <- alt
#' colnames(pm) <- cri
#' t <- idocriw(pm, minmax)
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords IDOCRIW weights
idocriw <- function(pm, minmax = "max") {
  ## check validity of the objects manipulated by the current function
  # with < 2 criteria or alternatives, there is no MCDA problem
  ncri <- ncol(pm)
  cri <- colnames(pm)
  validation$validate_pm(pm)
  minmax <- validation$validate_minmax(minmax, ncri)
  ## End of checking the validity of the "inputs"

  t <- EWM(pm, minmax)
  ew <- t$entropyWeight
  q <- cilos(pm, minmax)
  w <- (q * ew) / sum(q * ew)
  names(w) <- cri
  return(w)
}