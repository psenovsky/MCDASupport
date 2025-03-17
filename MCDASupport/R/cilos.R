#' Criterion Impact LOSs
#'
#' @description
#' A method for criteria weight derivation based on measure impact loss.
#'
#' The method is interesting as it was first coined by Mirkin in 1974, but only
#'  in general terms. True formulation was performed by Zavadskas and Podvezko
#'  in 2016 for purposes of their method IDOCRIW. For their purposes they named
#'  the method CILOS.
#'
#' As first step the method requires all criteria to be maximized. Minimized
#'  criteria are expected to be translated to maximalization using
#'
#' \mjsdeqn{r_{ij} = \frac{min_i r_{ij}}{r_{ij}}}
#'
#' Next the whole performance matrix needs to be normalized.
#'
#' \mjsdeqn{x_{ij} = \frac{r_{ij}}{\sum_{i = 1}^n r_{ij}}}
#'
#' Next matrix A is constructed. To do that we need to identify maximal values
#'  \mjseqn{x_j} in each columns of the normalized performance matrix and rows
#'  \mjseqn{k} on which they reside.
#'
#' \mjsdeqn{a_{ij} = x_{kj}}
#'
#' meaning for example, that if in first column of the performance matrix the
#'  maximum was on third row, then
#'
#' \mjsdeqn{a_{11} = x_{31}}
#'
#' In short we construct the matrix A by copying rows from normalized
#'  performance matrix X corresponding to the row the maximum in the column
#'  was. That also means, that if we identified in such maximums in multiple
#'  columns, then the rows will repeat in matrix A.
#'
#' Next we compute matrix of relative losses P
#'
#' \mjsdeqn{p_{ij} = \frac{x_j - a_{ij}}{x_j}}
#'
#' Since the matrix A will always have identified maximums in its diagonal, the
#'  matrix P will always have zeros in its diagonal.
#'
#' Finaly we construct weight system matrix (F). We start with matrix P and
#'  minus sum the columns in the diagonal. The diagonal of F will thus always
#'  have negative numbers and sum of the column in F = 0.
#'
#' We will use the F as a basis for weight derivation
#'
#' \mjsdeqn{Fq^T = 0}
#'
#' by solving the equation to get q and normalizing it we get the desired
#'  weights.
#'
#' Please note that the system of equation is clearly homogenous system of
#'  equations with trivial solution q = 0, which is not useful for purposes of
#'  weight establishments. For establishment of weights non-trivial solutions
#'  need to be found.
#'
#' This implementation of the method uses singular valuable decomposition (SVD) of
#'  matrix F for this purpose.
#'
#' Final values of the weights is achieved by normalizing the solution.
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
#'
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
#' t <- cilos(pm, minmax)
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords CILOS weights
cilos <- function(pm, minmax = "max") {
  ## check validity of the objects manipulated by the current function
  # with < 2 criteria or alternatives, there is no MCDA problem
  ncri <- ncol(pm)
  cri <- colnames(pm)
  validation$validate_pm(pm)
  minmax <- validation$validate_minmax(minmax, ncri)
  ## End of checking the validity of the "inputs"

  # 2max for criteria being minimalized
  min <- apply(pm, 2, min)
  for (i in which(minmax == "min")) pm[, i] <- min[i] / pm[, i]
  # normalize the pm
  pm_norm <- sweep(pm, 2, colSums(pm), "/")
  # largest criteria
  l_crit <- apply(pm_norm, 2, max)
  r_crit <- rep(0, times = ncri)
  t <- matrix(0, nrow = ncri, ncol = ncri)
  rownames(t) <- colnames(t) <- cri
  a <- t
  for (i in 1:ncri) {
    r_crit[i] <- which(pm_norm[, i] == l_crit[i])
  }
  for (i in 1:ncri) {
    for (j in 1:ncri) {
      a[i, j] <- pm_norm[r_crit[i], j]
    }
  }
  p <- t
  for (i in 1:ncri) {
    for (j in 1:ncri) {
      p[i, j] <- (l_crit[j] - a[i, j]) / l_crit[j]
    }
  }
  # weight system matrix F
  impact_loss <- colSums(p)
  f <- p
  diag(f) <- -1 * impact_loss
  svd_result <- svd(f)
  # Adjust tolerance as needed
  zero_singular_values <- which(svd_result$d < 1e-10)
  w <- rep(0, times = ncri)
  if (length(zero_singular_values) > 0) {
    w <- svd_result$v[, zero_singular_values]
    w <- w / sum(w)
    names(w) <- cri
    return(w)
  } else {
    print("CILOS: No non-trivial solutions - no weights found")
    return(NULL)
  }
}