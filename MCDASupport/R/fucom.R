#' Full Consistency method
#'
#' @description
#' Full Consistency Method (FUCOM) is weight derivation method. The weights are
#' derived based on comparisson of neighbourhooding criteria (priority wise),
#' which is one of main benefits of the method as it does not require Full
#' pairwise comparison.
#'
#' It takes comparative priority vector phi as input, in which the criteria are ordered by
#' prioriry from highest to lowes. Comparative priorities then mean how much more
#' important the more important criterium is. For first criterium value is always 1,
#' for all other elements of the vector the value must be > 1.
#'
#' Final weights are established by solving nonlinear optimization problem of minimizing
#' DFC (Deviation from Full Consistency):
#'
#' \mjsdeqn{min DFC}
#'
#' subject to
#'
#' \mjsdeqn{\left| \frac{w_{j_(k)}}{w_{j_(k+1)}}-\varphi_{k/(k+1)} \right| \le DFC, \forall j }
#'
#' \mjsdeqn{\left| \frac{w_{j_(k)}}{w_{j_(k+1)}}-\varphi_{k/(k+1)} \otimes \varphi{(k+1)/(k+2)} \right| \le \chi, \forall j}
#'
#' \mjsdeqn{\sum w_j = 1, \forall j}
#'
#' \mjsdeqn{w_j \ge 0, \forall j}
#'
#' First constrains takes in the account the ration of neighbourhooding criteria weights,
#' while the second one focuses on mathematical transitivity.
#'
#' The resulting DFC should be very close to 0.
#'
#' @param phi vector of comaparative priorities
#'
#' @return
#' list:
#' \itemize{
#'    \item w - derived weights
#'    \item DFC - Deviation from Full Consistency
#' }
#'
#' @references
#' Pamučar, D., Stević, Ž., & Sremac, S. (2018). A New Model for Determining Weight
#'  Coefficients of Criteria in MCDM Models: Full Consistency Method (FUCOM).
#'  Symmetry, _10_(9), 393. https://doi.org/10.3390/sym10090393
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords FUCOM weights
#'
#' @examples
#' phi <- c(1, 1.08, 1.25, 1.45)
#' t <- fucom(phi)
fucom <- function(phi) {
  ## Validation
  if (!is.numeric(phi)) stop("comparison vector phi must be numeric verctor")
  n <- length(phi)
  if (n < 2) stop("phi is not a vector")
  if (phi[1] != 1) stop("phi[1] must be 1")
  for (i in 2:n) {
    if (phi[i] <= 1) stop("elements 2+ in vector phi must be > 1")
  }
  ## end of validation

  # Objective function: minimize chi (the last element of x)
  eval_f <- function(x) {
    return(x[n + 1])
  }

  # Equality constraint: sum of weights equals 1
  eval_g_eq <- function(x) {
    w <- x[1:n]
    return(sum(w) - 1)
  }

  # Inequality constraints (FUCOM conditions and deviation chi)
  eval_g_ineq <- function(x) {
    w <- x[1:n]
    chi <- x[n + 1]
    res <- c()

    # 1. Adjacent criteria constraints (k from 1 to n-1)
    for (k in 1:(n - 1)) {
      ratio <- w[k] / w[k + 1]
      phi_val <- phi[k + 1]
      # |(w_k / w_{k+1}) - phi_val| <= chi
      res <- c(res, ratio - phi_val - chi)
      res <- c(res, phi_val - ratio - chi)
    }

    # 2. Transitivity constraints (k from 1 to n-2)
    if (n >= 3) {
      for (k in 1:(n - 2)) {
        ratio_trans <- w[k] / w[k + 2]
        phi_trans <- prod(phi[(k + 1):(k + 2)])
        # |(w_k / w_{k+2}) - (phi_{k/(k+1)} * phi_{(k+1)/(k+2)})| <= chi
        res <- c(res, ratio_trans - phi_trans - chi)
        res <- c(res, phi_trans - ratio_trans - chi)
      }
    }

    return(res)
  }

  # Initial estimates: equal weights and small chi
  x0 <- c(rep(1 / n, n), 0.05)

  # Bounds: weights must be positive (>0), chi >= 0
  lb <- c(rep(1e-5, n), 0)
  ub <- c(rep(1, n), Inf)

  # Run optimization using nloptr (COBYLA algorithm)
  opts <- list(
    algorithm = "NLOPT_LN_COBYLA",
    xtol_rel = 1e-6,
    maxeval = 2000,
    print_level = 0
  )

  result <- nloptr(
    x0 = x0,
    eval_f = eval_f,
    lb = lb,
    ub = ub,
    eval_g_eq = eval_g_eq,
    eval_g_ineq = eval_g_ineq,
    opts = opts
  )

  if (result$status < 0) {
    warning("Optimization might have failed to converge properly.")
  }

  # Extract results
  w_opt <- result$solution[1:n]
  if(!is.null(names(phi))) {
    names(w_opt) <- names(phi)
  } else {
    names(w_opt) <- paste0("C", 1:n)
  }

  dfc_val <- result$solution[n + 1]

  return(list(w = w_opt, DFC = dfc_val))
}
