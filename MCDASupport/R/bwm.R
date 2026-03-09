#' Best worst method
#'
#' @description
#' Best-worse method (BWM) uses similar approach to weight estimation as AHP,
#'  but simplifies requrements on pairwise comparisons. While in AHP
#'  combination of all criteria importance is required, BWM compares only best
#'  to others and worse criterium to other, all other weights can be derived by
#'  solving linear programing optimization problem.
#'
#' Similarly to AHP Consistency ratio can be computed to identify how
#'  consistent these preferences are, although the interpretation is different.
#'
#' \itemize{
#'   \item CR <= 0.1 - exceptional consistency (rare in real-world studies)
#'   \item CR in (0.1; 0.25) - good consistency, usually acceptable in studies
#'   \item CR > 0.3 - high level of preferences inconsistence, reevaluate
#'  preferences
#'}
#' @param a_best vector of preferences of best criterium to others (preference
#'  1-9)
#'
#' @param a_worst vector of preferences of all other criteri to worst
#'  criterium (preference 1-9)
#' 
#' @return
#' list:
#' \itemize{
#'    \item weights - derived weights
#'    \item xi - inconsistence indicator
#'    \item CR - consistency ratio
#'    \item status - status of LP problem solution
#' }
#'
#' @references
#' REZAEI, Jafar. Best-worst multi-criteria decision-making method. Omega.
#'  2015, vol. 53, s. 49–57, available from:
#'  https://doi.org/10.1016/j.omega.2014.11.009, ISSN 0305-0483.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords BWM weights
bwm <- function(a_best, a_worst) {
  ## Validation
  if (!is.numeric(a_best) || !is.numeric(a_worst)) {
    stop("a_best and a_worst must be numeric verctors.")
  }
  if (length(a_best) != length(a_worst)) {
    stop("a_best and a_worst must be of same length.")
  }
  n <- length(a_best)
  if (n < 2) {
    stop("At minimum two criteria are required.")
  }
  if (any(!is.finite(a_best)) || any(!is.finite(a_worst))) {
    stop("a_best and a_worst must not contain NA/NaN/Inf.")
  }
  if (any(a_best <= 0) || any(a_worst <= 0)) {
    stop("All values in a_best and a_worst must be positive.")
  }
  # end of validation
  best_index <- match(1, a_best)
  worst_index <- match(1, a_worst)

  # linear program
  obj <- c(rep(0, n), 1) # objective function: min xi

  # Subject to:
  # for all j:
  # 1)  w_B - a_Bj * w_j - xi <= 0
  # 2)  a_Bj * w_j - w_B - xi <= 0
  # 3)  w_j - a_jW * w_W - xi <= 0
  # 4)  a_jW * w_W - w_j - xi <= 0
  # sum_j w_j = 1
  A <- matrix(0, nrow = 4 * n + 1, ncol = n + 1)
  dir <- rep("<=", 4 * n + 1)
  rhs <- rep(0, 4 * n + 1)

  row <- 0
  for (j in 1:n) {
    # 1) w_B - a_Bj * w_j - xi <= 0
    row <- row + 1
    A[row, best_index] <- 1
    A[row, j] <- A[row, j] - a_best[j]
    A[row, n + 1] <- -1

    # 2) a_Bj * w_j - w_B - xi <= 0
    row <- row + 1
    A[row, j] <- A[row, j] + a_best[j]
    A[row, best_index] <- A[row, best_index] - 1
    A[row, n + 1] <- -1

    # 3) w_j - a_jW * w_W - xi <= 0
    row <- row + 1
    A[row, j] <- A[row, j] + 1
    A[row, worst_index] <- A[row, worst_index] - a_worst[j]
    A[row, n + 1] <- -1

    # 4) a_jW * w_W - w_j - xi <= 0
    row <- row + 1
    A[row, worst_index] <- A[row, worst_index] + a_worst[j]
    A[row, j] <- A[row, j] - 1
    A[row, n + 1] <- -1
  }

  # sum_j w_j = 1
  row <- row + 1
  A[row, 1:n] <- 1
  dir[row] <- "="
  rhs[row] <- 1

  # lower and upper bounds of variables
  lower <- rep(0, n + 1) # w_j >= 0, xi >= 0
  upper <- rep(Inf, n + 1)

  A <- rbind(A, -diag(n + 1))
  dir <- c(dir, rep("<=", n + 1))
  rhs <- c(rhs, rep(0, n + 1))

  sol <- lpSolve::lp(
      direction = "min",
      objective.in = obj,
      const.mat = A,
      const.dir = dir,
      const.rhs = rhs)
  w <- sol$solution[1:n]
  xi <- sol$solution[n + 1]
  if (sum(w) > 0) w <- w / sum(w)
  # tabulated values of CI (Consistency Index) to compute CR
  ci_table <- c(0, 0, 0.44, 1.00, 1.63, 2.30, 3.00, 3.73, 4.47, 5.23)
  a_bw <- a_best[worst_index]
  cr <- 0  
  if (n <= 9) {
    cr <- xi / ci_table[a_bw + 1]
  }
  
  res <- list(
    weights = as.numeric(w),
    xi = as.numeric(xi),
    CR = cr,
    status = switch(
      as.character(sol$status),
      "0" = "optimal",
      "1" = "suboptimal",
      "2" = "infeasible",
      "3" = "unbounded",
      "4" = "degenerate",
      "5" = "numerical failure",
      "6" = "process aborted",
      "7" = "timeout",
      "9" = "problem failure",
      "10" = "presolved",
      sol$status
    )
  )
  return(res)
}