#' Comparisson Between Related Criteria
#'
#' @description
#' Comparisson Between Related Criteria (COBRAC) is a weighting method similar to
#'  \link{fucom}, Similary to it, we derive weight based on comparisson of
#'  neighbourhooding criteria (priority wise), which is one of main benefits of
#'  the method as it does not require full pairwise comparison.
#'
#' Unlike FUCOM the comparison is performed differently by means of participation
#'  in interval [0,1]. For example if
#'
#' \mjsdeqn{\xi_{n-1, n} = 0.72}
#'
#' Then C(n-1) occupies 72 percent of the participation interval and C(n) remaning
#'  28 percent. Another difference to FUCOM is in that it allows for same importance
#'  critecia by assigning 0.5 value.
#'
#' Values of xi represent local comparison criteria, which need to be rocomputed to
#'  global weight.This is done by solving linear problem of minimizing
#'  DFC (Deviation from Full Consistency):
#'
#' \mjsdeqn{min DFC}
#'
#' subject to
#'
#' \mjsdeqn{\left| \frac{w_{n-1}}{w_n}-\frac{\xi_{n-1,n}}{1-\xi_{n-1,n}} \right| \le DFC, \forall j }
#'
#' \mjsdeqn{\sum w_j = 1, \forall j}
#'
#' \mjsdeqn{w_j \ge 0, \forall j}
#'
#' The resulting DFC should be very close to 0.
#'
#' @param criteria vector with names of the criteria
#' @param xi vector of comaparative priorities
#' @param control list with additional controls for model computation [optional]
#'
#' @return
#' list:
#' \itemize{
#'    \item weights
#'    \item max_deviation,
#'    \item comparisons,
#'    \item convergence,
#'    \item message,
#'    \item objective
#' }
#'
#' @references
#' PAMUCAR, Dragan, SIMIC, Vladimir, GÖRÇÜN, Ömer Faruk, KÜÇÜKÖNDER, Hande.
#'  Selection of the best Big Data platform using COBRAC-ARTASI methodology
#'  with adaptive standardized intervals. Expert Systems with Applications.
#'  2024, vol. 239, article no. 122312, available from:
#'  https://doi.org/10.1016/j.eswa.2023.122312, ISSN 0957-4174.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords COBRAC weights
#'
#' @examples
#' xi <- c(0.5263, 0.5037, 0.5064, 0.5098, 0.5358, 0.5030, 0.5142)
#' criteria <- c("BE1", "BE2", "BE3", "BE6", "BE8", "BE4", "BE5", "BE7")
#' t <- cobrac(criteria, xi)
cobrac <- function(criteria, xi, control = list()) {
  ## validation of inputs
  if (!is.character(criteria) || length(criteria) < 2L ||
      anyNA(criteria) || any(!nzchar(criteria))) {
    stop("'missing criteria names")
  }
  if (anyDuplicated(criteria)) {
    stop("criteria names must be unique")
  }
  n <- length(criteria)
  if (!is.numeric(xi) || length(xi) != n - 1L ||
      any(!is.finite(xi)) || any(xi <= 0 | xi >= 1)) {
    stop("'xi' must have n-1 values in interval (0, 1).")
  }
  ## end of validation

  # target ratios of neighbouring criteria
  target_ratio <- xi / (1 - xi)

  # transfer parameters to positive weights with sum = 1
  # - max improves numerical stability of computation
  get_weights <- function(theta) {
    z <- c(theta, 0)
    z <- z - max(z)
    exp(z) / sum(exp(z))
  }

  # COBRAC: minimize maximal absolute deviation
  objective <- function(theta) {
    w <- get_weights(theta)
    ratios <- w[-n] / w[-1L]
    max(abs(ratios - target_ratio))
  }

  # first values = set local ratios
  log_weights <- numeric(n)
  for (j in seq_len(n - 1L)) {
    log_weights[j + 1L] <-
      log_weights[j] - log(target_ratio[j])
  }

  log_weights <- log_weights - log_weights[n]

  fit <- optim(
    par = log_weights[-n],
    fn = objective,
    method = "BFGS",
    control = modifyList(
      list(maxit = 10000, reltol = 1e-12),
      control
    )
  )

  w <- get_weights(fit$par)
  names(w) <- criteria

  ratios <- w[-n] / w[-1L]
  residuals <- ratios - target_ratio

  comparisons <- data.frame(
    criterion_1 = criteria[-n],
    criterion_2 = criteria[-1L],
    xi = xi,
    target_ratio = target_ratio,
    weight_ratio = ratios,
    residual = residuals
  )

  structure(
    list(
      weights = w,
      max_deviation = max(abs(residuals)),
      comparisons = comparisons,
      convergence = fit$convergence,
      message = fit$message,
      objective = fit$value
    ),
    class = "cobrac"
  )
}
