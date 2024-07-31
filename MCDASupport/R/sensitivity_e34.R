#' function generalizing sensitivity checking for Electre III and IV methods
#'
#' @description
#' sensitivity checking in Electre III \code{\link{electre3}} and IV
#'  \code{\link{electre4}} methods. The difference is in support of weights
#'  (Electre IV does not support these).
#'
#' This function generalizes common computation logic into this function.
#'
#' This function is intended for package internal use only.
#'
#' sensitivity analysis is performed on values of preference (p), indefference
#'  (q) and veto (v) thresholds.
#'
#' Since the thresholds (all of them) are specfied separately for each
#'  criterion, their sensitivity needs to be also evaluated seprately.
#'  This is being realized in this function by generating separate
#'  dataframe for each threshold (sens_p, sens_q, sens_v) with following
#'  structure:
#'
#' criterium; from; default; to
#'
#' From and to limits are limist of of solution stability, meaning that
#'  going under it (from) or over it (to) will produce different result.
#'
#' Sensitivity is tested from default value (the value used to produce
#'  result) going down to 0 for from column and up to maximal performance
#'  in the criterium j (max(pm[j])). If no change in result is detected,
#'  value "insens." is inserted into dataframe.
#'
#' @param object object representing a solution the sensitivity should be
#'  tested for
#' @param steps how many steps should sensitivity testing take. Interval
#'  for testing will be split to steps segments. The higher number of steps
#'  the smaller the step will be, the more granular the testing will be.
#'  Set to 100 by default
#'
#' @return
#' returns dataframes sens_p, sens_q and sens_v with sensitivity limit for
#'  the criteria
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords ELECTRE III
#' @keywords ELECTRE IV
#' @keywords sensitivity testing
sensitivity_e34 <- function(object, steps = 100) {

  # @description
  # sensitivity testing for preference threshold
  #
  # @param p preference threshold value to be tested for sensitivity
  # @param j criterium being tested
  #
  # @return
  # value of preference threshold at which provided solution for the decision
  # problem changes. If no change detected returns insens.
  sens_p2 <- function(p, j, e, model) {
    p2 <- e$p
    for (i in seq_along(p)) {
      p2[j] <- p[i]
      t <- switch(model,
                  "electre4" = electre4$new(e$pm_orig, p2, e$q, e$v, e$minmaxcriteria),
                  "electre3" = electre3$new(e$pm_orig, e$w, p2, e$q, e$v, e$minmaxcriteria),
                  stop("unsupported model"))
      if (!vector_compare(t$finalPreorder, e$finalPreorder)) {
        if (i != 1) return(p[i - 1])
        return(p[i])
      }
    }
    return("insens.")
  }

  # @description
  # sensitivity testing for indifference threshold
  #
  # @param q indifference threshold value to be tested for sensitivity
  # @param j criterium being tested
  #
  # @return
  # value of indifference threshold at which provided solution for the
  #  decision problem changes. If no change detected returns insens.
  sens_q2 <- function(q, j, e, model) {
    q2 <- e$q
    for (i in seq_along(q)) {
      q2[j] <- q[i]
      t <- switch(model,
                  "electre4" = electre4$new(e$pm_orig, e$p, q2, e$v, e$minmaxcriteria),
                  "electre3" = electre3$new(e$pm_orig, e$w, e$p, q2, e$v, e$minmaxcriteria),
                  stop("unsupported model"))
      if (!vector_compare(t$finalPreorder, e$finalPreorder)) {
        if (i != 1) return(q[i - 1])
        return(q[i])
      }
    }
    return("insens.")
  }

  # @description
  # sensitivity testing for veto threshold
  #
  # @param q veto threshold value to be tested for sensitivity
  # @param j criterium being tested
  #
  # @return
  # value of veto threshold at which provided solution for the
  #  decision problem changes. If no change detected returns insens.
  sens_v2 <- function(v, j, e, model) {
    v2 <- e$v
    for (i in seq_along(v)) {
      v2[j] <- v[i]
      t <- switch(model,
                  "electre4" = electre4$new(e$pm_orig, e$p, e$q, v2, e$minmaxcriteria),
                  "electre3" = electre3$new(e$pm_orig, e$w, e$p, e$q, v2, e$minmaxcriteria),
                  stop("unsupported model"))
      if (!vector_compare(t$finalPreorder, e$finalPreorder)) {
        if (i != 1) return(v[i - 1])
        return(v[i])
      }
    }
    return("insens.")
  }

  e <- object
  model <- class(e)
  models <- c("electre3", "electre4")
  if (!(model[1] %in% models)) stop("unsupporter model")
  ncri <- ncol(e$pm)
  cri <- colnames(e$pm)
  sens_p <- data.frame(matrix(0, nrow = ncri, ncol = 4))
  colnames(sens_p) <- c("criterium", "from", "default", "to")
  sens_q <- sens_p
  sens_v <- sens_p
  for (i in 1:ncri) {
    sens_p[i, 1] <- cri[i]
    sens_p[i, 3] <- e$p[i]
    sens_q[i, 1] <- cri[i]
    sens_q[i, 3] <- e$q[i]
    sens_v[i, 1] <- cri[i]
    sens_v[i, 3] <- e$v[i]
    m_i <- max(e$pm[, i])
    hyp_p0 <- rev(seq(from = e$q[i], to = e$p[i],
                      by = (e$p[i] - e$q[i]) / steps))
    step <- (e$v[i] - e$p[i]) / steps
    hyp_p1 <- seq(from = e$p[i], to = (e$v[i] - step), by = step)
    hyp_v0 <- rev(seq(from = e$p[i] + step, to = e$v[i], by = step))
    hyp_v1 <- seq(from = e$v[i], to = m_i,
                  by = (m_i - e$v[i]) / steps)
    hyp_q0 <- rev(seq(from = 0, to = e$q[i], by = e$q[i] / steps))
    hyp_q1 <- seq(from = e$q[i], to = e$p[i],
                  by = (e$p[i] - e$q[i]) / steps)
    sens_p[i, 2] <- sens_p2(hyp_p0, i, e, model[1])
    sens_p[i, 4] <- sens_p2(hyp_p1, i, e, model[1])
    sens_q[i, 2] <- sens_q2(hyp_q0, i, e, model[1])
    sens_q[i, 4] <- sens_q2(hyp_q1, i, e, model[1])
    sens_v[i, 2] <- sens_v2(hyp_v0, i, e, model[1])
    sens_v[i, 4] <- sens_v2(hyp_v1, i, e, model[1])
  }
  t <- list(
    sens_p = sens_p,
    sens_q = sens_q,
    sens_v = sens_v
  )
  return(t)
}