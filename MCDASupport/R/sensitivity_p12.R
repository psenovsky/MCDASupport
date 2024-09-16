#' test sensitivity of the PROMETHEE I and II models to changes in the
#'  thresholds.
#'
#' @description
#' Function tests sensitivity of \link{promethee1} and \link{promethee2} models
#'  to changes of thresholds.
#'
#' Computes sens_i (for indifference threshold), sens_p (for prefference
#'  threshold) and sens_im (for intermediate treshold) dataframes in
#'  structure:
#'
#' \itemize{
#'   \item criterium
#'   \item from - lower bound of sensitivity
#'   \item default - value computed by original model
#'   \item to - upper bound of sensitivity
#'   \item function - type of preference function for criterium
#' }
#'
#' Dataframes do have a numeric value with lower/upper bound of
#'  sensitivity identifying last value of the threshold for which the
#'  result still doesn't change.
#'
#' If no such value is identified "insens." is provided.
#'
#' Also note that PROMETHEE function is complex, because of existence of
#'  "preference" functions, which are stated separately for each criterium
#'  and each of these has a different requirements on types of thresholds
#'  it uses. For example level and linear functions use both preference and
#'  indifference thresholds (but not intermediate). V-shape function uses
#'  prefference threshold only, U-shape uses indifference threshold only.
#'
#' Gaussian function uses intermediate threshold only, but if preference
#'  andindifference thresholds are provided, the sensitivity is being
#'  tested on in interval between these two.
#'
#' Function is intended for internal use only
#'
#' Also note that right now the sensitivity testing for PROMETHEE I and II
#'  presumes, that if positive and negative flows remain same, so does the
#'  final ranking. That means that sensitivity of both methods is tested
#'  through optic of the PROMETHEE I method. If above mentioned presumption
#'  would not hold, the PROMETHEE II (or III) would need to be tested based
#'  on either net flow or from it derived ranking.
#'
#' @param object PROMETHEE I, II or III model we want to test sensitivity for
#' @param step number of steps to divide threshold testing interval (default:
#'  100)
#'
#' @return dataframes sens_i, sens_p and sens_im with sensitivity limit for
#'  the criteria
sensitivity_p12 <- function(object, step = 100) {

  # @description
  # sensitivity testing for preference threshold
  #
  # @param p vector of preference threshold values to be tested for
  #  sensitivity
  # @param j criterium being tested
  # @param e PROMETHEE I or II object
  # @param model model class name
  #
  # @return
  # value of preference threshold at which provided solution for the decision
  # problem changes. If no change detected returns insens.
  sens_p2 <- function(p, j, e, model) {
    p2 <- e$p_threshold
    for (i in seq_along(p)) {
      p2[j] <- p[i]
      t <- switch(model,
        "promethee1" = promethee1$new(e$pm_orig, e$pref_function, e$w,
                                      e$minmax, e$i_threshold, p2,
                                      e$im_threshold),
        "promethee2" = promethee2$new(e$pm_orig, e$pref_function, e$w,
                                      e$minmax, e$i_threshold, p2,
                                      e$im_threshold),
        "promethee3" = promethee3$new(e$pm_orig, e$pref_function, e$w,
                                      e$minmax, e$i_threshold, p2,
                                      e$im_threshold),
        stop("unsupported model")
      )
      if (!vector_compare(t$positiveFlow, e$positiveFlow) ||
            !vector_compare(t$negativeFlow, e$negativeFlow)) {
        if (i != 1) return(p[i - 1])
        return(p[i])
      }
    }
    return("insens.")
  }

  # @description
  # sensitivity testing for indifference threshold
  #
  # @param indiff vector of indifference threshold values to be tested for
  #  sensitivity
  # @param j criterium being tested
  # @param e PROMETHEE I, II or II object
  # @param model model class name
  #
  # @return
  # value of indifference threshold at which provided solution for the
  # decision problem changes. If no change detected returns insens.
  sens_i2 <- function(indiff, j, e, model) {
    indiff2 <- e$i_threshold
    for (i in seq_along(indiff)) {
      indiff2[j] <- indiff[i]
      t <- switch(model,
        "promethee1" = promethee1$new(e$pm_orig, e$pref_function, e$w,
                                      e$minmax, indiff2, e$p_threshold,
                                      e$im_threshold),
        "promethee2" = promethee2$new(e$pm_orig, e$pref_function, e$w,
                                      e$minmax, indiff2, e$p_threshold,
                                      e$im_threshold),
        "promethee3" = promethee3$new(e$pm_orig, e$pref_function, e$w,
                                      e$minmax, indiff2, e$p_threshold,
                                      e$im_threshold),
        stop("unsupported model")
      )
      if (!vector_compare(t$positiveFlow, e$positiveFlow) ||
            !vector_compare(t$negativeFlow, e$negativeFlow)) {
        if (i != 1) return(indiff[i - 1])
        return(indiff[i])
      }
    }
    return("insens.")
  }

  # @description
  # sensitivity testing for intermediate threshold
  #
  # @param im vector of intermediate threshold values to be tested for
  #  sensitivity
  # @param j criterium being tested
  # @param e PROMETHEE I, II or III object
  # @param model model class name
  #
  # @return
  # value of intermediate threshold at which provided solution for the
  # decision problem changes. If no change detected returns insens.
  sens_im2 <- function(im, j, e, model) {
    im2 <- e$im_threshold
    for (i in seq_along(im)) {
      im2[j] <- im[i]
      t <- switch(model,
        "promethee1" = promethee1$new(e$pm_orig, e$pref_function, e$w,
                          e$minmax, e$i_threshold, e$p_threshold,
                          im2),
        "promethee2" = promethee2$new(e$pm_orig, e$pref_function, e$w,
                          e$minmax, e$i_threshold, e$p_threshold,
                          im2),
        "promethee3" = promethee3$new(e$pm_orig, e$pref_function, e$w,
                          e$minmax, e$i_threshold, e$p_threshold,
                          im2),
        stop("unsupported model")
      )
      if (!vector_compare(t$positiveFlow, e$positiveFlow) ||
            !vector_compare(t$negativeFlow, e$negativeFlow)) {
        if (i != 1) return(im[i - 1])
        return(im[i])
      }
    }
    return("insens.")
  }


  # check validity of parameters
  model <- class(object)
  models <- c("promethee1", "promethee2", "promethee3")
  if (!(model[1] %in% models)) stop("unsupporter model")
  if (!is.numeric(step) || step < 0) {
    stop("step parameter mus be positive number")
  }
  ncri <- ncol(object$pm)
  cri <- colnames(object$pm)
  sens_p <- data.frame(matrix(0, nrow = ncri, ncol = 5))
  colnames(sens_p) <- c("criterium", "from", "default", "to", "function")
  sens_i <- sens_p
  sens_im <- sens_p
  pref_types <- c("V-shape", "level", "linear")
  for (i in 1:ncri) {
    sens_p[i, 5] <- sens_im[i, 5] <- sens_i[i, 5] <- object$pref_function[i]
    sens_p[i, 1] <- sens_im[i, 1] <- sens_i[i, 1] <- cri[i]
    if (object$pref_function[i] %in% pref_types) {
      sens_p[i, 3] <- object$p_threshold[i]
    } else {
      sens_p[i, 2:4] <- "NA"
    }
    if (object$pref_function[i] %in% pref_types) {
      sens_i[i, 3] <- object$i_threshold[i]
    } else {
      sens_i[i, 2:4] <- "NA"
    }
    if (object$pref_function[i] == "Gaussian") {
      sens_im[i, 3] <- object$im_threshold[i]
    } else {
      sens_im[i, 2:4] <- "NA"
    }
    m_i <- max(object$pm[, i])

    # functions level and linear (both p & i)
    if (object$pref_function[i] %in% c("level", "linear")) {
      s <- (object$p_threshold[i] - object$i_threshold[i]) / step
      hyp_i0 <- rev(seq(from = 0, to = object$i_threshold[i],
                        by = object$i_threshold[i] / step))
      hyp_i1 <- seq(from = object$i_threshold[i],
                    to = object$p_threshold[i] - s, by = s)
      hyp_p0 <- rev(hyp_i1)
      hyp_p1 <- seq(from = object$p_threshold[i], to = m_i,
                    by = (m_i - object$p_threshold[i] / step))
      sens_p[i, 2] <- sens_p2(hyp_p0, i, object, model[1])
      sens_p[i, 4] <- sens_p2(hyp_p1, i, object, model[1])
      sens_i[i, 2] <- sens_i2(hyp_i0, i, object, model[1])
      sens_i[i, 4] <- sens_i2(hyp_i1, i, object, model[1])
    }

    # prefference threshold (p) only
    if (object$pref_function[i] == "V-shape") {
      hyp_p0 <- rev(seq(from = 0, to = object$p_threshold[i],
                        by = object$p_threshold[i] / step))
      hyp_p1 <- seq(from = object$p_threshold[i], to = m_i,
                    by = (m_i - object$p_threshold[i] / step))
      sens_p[i, 2] <- sens_p2(hyp_p0, i, object, model[1])
      sens_p[i, 4] <- sens_p2(hyp_p1, i, object, model[1])
    }

    # only indifference threshold (i)
    if (object$pref_function[i] == "U-shape") {
      hyp_i0 <- rev(seq(from = 0, to = object$i_threshold[i],
                        by = object$i_threshold[i] / step))
      hyp_i1 <- seq(from = object$i_threshold[i], to = m_i,
                    by = (m_i - object$i_threshold[i]) / step)
      sens_i[i, 2] <- sens_i2(hyp_i0, i, object, model[1])
      sens_i[i, 4] <- sens_i2(hyp_i1, i, object, model[1])
    }

    # intermediate threshold (im)
    if (object$pref_function[i] == "Gaussian") {
      if (!is.null(object$i_threshold[i]) &&
            is.numeric(object$i_threshold[i]) &&
            !is.null(object$p_threshold[i]) &&
            is.numeric(object$p_threshold[i]) &&
            object$p_threshold[i] > object$i_threshold[i]) {
        # im based on p and i
        t <- seq(from = object$i_threshold[i], to = object$im_threshold[i],
                 by = (object$im_threshold[i] - object$i_threshold[i]) / step)
        hyp_im0 <- rev(t)
        s <- (object$p_threshold[i] - object$im_threshold[i]) / step
        hyp_im1 <- seq(from = object$im_threshold[i],
                       to = object$p_threshold - s, by = s)
      } else { # im only
        hyp_im0 <- rev(seq(from = 0, to = object$im_threshold[i],
                           by = object$im_threshold[i] / step))
        hyp_im1 <- seq(from = object$im_threshold[i], to = m_i,
                       by = (m_i - object$im_threshold[i]) / step)
      }
      sens_im[i, 2] <- sens_im2(hyp_im0, i, object, model[1])
      sens_im[i, 4] <- sens_im2(hyp_im1, i, object, model[1])
    }
  } # end of cycle iterating over criteria
  t <- list(
    sens_prefference = sens_p,
    sens_indifference = sens_i,
    sens_intermediate = sens_im
  )
  return(t)

}