#' Combination Weight Method
#' 
#' @description
#' Approach derives weights from two sets of weights usualy one set is derived
#'  usually by objective weighting method such as EWM, COPRAS (and many other)
#'  and the other based on weighs derived from preferences of decision maker
#'  such as AHP, BWM and many other methods.
#' 
#' It is claimed that the derived weights are more stable, robust, and reliable
#'  evaluation by reducing randomness and bias, but there is no "hard" scientific
#'  evidence of the method being able to do that.
#' 
#' Mathematically:
#' 
#' \mjsdeqn{w = \alpha w_1 + (1 - \alpha) w_2}
#' 
#' Both weights do not need to be normalized - the function is normalizing provided
#'  weights.
#' 
#' @examples
#' w1 <- c(1, 3, 4)
#' w2 <- c(1, 4, 3)
#' w <- cwm(w1, w2, alpha = 0.5)
cwm <- function(w1, w2, alpha = 0.5) {
  # validation
  if (!(is.vector(w1, mode = "numeric")) || !(is.vector(w2, mode = "numeric"))) {
    stop("criteria weights (w1, w2) must be numeric vector")
  }
  if (length(w1) != length(w2)) {
    stop("different legth of weight vectors w1 and w2")
  }
  validation$validate_value_in_interval(alpha, 0, 1, "alpha")
  w1 <- w1 / sum(w1)
  w2 <- w2 / sum(w2)

  # end of validation
  w <- alpha * w1 + (1 - alpha) * w2
}