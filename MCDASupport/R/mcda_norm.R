# normalize finction
#
# normaslizes provided vector using specified normalization method
#
# parameters
#   tonorm - vector to norm
#   minmax - min/max specifying if the the criterium is benefit or cost one
#   method - normalization method used
mcda_norm <- function(tonorm, minmax = "max", method = "minmax") {
  # validate inputs
  if (minmax != "max" && minmax != "min") {
    stop("minmax parameter expected to be max or min.")
  }
  if (!is.vector(tonorm)) stop("tonorm paramerer expected to be vector.")
  if (!is.numeric(tonorm)) stop("tonorm expected to be numeric vector.")
  nmethods <- c("LaiHwang",
                "linear aggregation",
                "logarithm",
                "Markovic",
                "minmax",
                "nonlinear",
                "toaverage",
                "tobest",
                "TzengHuang",
                "vector",
                "ZavadskasTurskis",
                "zscore")
  if (!(method %in% nmethods)) {
    stop("trying to normalize with unsupported (unknown) normalization method")
  }

  #LaiHwang normalization
  norm_lai_hwang <- function(tonorm, minmax = "max") {
    maximum <- max(tonorm)
    minimum <- min(tonorm)
    if (minmax == "max") {
      z <- tonorm / (maximum - minimum)
    } else {
      z <- tonorm / (minimum - maximum)
    }
    return(z)
  }

  # Linear normalization using aggregation of values
  norm_linear_aggreg <- function(tonorm, minmax = "max") {
    sx <- sum(tonorm)
    if (sx == 0) {
      stop("sum of all values = 0, use different normalization method.")
    }
    if (minmax == "max") {
      z <- tonorm / sx
    } else {
      z <- (1 / tonorm) / sum(1 / tonorm)
    }
    return(z)
  }

  # logarithmic noemalization
  norm_logarithm <- function(tonorm, minmax = "max") {
    p <- Reduce("*", tonorm)
    z <- log(tonorm) / p
    if (minmax == "min") z <- (1 - z) / (length(tonorm) - 1)
    return(z)
  }

  # Markovic normalization
  norm_markovic <- function(tonorm) {
    maximum <- max(tonorm)
    minimum <- min(tonorm)
    z <- 1 - (tonorm - minimum) / maximum
    return(z)
  }

  # min-max method
  norm_minmax <- function(tonorm, minmax = "max") {
    minimum <- min(tonorm)
    maximum <- max(tonorm)
    if (maximum == minimum) stop("All values same, nothing to normalize")
    z <- (tonorm - minimum) / (maximum - minimum)
    if (minmax == "min") z <- 1 - z
    return(z)
  }

  # nonlinear normalization
  norm_nonlinear <- function(tonorm, minmax = "max") {
    if (minmax == "max") {
      z <- (tonorm / max(tonorm))^2
    } else {
      z <- (min(tonorm) / tonorm)^2
    }
    return(z)
  }

  # normalizing to average
  norm_toaverage <- function(tonorm) {
    av <- mean(tonorm)
    if (av == 0) {
      stop("Mean of the tonorm vector is zero, so it is imposible 
        to normalize it using this function.")
    }
    z <- 100 * tonorm / av
    return(z)
  }

  # normalize to best value
  norm_tobest <- function(tonorm){
    maximum <- max(tonorm)
    if (maximum == 0) {
      stop("Maximal value in the vector is 0, 
        unable to normalize using this function.")
    }
    z <- 100 * tonorm / maximum
    return(z)
  }

  # Tzeng-Huang normalization
  norm_tzeng_huang <- function(tonorm) {
    maximum <- max(tonorm)
    z <- maximum / tonorm
    return(z)
  }

  # vector normalization
  norm_vector <- function(tonorm, minmax = "max") {
    x2 <- tonorm * tonorm
    if (minmax == "max") {
      z <- tonorm / sqrt(sum(x2))
    } else {
      z <- (1 / tonorm) / sqrt(sum(1 / x2))
    }
    return(z)
  }

  # Zavadskas-Turskis normalization
  norm_zavadskas_turskis <- function(tonorm, minmax = "max") {
    maximum <- max(tonorm)
    minimum <- min(tonorm)
    if (minmax == "max") {
      z <- 1 - abs((maximum - tonorm) / maximum)
    } else {
      z <- 1 - abs((minimum - tonorm) / minimum)
    }
    return(z)
  }

  # Z score
  norm_zscore <- function(tonorm) {
    z <- (tonorm - mean(tonorm)) / sd(tonorm)
    return(z)
  }

  # perform normalization
  result <- switch(
    method,
    "LaiHwang" = norm_lai_hwang(tonorm, minmax = minmax),
    "linear aggregation" = norm_linear_aggreg(tonorm, minmax = minmax),
    "logarithm" = norm_logarithm(tonorm, minmax = minmax),
    "Markovic" = norm_markovic(tonorm),
    "minmax" = norm_minmax(tonorm, minmax = minmax),
    "nonlinear" = norm_nonlinear(tonorm, minmax = minmax),
    "toaverage" = norm_toaverage(tonorm),
    "tobest" = norm_tobest(tonorm),
    "TzengHuang" = norm_tzeng_huang(tonorm),
    "vector" = norm_vector(tonorm, minmax),
    "ZavadskasTurskis" = norm_zavadskas_turskis(tonorm, minmax),
    "zscore" = norm_zscore(tonorm)
  )
  return(result)
}