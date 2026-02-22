#' Compute weights of the criteria using objective weighting methods.
#'
#' @description
#' The function for criteria weights computation using various objective 
#'  weighting methods. At present time supported are:
#' 
#' \tabular{llc}{
#'        \bold{constant} \tab \bold{method} \tab \bold{normalization}\cr
#'        MW \tab Mean weighting method \tab N \cr
#'        SDW \tab Standard Deviation Weighting method \tab Y \cr
#'    }
#' 
#' In normalization column of previous table Y means that the analyst can also
#'  choose normalization method. All normalization methods from mcda_norm can
#'  be used. If no normalization method is provided, the function uses default
#'  one for the method, usually min-max normalization.
#' 
#' \bold{MW - Mean weighting Method}
#' 
#' Method presumes that the weights of all criteria are the same (there are no)
#'  meaningfull differences between them. Thus method approximates the weigts 
#'  as a mean value:
#' 
#' \mjsdeqn{w_j = \frac{1}{n}}
#' 
#' where n is number of criteria in decision problem.
#' 
#' \bold{SDW - Standard Deviation Weighting Method}
#' 
#' Steps:
#' 
#' 1) normalize data, min-max method is default, but you can choose to use
#'  other normalization method
#' 
#' 2) compute standard deviation (SD) of the population for the criteria:
#' 
#' \mjsdeqn{\sigma_j = \sqrt{\frac{\sum_{i=1}^m (F_{ij} - \overline{F_j})^2}{m}}}
#' 
#' For alll j in (1, 2, ..., n), where F`j is arithmetic mean of normalized
#'  values.
#' 
#' 3) calcutate the weights by normalizing the sigma to max sigma
#' 
#' \mjsdeqn{w_j = \frac{\sigma_j}{\sum_{k = 1}^m \sigma_k}}
#' 
#' @param pm performance matrix
#' @param method weight computation method
#' @param minmax 'min' or 'max' to specify cost or benefit criterion, max is
#'  default value
#' @param norm specify normalization method, use constant from table above,
#'  the 'minmax' is default value.
#'
#' @return weights for the criteria
#' 
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#' @examples
#' alternatives <- c("A1", "A2", "A3", "A4", "A5")
#' c1 <- c(0.185, 0.3, 0.72, 0.44, 0.98)
#' c2 <- c(3.14, 1, 7.4, 6.4, 8.2)
#' c3 <- c(420, 750, 300, 634, 248)
#' pm <- cbind(c1, c2, c3)
#' rownames(pm) <- alternatives
#' minmax <- c("max", "min", "max")
#' t <- mcda_objective_weights(pm, method = "MW", minmax = minmax, method = "MW")
#'
#' @keywords weighting MW
mcda_objective_weights <- function(pm, method, minmax = "max", norm = "minmax") {
  # validate inputs
  t <- c("min", "max")
  validation$validate_invalid_val(minmax, t, "minmax")
  validation$validate_pm(pm)
  n <- length(minmax)
  if (n == 1) minmax <- rep(minmax, times = n)
  nmethods <- c(
    "MW",
    "SDW"
  )
  validation$validate_invalid_val(method, nmethods, "objective weighting method")
  # end of validation

  # MW - Mean Weighting Method
  MW <- function(pm) {
    ncri <- ncol(pm)
    w <- rep(1/ncri, times = ncri)
    return(w)
  }

  # SDW - Standard Deviation Weighting Method
  SDW <- function(pm, minmax, method = "minmax") {
    ncri <- ncol(pm)
    nalt <- nrow(pm)
    pm2 <- pm
    SD <- rep(0, times = ncri)
    for(i in 1:ncri) { # step 1) normalization
      pm2[, i] <- mcda_norm(pm[, i], minmax = minmax[i], method = method)
      SD[i] <- sd(pm2[, i]) * sqrt((nalt - 1) / nalt) # step 2) compute SD
    }
    w <- SD / sum(SD)
    return(w)
  }

  # perform weight computation based on selected method
  result <- switch(
    method,
    "MW" = MW(pm), # Mean Weighting Method
    "SDW" = SDW(pm, minmax, norm) # Standard Deviation Method
  )
}