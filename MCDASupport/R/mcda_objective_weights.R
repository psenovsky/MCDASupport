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
#'        SVW \tab Statistical Variance Weighting method \tab Y \cr
#'        EWM \tab Entropy Weight Method \tab N \cr
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
#' Computes weights based on standard deviation of the criteria.
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
#' \bold{SVW - Statistical Variance Weighting method}
#' 
#' Allows to compute the weights of criteria based on differences in variance
#'  in performance of altrernatives in the criteria.
#' 
#' Steps:
#' 
#' 1) normalize data, min-max method is default, but you can choose to use
#'  other normalization method
#' 
#' 2) compute variance of normalized data. Again population variance is being used
#' 
#' \mjsdeqn{\sigma_j^2 = \frac{\sum_{i=1}^m (F_{ij} - \overline{F_j})^2}{m}}
#' 
#' 3) Calculate the weights
#' 
#' \mjsdeqn{w_j = \frac{\sigma_j^2}{\sum_{k = 1}^m \sigma_k^2}}
#' 
#' \bold{EWM - Entropy Weight Method}
#' 
#' Allows us to establish weight system just based on evaluation of the entropy
#'  in input data of the performance matrix.
#' 
#' \mjsdeqn{EM_{ij} = \frac{PM_{ij}}{max(PM_{ij})_j}}
#'
#' Then we compute probability of criteria to occur.
#'
#' \mjsdeqn{p_{ij} = \frac{EM_{ij}}{\sum_{i=1}^n EM_{ij}}}
#'
#' Where j = criteria, i = alternatives, n = number of alternatives.
#'
#' Then we compute entropy E:
#'
#' \mjsdeqn{E_j = -P \sum_{i=1}^n p_{ij} * ln (p_{ij})}
#'
#' Where
#'
#' \mjsdeqn{P = \frac{1}{ln(n)}}.
#'
#' Value of E is then in interval of <0;1>.
#'
#' Then we compute degree of divergence:
#'
#' \mjsdeqn{div_j = | 1 - E_j|}
#'
#' and from it entropy weights which are returned as result of the function:
#'
#'\mjsdeqn{Ew_j = \frac{div_j}{\sum_{j=1}^m div_j}}
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
#' @references
#' Kumar, Raman; Bilga, Paramjit Singh Singh, Sehijpal. Multi objective
#'  optimization using different methods of assigning weights to energy
#'  consumption responses, surface roughness and material removal rate during
#'  rough turning operation. Journal of Cleaner Production, vol. 16, pp. 45-57,
#'  DOI: 10.1016/j.jclepro.2017.06.077.
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
    "SDW",
    "SVW",
    "EWM"
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
    SD <- pop_sd(pm, minmax, method)
    w <- SD / sum(SD)
    return(w)
  }

  # SVW - Statistical Variance Weighting Method
  SVW <- function(pm, minmax, method = "minmax") {
    SD <- pop_sd(pm, minmax, method)
    SD2 <- SD^2
    w <- SD2 / sum(SD2)
    return(w)
  }

  # population standard deviation
  pop_sd <- function(pm, minmax, method = "minmax") {
    ncri <- ncol(pm)
    nalt <- nrow(pm)
    pm2 <- pm
    SD <- rep(0, times = ncri)
    p <- sqrt((nalt - 1) / nalt) # const. to transform to population sd
    for (i in 1:ncri) {
      # step 1) normalization
      pm2[, i] <- mcda_norm(pm[, i], minmax = minmax[i], method = method)
      SD[i] <- sd(pm2[, i]) * p # step 2) compute SD
    }
    return(SD)
  }

  # Entropy Weight Method
  EWM <- function(pm) {
    # function computes maximum in columns in matrix or data frame
    col_max <- function(data) {
      t <- apply(data, 2, max)
      return(t)
    }
    nalt <- nrow(pm)
    em <- sweep(pm, 2, col_max(pm), "/") # 1. normalize PM: EM = PM_ij/max(PM_j)
    # sum_{i=1}^n EM_{ij}, where j = criteria, i = alternatives,
    # n = number of alternatives
    p <- sweep(em, 2, colSums(em), "/") # probability of criteria to occur
    p2 <- p * log(p)
    # required since if p = 0, then ln(0) = -inf, so 0 * ln(0) = NaN
    na_values <- is.na(p2)
    p2[na_values] <- 0
    # E_j = -P sum_{i=1}^n p_{ij} * log_e (p_{ij})
    ej <- -(1 / log(nalt)) * colSums(p2)
    divj <- abs(1 - ej) # degree of divergence
    # Entropy weight
    ewj <- divj / sum(divj) # Ew_j = div_j / sum_{j=1}^m div_j
    return(ewj)
  }

  # perform weight computation based on selected method
  result <- switch(
    method,
    "MW" = MW(pm), # Mean Weighting Method
    "SDW" = SDW(pm, minmax, norm), # Standard Deviation Weighting Method
    "SVW" = SVW(pm, minmax, norm), # Statistical Variace Weighting Method
    "EWM" = EWM(pm) # Entropy Weight Method
  )
}