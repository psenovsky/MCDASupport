# Function for sensitivity analysis for Electre_2 function
#   PM - performance matrix criteria in columns and alternatives in rows
#   w  - weights
#   minmaxcriteria - information on "direction" of the criteria
#                    (min/max, max is default)
#   c-, c0 and c+ fuzzy concordance threshold
#   c_minus, c_zero, c_plus - all in format of c(from, to, step, default)
#   d-, d+ discordance threshold
#   d_minus, d_plus - also in format c(from, to, step, default)
Electre_2_sensitivity <- function(PM, w,
                                  minmaxcriteria = "max",
                                  c_minus = c(0.51, 0.74, 0.01, 0.65),
                                  c_zero = c(0.66, 0.84, 0.01, 0.75),
                                  c_plus = c(0.76, 0.99, 0.01, 0.85),
                                  d_minus = c(0.01, 0.49, 0.01, 0.25),
                                  d_plus = c(0.26, 0.66, 0.01, 0.5)){

  # common consistency check (common with Electre_2 function)
  Electre_2_paramCheck(PM, w)
  # all values of c_ and d_ must be specified as vector in format:
  # c(from, to, step), where from < to.
  if (!(is.vector(c_minus, mode = "numeric")) ||
        !(is.vector(c_zero, mode = "numeric")) ||
        !(is.vector(c_plus, mode = "numeric")) ||
        !(is.vector(d_plus, mode = "numeric")) ||
        !(is.vector(d_minus, mode = "numeric"))) {
    stop("c_* and d_* parameters must be numeric vectors")
  }
  if (length(c_minus) != 4 || length(c_zero) != 4 || length(c_plus) != 4 ||
        length(d_minus) != 4 || length(d_plus) != 4) {
    stop("c_* and d_* must be 4-element vectors")
  }
  if (c_minus[1] > c_minus[2] || c_zero[1] > c_zero[2] ||
        c_plus[1] > c_plus[2] || d_minus[1] > d_minus[2] ||
        d_plus[1] > d_plus[2]) {
    stop("for c_* or d_* parameters progression wrong.")
  }
  if (d_minus[4] >= d_plus[4] || d_plus[4] >= c_minus[4] ||
        c_minus[4] >= c_zero[4] || c_zero[4] >= c_plus[4]) {
    stop("following constrain must hold d- < d+ < c- < c0 < c+")
  }
  # end of consistency check

  # set hyperparameters
  hyp_c_minus <- seq(from = c_minus[1], to = c_minus[2], by = c_minus[3])
  hyp_c_zero  <- seq(from = c_zero[1], to = c_zero[2], by = c_zero[3])
  hyp_c_plus  <- seq(from = c_plus[1], to = c_plus[2], by = c_plus[3])
  hyp_d_minus <- seq(from = d_minus[1], to = d_minus[2], by = d_minus[3])
  hyp_d_plus  <- seq(from = d_plus[1], to = d_plus[2], by = d_plus[3])

  # default values to compare non-changing parameters to
  c_m <- c_minus[4]
  c_z <- c_zero[4]
  c_p <- c_plus[4]
  d_m <- d_minus[4]
  d_p <- d_plus[4]

  # sensitivity of parameter c-
  t <- Electre_2(PM, w, minmaxcriteria, hyp_c_minus[1],
                 c_z, c_p, d_m, d_p, FALSE)
  hyp_c_minus_result <- data.frame(as.list(t$finalPreorder),
                                   stringsAsFactors = FALSE)
  i <- 2
  while (hyp_c_minus[i] < c_z && i < length(hyp_c_minus)) {
    t <- Electre_2(PM, w, minmaxcriteria, hyp_c_minus[i],
                   c_z, c_p, d_m, d_p, FALSE)
    fin_pre <- data.frame(as.list(t$finalPreorder), stringsAsFactors = FALSE)
    hyp_c_minus_result <- rbind(hyp_c_minus_result, fin_pre)
    i <- i + 1
  }
  hyp_c_minus_graph <- rankGraph(hyp_c_minus_result, hyp_c_minus, "c-")

  # sensitivity of the parameter c0
  t <- Electre_2(PM, w, minmaxcriteria, c_m, hyp_c_zero[1],
                 c_p, d_m, d_p, FALSE)
  hyp_c_zero_result <- data.frame(as.list(t$finalPreorder),
                                  stringsAsFactors = FALSE)
  i <- 2
  while (hyp_c_zero[i] < c_p && i < length(hyp_c_zero)) {
    t <- Electre_2(PM, w, minmaxcriteria, c_m, hyp_c_zero[i],
                   c_p, d_m, d_p, FALSE)
    fin_pre <- data.frame(as.list(t$finalPreorder), stringsAsFactors = FALSE)
    hyp_c_zero_result <- rbind(hyp_c_zero_result, fin_pre)
    i <- i + 1
  }
  hyp_c_zero_graph <- rankGraph(hyp_c_zero_result, hyp_c_zero, "c0")

  # sensitivity of the parameter c+
  t <- Electre_2(PM, w, minmaxcriteria, c_m, c_z, hyp_c_plus[1],
                 d_m, d_p, FALSE)
  hyp_c_plus_result <- data.frame(as.list(t$finalPreorder),
                                  stringsAsFactors = FALSE)
  i <- 2
  while (i < length(hyp_c_plus)) {
    #for(i in 1:length(hyp_c_plus)) {
    t <- Electre_2(PM, w, minmaxcriteria, c_m, c_z, hyp_c_plus[i],
                   d_m, d_p, FALSE)
    fin_pre <- data.frame(as.list(t$finalPreorder), stringsAsFactors = FALSE)
    hyp_c_plus_result <- rbind(hyp_c_plus_result, fin_pre)
    i <- i + 1
  }
  hyp_c_plus_graph <- rankGraph(hyp_c_plus_result, hyp_c_plus, "c+")

  # sensitivity of the parameter d-
  t <- Electre_2(PM, w, minmaxcriteria, c_m, c_z, c_p, hyp_d_minus[1],
                 d_p, FALSE)
  hyp_d_minus_result <- data.frame(as.list(t$finalPreorder),
                                   stringsAsFactors = FALSE)
  i <- 2
  while (hyp_d_minus[i] < d_p && i < length(hyp_d_minus)) {
    t <- Electre_2(PM, w, minmaxcriteria, c_m, c_z, c_p, hyp_d_minus[i],
                   d_p, FALSE)
    fin_pre <- data.frame(as.list(t$finalPreorder), stringsAsFactors = FALSE)
    hyp_d_minus_result <- rbind(hyp_d_minus_result, fin_pre)
    i <- i + 1
  }
  hyp_d_minus_graph <- rankGraph(hyp_d_minus_result, hyp_d_minus, "d-")

  # sensitivity of the parameter d+
  t <- Electre_2(PM, w, minmaxcriteria, c_m, c_z, c_p, d_m,
                 hyp_d_plus[1], FALSE)
  hyp_d_plus_result <- data.frame(as.list(t$finalPreorder),
                                  stringsAsFactors = FALSE)
  i <- 2
  while (hyp_d_plus[i] < d_p && i < length(hyp_d_plus)) {
    t <- Electre_2(PM, w, minmaxcriteria, c_m, c_z, c_p, d_m, 
                   hyp_d_plus[i], FALSE)
    fin_pre <- data.frame(as.list(t$finalPreorder), stringsAsFactors = FALSE)
    hyp_d_plus_result <- rbind(hyp_d_plus_result, fin_pre)
    i <- i + 1
  }
  hyp_d_plus_graph <- rankGraph(hyp_d_plus_result, hyp_d_minus, "d-")

  # prepare results
  out <- list(
    c_minus_sensitivity = hyp_c_minus_result,
    c_zero_sensitivity = hyp_c_zero_result,
    c_plus_sensitivity = hyp_c_plus_result,
    d_minus_sensitivity = hyp_d_minus_result,
    d_plus_sensitivity = hyp_d_plus_result,
    c_minus_graph = hyp_c_minus_graph,
    c_zero_graph = hyp_c_zero_graph,
    c_plus_graph = hyp_c_plus_graph,
    d_minus_graph = hyp_d_minus_graph,
    d_plus_graph = hyp_d_plus_graph,
    c_minus = hyp_c_minus[1:nrow(hyp_c_minus_result)], # nolint: seq_linter.
    c_zero = hyp_c_zero[1:nrow(hyp_c_zero_result)], # nolint: seq_linter.
    c_plus = hyp_c_plus[1:nrow(hyp_c_plus_result)], # nolint: seq_linter.
    d_minus = hyp_d_minus[1:nrow(hyp_d_minus_result)], # nolint: seq_linter.
    d_plus = hyp_d_plus[1:nrow(hyp_d_plus_result)] # nolint: seq_linter.
  )
  return(out)

}
