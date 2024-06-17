# topsis_ideal - function for computation of ideal and antiideal solution
# this function is being utilized by TOPSIS and SIR functions.
#
# function is intended for internal usage only.
#
# parameters:
#   pm - normalized performance matrix
#   inferiority - compute inferiority index with a_ideal and a_anti switched,
#                 FALSE by default
topsis_ideal <- function(pm, inferiority = FALSE) {
  ncri <- ncol(pm)     # no. of criteria
  nalt <- nrow(pm)     # no. of alternatives
  cri  <- colnames(pm) # criteria names
  alt  <- rownames(pm) # alternatives name

  # step 3
  # Determination of the Ideal (A_ideal) and Anti-ideal (A_anti) solutions,
  # as max or minimums in the criteria
  a_ideal <- pm %>%
    group_by() %>%
    summarise(across(1:ncri, max)) %>%
    unlist()
  a_anti <- pm %>%
    group_by() %>%
    summarise(across(1:ncri, min)) %>%
    unlist()
  names(a_ideal) <- cri
  names(a_anti) <- cri

  if (inferiority) {
    # switch ideal and antiideal for inferiority index in SIR
    a_t <- a_anti
    a_anti <- a_ideal
    a_ideal <- a_t
    a_t <- NULL
  }

  # Step 4. Calculation of the Separation Measures (distance from ideal/anti
  # ideal solution)
  d_ideal <- sqrt(sapply(1:nalt, function(i) sum((pm[i, ] - a_ideal)^2)))
  d_anti <- sqrt(sapply(1:nalt, function(i) sum((pm[i, ] - a_anti)^2)))
  names(d_ideal) <- alt
  names(d_anti) <- alt

  # Step 5. Calculation of the Relative Closeness to the Ideal Solution
  # always 0-1, the closer to 1, the better
  closenes <- d_anti / (d_anti + d_ideal)
  names(closenes) <- alt

  t <- list(
    a_ideal = a_ideal,
    a_anti = a_anti,
    d_ideal = d_ideal,
    d_anti = d_anti,
    closenes = closenes
  )
  return(t)
}