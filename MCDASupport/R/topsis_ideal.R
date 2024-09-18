#' function to compute ideal and antiedeal solution for TOPSIS and SIR
#'  (internal use only)
#'
#' @description
#' Function to compute ideal and antiideal solution to normalized perfomance
#'  matrix.
#'
#' The function is utilized by \link{sir} and \link{topsis} methods for
#'  this purpose. It incorporates steps 3 - 5 of the TOPSIS computation.
#'  Consult \link{topsis} function's documentation for implementation details.
#'
#' @param pm normalized performance matrix
#' @param inferiority FALSE by defaul to compute ideals as max from pm, TRUE
#'  will minimize ideal and maximize anitiideal
#'
#' @return
#' Returns list with following structure:
#' \itemize{
#'   \item a_ideal - Positive ideal soluation A*
#'   \item a_anti - Anti-ideal solution A-
#'   \item d_ideal - Closenes to ideal variant
#'   \item d_anti - Closeness to anti-ideal variant
#'   \item closenes - Relative closeness of the alternatives to ideal solution
#' }
#'
#' @references
#' ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca
#'  Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
#'
#' Papathanasiou, Jason, Ploskas, Nikolaos. Multiple Criteria Decision Aid
#'  Methods, Examples and Python Implementations. Springer, 173 p., ISBN
#'  978-3-319-91648-4.
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords TOSPIS SIR ideal anti-ideal
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