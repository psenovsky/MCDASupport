#' comparest two vectors element wise
#'
#' @description
#' `vector_compare` compares two vectors on per element basis. The function
#'  is intended to be internaly used by various sensitivity evaluation
#'  functions, which presumes, that provided vectors do have same structure,
#'  are ordered is same way (or more specifically are not ordered).
#'
#' @param vector1 first vector to compare
#' @param vector2 second vector to compare
#'
#' @returns TRUE if all elements of the vector are same, otherwise
#' returns FALSE
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @keywords vector comparison
#' @keywords ELECTRE III
#' @keywords ELECTRE II
#'
#' @examples
#' a <- b <- c(1, 2, 3, 4, 5)
#' c <- c(2, 2, 3, 4, 5)
#' d <- c(1, 2, 3)
#' vector_compare(a, b) # TRUE
#' vector_compare(a, c) # FALSE
#' vector_compare(a, d) # error
vector_compare <- function(vector1, vector2) {
  if (length(vector1) != length(vector2)) {
    stop("both vectors expected to be same length.")
  }
  for (i in seq_along(vector1)) {
    if (vector1[i] != vector2[i]) return(FALSE)
  }
  return(TRUE)
}