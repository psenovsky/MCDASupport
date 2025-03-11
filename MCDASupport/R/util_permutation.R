#' a function to generate all possible permutations
#'
#' @description
#' of values provided in vec vector
#'
#' @param vec vector to derive the permutations
#'
#' @return all permutations of vec values
#'
#' @author Pavel Šenovský \email{pavel.senovsky@vsb.cz}
#'
#' @examples
#' alternatives <- c("A", "B", "C")
#' permutations <- util_permutation(alternatives)
util_permutation <- function(vec) {
  if (length(vec) == 1) {
    return(list(vec))
  } else {
    perms <- list()
    for (i in seq_along(vec)) {
      sub_vec <- vec[-i]
      sub_perms <- util_permutation(sub_vec)
      for (perm in sub_perms) {
        perms <- c(perms, list(c(vec[i], perm)))
      }
    }
    return(perms)
  }
}
