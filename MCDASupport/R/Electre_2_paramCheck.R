# Check parameters of the function to find possible problems and stop execution of the run if problem is found
#
# parameters
#   PM - performance matrix criteria in columns and alternatives in rows
#   w  - weights
Electre_2_paramCheck <- function(PM, w){

  # general consistency checks, this is directly copied from Electre_2. Intention is to let fail this function immediately instead of waiting for it to fail when Electre_2 method is called
  # in the sensitivity analysis run
  if (is.null(dim(PM))) stop('less than 2 criteria or 2 alternatives')
  if (!(is.matrix(PM) || (is.data.frame(PM)))) stop('wrong performanceMatrix, should be a matrix or a data frame')
  if (!is.numeric(unlist(PM))) stop('Only numeric values in performance matrix expected')
  if (!(is.vector(w))) stop('criteriaWeights should be a vector')
  if (!is.numeric(w)) stop('criteriaWeights should be a numeric vector')
  if (ncol(PM)!=length(w)) stop('length of criteriaWeights should be checked')

}
