\name{promethee_param_check}
\alias{promethee_param_check}
\title{promethee_param_check : internal function to check parameters for PROMETHEE and SIR functions}
\description{
  Performs parameter checking for PREMETHEE and SIR functions. While checking it also evaluates value of the intermediate threshold if not set as a parameter, based on other two thresholds. The computed value of the threshold is then returned.

  This function is intended for package's internal use only.

  If some problem with the parameters is detected, the function stops to execute with the error message on the reason.
}
\usage{
promethee_param_check(PM, preferenceFunction, w, indifferenceTreshold = NULL,
prefferenceThreshold = NULL, intermediateThreshold = NULL)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. only numeric values expercted. Rows and columns are expected to be named.}
  \item{preferenceFunction}{vector, specifies type of function used to compute preferences. Need to be set for each criterion. Possible values are: 'default', 'U-shape', 'V-shape', 'level', 'linear', 'Gaussian'. Choice of function type will decide on what type of threshold (if any) is required for computation. Each criterion can use different preference function.}
  \item{w}{vector containing the weights of the criteria. Values need to 0 <= wi <= 1, sum(wi) = 1}
  \item{indifferenceTreshold}{vector containing indifference threshods for criteria. Not all types of performance functions require it. The parameter must be used if there is at least one criterion, for which it is required. Values for all other criteria should be 0 (and will not be used during computations). Only 'U-shape', 'level', 'linear' functions need this threshold.}
  \item{prefferenceThreshold}{vector containing prefference threshods for criteria. Not all types of performance functions require it. The parameter must be used if there is at least one criterion, for which it is required. Values for all other criteria should be 0 (and will not be used during computations). Only 'V-shape', 'level', 'linear' functions need this threshold.}
  \item{intermediateThreshold}{vector containing intermetiate thresholds for criteria. only Gaussian type performance functions rewuire this type of threshold. If prefference and indifference thresholds are present, the PROMETHEE function will try to 'gues' intermediate threshold as value right in the middle between these thresholds.}
}

\value{
	The function returns recomputed value of intermediate threshold if no error in parameter checking logic is detected.
}

\references{
ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ PROMETHEE }
\keyword{ PROMETHEE I }
\keyword{ PROMETHEE II }
\keyword{ PROMETHEE III }
\keyword{ SIR }