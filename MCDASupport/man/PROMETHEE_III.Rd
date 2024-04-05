\name{PROMETHEE_III}
\alias{PROMETHEE_III}
\title{PROMETHEE III: method for computations of partial preference using PROMETHEE III method}
\description{
PROMETHEE stands for Preference ranking organization method for enrichment evaluation. Promethee II method is intended for establishment of full ranking of the alternatives, by evaluation of positive and negative prefference flows in pairweise comparisons of the alternatives.

Difference between positive and negative flows forms net outranking flow. PROMETHEE III function is usable for partial preorder, similarly to PROMETHEE I function, but uses intervals for establishing preorder.

Method starts with quantitatively expressed performance of the alternatives in the criteria. Positive, negative and net flows is computed using \code{\link{PROMETHEE}} function (see PROMETHEE function documentation for details on computation), in same way as in \code{\link{PROMETHEE_I}} and \code{\link{PROMETHEE_II}}. The difference is in what the method does with the  results next.

\mjsdeqn{s_{err} = \frac{sd_{net}}{\sqrt{n}}}

Where \mjseqn{s_{err}} ... standard error, \mjseqn{sd_{net}} standard deviation of net flow, n ... number of alternatives.

For each alternative we then specify x and y limit computed as:

\mjsdeqn{x_{lim} = net_i - s_{err}}

\mjsdeqn{y_{lim} = net_i + s_{err}}

Where x limit is lover limit of of the interval and y is the upper limit, \mjseqn{s_{err}} ... is standard error.

Partial preference is then  established based on comparing lower and upper limits of the alternatives.

If \mjseqn{x_{lim}(a) > y_{lim}(b)} then alternative is prefered to b: aPb. If \mjseqn{x_{lim}(a) == y_{lim}(b)} than we are clearly indifferent to the alternatives: aIb. Otherwise alternative b is prefered to a.

Method does not provide ranking.

Limit comparison can be seen as an alternative implementation of thresholds as the performance must exceed some limit for the alternative to be considered prefered.

Note: the implementation of function is partially inspired by implementation of PROMETHEE III portion of promethee123 package, thou this implementation provides different output and utilizes general PROMETHEE function developed for PROMETHEE I and II functions.
}
\usage{
PROMETHEE_III(PM, preferenceFunction, w, minmax = 'max', indifferenceTreshold = NULL,
prefferenceThreshold = NULL, intermediateThreshold = NULL)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. only numeric values expercted. Rows and columns are expected to be named.}
  \item{preferenceFunction}{vector, specifies type of function used to compute preferences. Need to be set for each criterion. Possible values are: 'default', 'U-shape', 'V-shape', 'level', 'linear', 'Gaussian'. Choice of function type will decide on what type of threshold (if any) is required for computation. Each criterion can use different preference function.}
  \item{w}{vector containing the weights of the criteria. Values need to 0 <= wi <= 1, sum(wi) = 1}
  \item{minmax}{can be set to either value or vector. Value (min or max) is usable in situation when all criteria are either benefit or cost (are not mixed). If Criteria orientation is mixed, vector is required to set criterion orientation right.}
  \item{indifferenceTreshold}{vector containing indifference threshods for criteria. Not all types of performance functions require it. The parameter must be used if there is at least one criterion, for which it is required. Values for all other criteria should be 0 (and will not be used during computations). Only 'U-shape', 'level', 'linear' functions need this threshold.}
  \item{prefferenceThreshold}{vector containing prefference threshods for criteria. Not all types of performance functions require it. The parameter must be used if there is at least one criterion, for which it is required. Values for all other criteria should be 0 (and will not be used during computations). Only 'V-shape', 'level', 'linear' functions need this threshold.}
  \item{intermediateThreshold}{vector containing intermetiate thresholds for criteria. only Gaussian type performance functions rewuire this type of threshold. If prefference and indifference thresholds are present, the PROMETHEE function will try to 'gues' intermediate threshold as value right in the middle between these thresholds.}
}

\value{
	The function returns a list structured as follows:
	\item{positiveFlow}{vector representing how the alternative is preffered to other alternatives}
	\item{negativeFlow}{vector representing how altenative is outranked by other alternatives}
	\item{netFlow}{positive - negative flow, forms an indicator PROMETHEE II uses to directly rank the alternatives}
	\item{preferenceDegree}{matrix representing aggregated weighted preferences of the alternatives across the criteria}
	\item{preferenceDegreeUnw}{list of matrixes with unweighted preferences, separate matrix for each criterion}
	\item{pairweiseComparison}{list of matrixes measuring nominal differences in alternatives performance in criterions. Separate matrixes are constructed for each criterion.}
	\item{preferenceMatrix}{preference matrix describing ourranking relation between the alternatives}
}

\references{
ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.

BRANS, Jean-Pierre; DE SMET, Yves. PROMETHEE methods. In: Multiple criteria decision analysis. Springer, New York, NY, 2016. p. 187-219. DOI: 10.1007/978-1-4939-3094-4_6.

Moreira, M.A.L., dos Santos, M., Gomes, C.F.S. promethee123 package. <https://cran.r-project.org/web/packages/promethee123/index.html>
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  #Example from Fuzzy TOPSIS book (see references)
  #ammended error in tab. 4.9, the computation presumes maximization of all criteria
  PM <- cbind(
    c(80, 65, 83, 40, 52, 94),
    c(90, 58, 60, 80, 72, 96),
    c(600, 200, 400, 1000, 600, 700),
    c(54, 97, 72, 75, 20, 36),
    c(8, 1, 4, 7, 3, 5),
    c(5, 1, 7, 10, 8, 6)
  )
  colnames(PM) <- c('C1', 'C2', 'C3', 'C4', 'C5', 'C6')
  rownames(PM) <- c('A1', 'A2', 'A3', 'A4', 'A5', 'A6')
  minmax <- 'max'
  shape <- c('U-shape', 'V-shape', 'linear', 'level', 'default', 'Gaussian')
  q <- c(10, 0, 450, 50, 0, 0) #indifference threshold
  p <- c(0, 30, 50, 10, 0, 0) #prefference threshold
  s <- c(0,0,0,0,0,5) #intermediate threshold
  w <- c(0.1667, 0.1667, 0.1667, 0.1667, 0.1667, 0.1665)
  result <- PROMETHEE_III(PM, shape, w, minmax, q, p, s)
}
\keyword{ PROMETHEE III }
\keyword{ PROMETHEE }

