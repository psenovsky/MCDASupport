\name{PROMETHEE_II}
\alias{PROMETHEE_II}
\title{PROMETHEE II: method for computations of partial preference using PROMETHEE II method}
\description{
PROMETHEE stands for Preference Ranking Organization Method for Enrichment Evaluation. While \code{\link{PROMETHEE_I}} focuses on computation of flows and establishment of basic outranking relation between the alternatives, PPROMETHEE II takes the proces one step further and provides full ranking of alternatives.

Positive, negative and net flows are computed is same way as in PROMETHEE I, see \code{\link{PROMETHEE}} function, which serves as general implementation of the approach in PROMETHEE I - III implementation in this package.

PROMETHEE II has very straighforward approach to computation of the ranks for alternatives.It takes net flow provided by PROMETHEE function and orders it decresingly. Alternatives with same values do have same rank, othervise we get full order of alternatives.
}
\usage{
PROMETHEE_II(PM, preferenceFunction, w, minmax = 'max', indifferenceTreshold = NULL,
prefferenceThreshold = NULL, intermediateThreshold = NULL, VERBOSE = FALSE)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. only numeric values expercted. Rows and columns are expected to be named.}
  \item{preferenceFunction}{vector, specifies type of function used to compute preferences. Need to be set for each criterion. Possible values are: 'default', 'U-shape', 'V-shape', 'level', 'linear', 'Gaussian'. Choice of function type will decide on what type of threshold (if any) is required for computation. Each criterion can use different preference function.}
  \item{w}{vector containing the weights of the criteria. Values need to 0 <= wi <= 1, sum(wi) = 1}
  \item{minmax}{can be set to either value or vector. Value (min or max) is usable in situation when all criteria are either benefit or cost (are not mixed). If Criteria orientation is mixed, vector is required to set criterion orientation right.}
  \item{indifferenceTreshold}{vector containing indifference threshods for criteria. Not all types of performance functions require it. The parameter must be used if there is at least one criterion, for which it is required. Values for all other criteria should be 0 (and will not be used during computations). Only 'U-shape', 'level', 'linear' functions need this threshold.}
  \item{prefferenceThreshold}{vector containing prefference threshods for criteria. Not all types of performance functions require it. The parameter must be used if there is at least one criterion, for which it is required. Values for all other criteria should be 0 (and will not be used during computations). Only 'V-shape', 'level', 'linear' functions need this threshold.}
  \item{intermediateThreshold}{vector containing intermetiate thresholds for criteria. only Gaussian type performance functions rewuire this type of threshold. If prefference and indifference thresholds are present, the PROMETHEE function will try to 'gues' intermediate threshold as value right in the middle between these thresholds.}
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
	The function returns a list structured as follows:
	\item{positiveFlow}{vector representing how the alternative is preffered to other alternatives}
	\item{negativeFlow}{vector representing how altenative is outranked by other alternatives}
	\item{netFlow}{positive - negative flow, forms an indicator PROMETHEE II uses to directly rank the alternatives}
	\item{preferenceDegree}{matrix representing aggregated weighted preferences of the alternatives across the criteria}
	\item{preferenceDegreeUnw}{list of matrixes with unweighted preferences, separate matrix for each criterion}
	\item{pairweiseComparison}{list of matrixes measuring nominal differences in alternatives performance in criterions. Separate matrixes are constructed for each criterion.}
	\item{rankedList}{ranked list of alternatives}
	\item{rankedListNOF}{sorted list of alternatives with assigned net outranking flow indicator, which can be used to sort the list.}
	\item{preferenceMatrix}{preference matrix describing ourranking relation between the alternatives}
}

\references{
ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.
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
  result <- PROMETHEE_II(PM, shape, w, minmax, q, p, s)
}
\keyword{ PROMETHEE II }
\keyword{ PROMETHEE }

