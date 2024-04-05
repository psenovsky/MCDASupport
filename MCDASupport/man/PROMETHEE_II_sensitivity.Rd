\name{PROMETHEE_II_sensitivity}
\alias{PROMETHEE_II_sensitivity}
\title{PROMETHEE II Sensitivity testing}
\description{
Function provides some ways of how to test positive and degative flows computed by \code{\link{PROMETHEE_II}} function.

Function uses privisional +/- 10 \% of set preference (P), indifference (Q) and intermediate (I) thresholds. The changes in net flow is then provided in tabular and graphical form.

Since 10 \% approach is only provisional it provides only example of how to approach sensitivity testing of PROMETHEE II.

Technically the computation procedure for PROMETHEE II is same as for \code{\link{PROMETHEE_I}} with that the RPOMETHEE I does not attempt to integrate the result to provide ranking. PROMETHEE II computes net flow as difference between positive and negative flows and provides ranking.

The sensitivity function does not provide positive and negative flow. If you want test their sensitivity to threshold changes, use PROMETHEE I function, it has same parameters.
}
\usage{
PROMETHEE_II_sensitivity(PM, preferenceFunction, w, minmax, indifferenceTreshold = NULL,
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
	\item{hyp_P}{value of hyperparametr preference threshold in criteria}
	\item{hyp_Q}{value of hyperparametr indifference threshold in criteria}
  \item{hyp_I}{value of hyperparametr intermediate threshold in criteria}
  \item{hyp_P_criteria_netFlow}{net flow in of preference threshold changes in criteria}
  \item{hyp_Q_criteria_netFlow}{net flow in of indifference threshold changes in criteria}
  \item{hyp_I_criteria_netFlow}{net flow in of intermediate threshold changes in criteria}
  \item{hyp_P_criteriaGraph_netFlow}{net flow of preference threshold changes in criteria}
  \item{hyp_Q_criteriaGraph_netFlow}{net flow of indifference threshold changes in criteria}
  \item{hyp_I_criteriaGraph_netFlow}{net flow of intermediate threshold changes in criteria}
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
  result <- PROMETHEE_II_sensitivity(PM, shape, w, minmax, q, p, s)
}
\keyword{ PROMETHEE II }
\keyword{ PROMETHEE I }
\keyword{ PROMETHEE }
\keyword{ sensitivity analysis }
