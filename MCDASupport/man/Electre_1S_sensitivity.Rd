\name{Electre_1S_sensitivity}
\alias{Electre_1S_sensitivity}
\title{
Electre_1S_sensitivity: function for performing sensitivity analysis for Electre 1S method
}
\description{
ELECTRE 1S method computes only so called kernel of the solution (in comparison with \code{\link{Electre_4}}, which provides also ranking for the alternatives). Sensitivity analysis must provide different results, than other analyzes. It returns dataframe with 0/1 values for the alternatives, then specifies threshold P/Q/V whose sensitivity we are testing, the criterion for which we are testing it and actual threshold value.

For example one row of resulting table could look like this:

1;1;1;1;1;P;CR5;18

First 5 columns would represent whether the alternative is present in the kernel, P signals that preference threshold was tested, CR5 in criterion 5 (CR5 is its name) and in last column we see actual value of the P (for criterion P). Akk ither parameters - Q, V and P for other criteria remain in their default values.

For purposes of computation of the sensitivity approach similar to \code{\link{Electre_4}} and \code{\link{Electre_3}} has been used, so the function takes value of P, Q, V and creates search space for these +/- 10 \% of the set value and the searched for values are then space by 1 \%. That appproach is far from optimal, as it would be better to let the user set desired space and step.

Unfortunatelly that would require using P/Q/V parameters as matrixes or data frames, which would typically not be easiest function signature to use. Especially considering that the \code{\link{Electre_4}} implementation is far from optimal performance wise. The thresholds are defined specified for method per criterium, which means, in present state, that the Electre_4 function is computed 63x per criterium, making it 315 for simple example specified in this documentation.

The resulting table should be filtered using dplyr's filter dunction for type of the threshold and criterium and ordered by threshold value, to see how the results change.
}
\usage{
Electre_1S_sensitivity(PM, w, Q, P, V, minmaxcriteria = "max", lambda = 0.5)
}
\arguments{
  \item{PM}{  performance of the alternatives in criteria. Alternatives are in rows, criteria in columns.}
  \item{w}{ weights of the criteria }
  \item{Q}{  indifference thresholds. Set in natural unit of the criterium, vestor - must be set for every criterium}
  \item{P}{  performance thresholds. Set in natural unit of the criterium, vestor - must be set for every criterium}
  \item{V}{  veto thresholds. Set in natural unit of the criterium, vestor - must be set for every criterium}
  \item{minmaxcriteria}{  vector specifying direction of the criterium (min/max). Use single min or max if all criteria have same direction}
  \item{lambda}{ cut-off criterion }
}

\value{
  data frame with information on alternatives being part of the kernel or not.
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
PM <- cbind(
  c(-14,129,-10,44,-14),
  c(90,100,50,90,100),
  c(0,0,0,0,0),
  c(40,0,10,5,20),
  c(100,0,100,20,40)
)
rownames(PM) <- c("Project1","Project2","Project3","Project4","Project5")
colnames(PM) <- c( "CR1","CR2","CR3","CR4","CR5")
minmaxcriteria <- 'max'
Q <- c(25,16,0,12,10) #Indifference thresholds
P <- c(50,24,1,24,20) #Preference thresholds
V <- c(100,60,2,48,90) #Veto thresholds
w <- c(1,1,1,1,1) #weights
t <- Electre_1S_sensitivity(PM, w, Q, P, V, minmaxcriteria)
}
\keyword{ ELECTRE 1S }
\keyword{ sensitivity analysis }
\keyword{ kernel }
