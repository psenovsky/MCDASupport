\name{Electre_3_sensitivity}
\alias{Electre_3_sensitivity}
\title{
Electre_3_sensitivity: function for performing sensitivity analysis for Electre III method
}
\description{
Initial implementation. Right now it test basic sensitivity of the solution to changes of preference (P), indifference (Q) and veto (V) thresholds. At present time it takes basic (set) value of P, Q, V and creates creates search space for these +/- 10 \% of the set value and the searched for values are then space by 1 \%. That appproach is far from optimal, as it would be better to let the user set desired space and step.

Unfortunatelly that would require using P/Q/V parameters as matrixes or data frames, which would typically not be easiest function signature to use. Especially considering that the \code{\link{Electre_3}} implementation is far from optimal to performance wise. The thresholds are defined specified for method per criterium, which means, in present state, that the Electre_3 function is computed 63x per criterium, making it 315 for simple example specified in this documentation.
}
\usage{
Electre_3_sensitivity(PM, w, P, Q, V, minmaxcriteria = "max", alpha = 0.3,
  beta = 0.15, VERBOSE = FALSE)
}
\arguments{
  \item{PM}{  performance of the alternatives in criteria. Alternatives are in rows, criteria in columns.}
  \item{w}{  weight of the criteria }
  \item{P}{  performance thresholds. Set in natural unit of the criterium, vestor - must be set for every criterium}
  \item{Q}{  indifference thresholds. Set in natural unit of the criterium, vestor - must be set for every criterium}
  \item{V}{  veto thresholds. Set in natural unit of the criterium, vestor - must be set for every criterium}
  \item{minmaxcriteria}{  vector specifying direction of the criterium (min/max). Use single min or max if all criteria have same direction}
  \item{alpha}{  first discrimination threshold}
  \item{beta}{  second discrimination threshold}
  \item{VERBOSE}{  boolean value to set VERBOSE mode of the Electre_3 function. Recommended to leave FALSE (default value) for sensitivity analysis, as the function can be really talkative.}
}

\value{
returns list of
  \item{hyp_P}{list of derived hyperparameters P used in sensitivity analysis}
  \item{hyp_Q}{list of derived hyperparameters Q used in sensitivity analysis}
  \item{hyp_V}{list of derived hyperparameters V used in sensitivity analysis}
  \item{hyp_P_criteria}{computed ranks for alternatives with P (as per hyp_P) changing}
  \item{hyp_Q_criteria}{computed ranks for alternatives with Q (as per hyp_Q) changing}
  \item{hyp_V_criteria}{computed ranks for alternatives with V (as per hyp_V) changing}
  \item{hyp_P_criteriaGraph}{visualization of hyp_P_criteria using rankGraph}
  \item{hyp_Q_criteriaGraph}{visualization of hyp_Q_criteria using rankGraph}
  \item{hyp_V_criteriaGraph}{visualization of hyp_V_criteria using rankGraph}
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

t <- Electre_3_sensitivity(PM, w, P, Q, V, minmaxcriteria)
t$hyp_P_criteriaGraph[[1]]
t$hyp_P_criteriaGraph[[5]]
}
\keyword{ ELECTRE III }
\keyword{ sensitivity analysis }
