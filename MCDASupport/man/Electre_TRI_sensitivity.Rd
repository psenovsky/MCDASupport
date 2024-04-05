\name{Electre_TRI_sensitivity}
\alias{Electre_TRI_sensitivity}
\title{ELECTRE TRI Method Sensitivity testing}
\description{The \code{\link{Electre_TRI}} is a multiple criteria decision aiding method, designed to deal with sorting problems. Sensitivity is tested against posible changes of thresholds.

At present time only simplified sensitivity testing is being tested - meaning 10 \% (+/-) for given threshold value, which is less then ideal especially for lower values of threshold. 10 \% in such case can be very close to original value, which goes against the purpose of the sensitivity analysis. I. e. 0.1 +/- 10 \% is <0.09;0.11> with step 0.001.

Sensitivity analyses thus shold be considered as experimental feature to be rewritten in the future.


Sensitivity analysis also doesn't test for changes in lambda parameter.

}
\usage{
Electre_TRI_sensitivity(PM, profiles, profiles_names,
  w, Q, P, V,
  minmaxcriteria = 'max', lambda=0.75)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
  \item{profiles}{Matrix containing, in each row, the lower profiles of the categories. The columns are named according to the criteria, and the rows are named according to the categories. The index of the row in the matrix corresponds to the rank of the category.}
  \item{profiles_names}{Vector containing profiles'names}
  \item{w}{Vector containing the weights of the criteria.}
  \item{Q}{Vector containing the indifference thresholds constraints defined for each criterion.}
  \item{P}{Vector containing the preference thresholds constraints defined for each criterion.}
  \item{V}{Vector containing the veto thresholds constraints defined for each criterion}
  \item{minmaxcriteria}{Vector containing the preference direction on each of the criteria. "min" (resp."max") indicates that the criterion has to be minimized (maximized). If all criteria are directed in same way use single min or max instead (the method prepars required parameters on its own)}
  \item{lambda}{Lambda parameter is used as cut-off criterion for outranking evaluation. Should be in range of 0.5 and 1.0. Default value=0.75}
}

\value{
	The function returns a list structured as follows:
  \item{hyp_P}{values of preference thresholds in criteria}
  \item{hyp_Q}{values of indifference thresholds in criteria}
  \item{hyp_V}{values of veto threshold in criteria}
  \item{hyp_P_criteria_optimistic}{results of sensitivity analysis of preference thresholds in criteria using optimistic ranking procedure}
  \item{hyp_Q_criteria_optimistic}{results of sensitivity analysis of indifference thresholds in criteria using optimistic ranking procedure}
  \item{hyp_V_criteria_optimistic}{results of sensitivity analysis of veto thresholds in criteria using optimistic ranking procedure}
  \item{hyp_P_criteria_pesimistic}{results of sensitivity analysis of preference thresholds in criteria using pesimistic ranking procedure}
  \item{hyp_Q_criteria_pesimistic}{results of sensitivity analysis of indifference thresholds in criteria using pesimistic ranking procedure}
  \item{hyp_V_criteria_pesimistic}{results of sensitivity analysis of veto thresholds in criteria using pesimistic ranking procedure}
  \item{hyp_P_criteriaGraph_optimistic}{results in graphical form of preference thresholds in criteria using optimistic ranking procedure}
  \item{hyp_Q_criteriaGraph_optimistic}{results in graphical form of indifference thresholds in criteria using optimistic ranking procedure}
  \item{hyp_V_criteriaGraph_optimistic}{results in graphical form of veto thresholds in criteria using optimistic ranking procedure}
  \item{hyp_P_criteriaGraph_pesimistic}{results in graphical form of preference thresholds in criteria using pesimistic ranking procedure}
  \item{hyp_Q_criteriaGraph_pesimistic}{results in graphical form of indifference thresholds in criteria using pesimistic ranking procedure}
  \item{hyp_V_criteriaGraph_pesimistic}{results in graphical form of veto thresholds in criteria using pesimistic ranking procedure}
}

\references{
Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7

FIGUEIRA, J.R., Greco, S., Roy, B., Slowinski, R. ELECTRE Methods : Main Features and Recent Developments. Laboratoire d'Analyse et Modélisation de Systèmes pour l'Aide à la Décision, Paris: 2010, 34 pp., available from \url{https://hal.archives-ouvertes.fr/hal-00876980/document}.

Prombo, M. Package OutrankingTools, CRAN: 2015, available from: \url{https://cran.r-project.org/web/packages/OutrankingTools/}

Shofade, Olanrewaju Joseph Soniran. Considering hierarchical structure of criteria in ELECTRE decision aiding methods. Universitat Rovira i Virgili, Escola Tecnica Superior d'Enginyeria, Tarragona: 2011 \url{https://deim.urv.cat/~itaka/itaka2/PDF/acabats/ThesisJoseph-ELECTRE-H.pdf}
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
PM <- cbind(
  c(-120.0,-150.0,-100.0,-60,-30.0,-80,-45.0),
  c(-284.0,-269.0,-413.0,-596,-1321.0,-734,-982.0),
  c(5.0,2.0,4.0,6,8.0,5,7.0),
  c(3.5,4.5,5.5,8,7.5,4,8.5),
  c(18.0,24.0,17.0,20,16.0,21,13.0)
)
# names of alternatives
rownames(PM) <- c("a1","a2","a3","a4","a5","a6","a7")
# names of criteria
colnames(PM) <- c( "g1","g2","g3","g4","g5")
w <- c(0.25,0.45,0.10,0.12,0.08) # criteria weights
# all criteria maxed - ommiting minmaxcriteria parameter
# lambda = 0.75 - ommiting lambda parameter
# Matrix containing the profiles.
profiles <- cbind(c(-100,-50), c(-1000,-500),
  c(4,7),c(4,7),c(15,20))
profiles_names <-c("b1","b2")
# thresholds vectors
I <- c(15,80,1,0.5,1) # indifference threshold
P <- c(40,350,3,3.5,5) # prefference threshold
V <- c(100,850,5,4.5,8) # veto threshold
t <- Electre_TRI_sensitivity(PM,
                             profiles, profiles_names,
                             w, I, P, V)
t$hyp_P_criteriaGraph_optimistic[1]
}

\keyword{ELECTRE methods}
\keyword{ELECTRE TRI}
\keyword{Sorting problem}
\keyword{sensitivity analysis}
\keyword{Aggregation/disaggregation approaches}
\keyword{Multi-criteria decision aiding}

