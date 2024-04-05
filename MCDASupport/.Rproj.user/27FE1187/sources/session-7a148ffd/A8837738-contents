\name{Electre_TRI}
\alias{Electre_TRI}
\title{ELECTRE TRI Method}
\description{The Electre Tri is a multiple criteria decision aiding method, designed to deal with sorting problems. Electre Tri method has been developed by LAMSADE (Paris-Dauphine University, Paris, France).

The method itself is very interesting as it does not directly compare alternatives against each other. The evaluation is performed against the "profile". The profile presents scale in which criterion exists and is divided in the categories. Since we presume imperfect information we are basically evaluating in which category the performance of the alternative in criterion is.

These categories are then aggregated across the criteria to form rank using "pessimistic" or "optimistic" agregation procedure. Results of these procedures is then used to form ranking.

As oposed to ELECTRE IV, ELECTRE TRI does not provide guidance to establish "final" ranking by agregating outcomes of optimistic and pesimistic procedures.

The procedure itself works with alternatives a, and performance profiles (\mjseqn{b_h}. Partial concordance matrix is derived from comparison between performance profile and preference or indifference thresholds for the criteria.

\mjsdeqn{c_j(a,b_h) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(a) \ge PM_j(b_h) - Q_j \cr 0 & \;if\; PM_j(a) < PM_j(b_h) - P_j \cr \frac{PM_j(a) + P_j - PM_j(b_h)}{P_j - Q_j} & \;otherwise\;\end{array}\right.}

Where P ... preference threshold, Q ... indifference threshold, \mjseqn{PM_j(a)} ... performance of alternative in criterium j, \mjseqn{PM_j(b_h)} ... performance profile in criterium j.

Comprehensive concordance index is then computed by aggregating weighted concordance indexes:

\mjseqn{c(a, b_h) = \sum_{j \in J} w_j c_j(a, b_h)}

Similarly discordance index for the criterie is derived from comparing alternative's performance against performace profiles with applied veto or preference thresholds:

\mjsdeqn{d_j(a,b_h) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(a) < PM_j(b_h) - V_j \cr 0 & \;if\; PM_j(a) \ge PM_j(b_h) - P_j \cr \frac{PM_j(b_h) - P_j - PM_j(b_h)}{V_j - P_j} & \;otherwise\;\end{array}\right.}

To construct preference relations, credibility of the outranking relation (aSbh - a is at least as good as bh) needs to be computed. First credibility index \mjseqn{\rho} needs to be constructed.

\mjsdeqn{\rho(a,b_h) = \left\lbrace\begin{array}{ll} c(a,b_h) \prod_{j \in V} \frac{1 - d_j(a,b_h)}{1 - c(a, b_h)} & \;if\; V \ne \;NULL\; \cr c(a, b_h) & \;otherwise\;\end{array}\right.}

Where \mjseqn{V = \{ j \in J: d_j(a, b_h) > c(a, b_h)\}}.

Based on it we can explore outranking relations by comparing the index to cut-off criterium \mjseqn{\lambda}. \mjseqn{\lambda} should be in interval of <0,5; 1>, value \mjseqn{\lambda = 0,75} is used as default.

\itemize{
  \item \mjseqn{\rho(a,b_h) \ge \lambda} and \mjseqn{\rho(b_h, a) \ge \lambda}: a is indifferent to bh
  \item \mjseqn{\rho(a,b_h) \ge \lambda} and \mjseqn{\rho(b_h, a) < \lambda}: a is preferred to bh (weakly or strongly) - aPbh
  \item \mjseqn{\rho(a,b_h) < \lambda} and \mjseqn{\rho(b_h,a) \ge \lambda}: bh is preferred to a (weakly or strongly)
  \item \mjseqn{\rho(a,b_h) < \lambda} and \mjseqn{\rho(b_h,a) < \lambda}: a is incomparable to bh
}

This information is then used to derive the ranking using pessimistic and optimistic procedure.

In Pessimistic procedure the alternative is assigned best category for which aPbh. The categories for profiles are evaluated in order \mjseqn{b_k, b_{k-1}, ..., b_0}.

Optimistic procedure evaluates in inversed order \mjseqn{b_0, b_1, ..., b_{k-1}, b_k}. Alternative is assigned warst category (first for which bhPa).

Overral agregation procedure is not provided by the method.


}
\usage{
Electre_TRI(PM, profiles, profiles_names,
  w, Q, P, V,
  minmaxcriteria = 'max', lambda=0.75,
  VERBOSE = FALSE)
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
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
	The function returns a list structured as follows:
	\item{performanceMatrix}{A matrix containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Criterion direction is recomputed to represent maximize direction.}
	\item{partialConcordanceIndex}{Partial concordance index indicates the relative dominance of one option over
profile aSbh where bh is profile value.Partial concordance index is being constructed separately for each criterion.}
  \item{partialConcordanceIndexInverse}{Same as partialConcordanceIndex but evaluates bhSa: dominance of profile value over performance of the alternative in criterion.}
  \item{overalConcordanceIndex}{Consolidates information from partial concordance indexes into single matrix describing dominance of the alternative over the profile aSbh across the criteria}
  \item{overallConcordanceIndexInverse}{same as overallConcordanceIndex, but describes bhSa instead, again across all criteria}
  \item{discordanceIndex}{discordance index is oposite to concordance index. It is a value (matrix of values) used to establish that a!Sbh (a does not dominate bh). Discordance index is being computed separately for each criterion.}
  \item{discordanceIndexInverse}{Same as discordanceIndex but helps evaluate bhSa.}
  \item{credibilityIndex}{represents a degree of credibility of the assertion that a outranks bh (aSbh).}
  \item{credibilityIndexInverse}{represents a degree of credibility of the assertion that bh outranks a (bhSa).}
  \item{preferenceRelation}{determination of preference situation between alternatives (aSbh = ">", bhSa = "<", I = indifferent, R = incomparable).}
  \item{pesimistic}{The direction of the ranking obtained from the pesimistic procedure is from best to worst.}
  \item{pesimisticUnsorted}{ranked results of pesimistic ranking procedure unsorted (sorted by alternatives)}
  \item{optimistic}{the direction of the ranking obtained from optimistic procesure goes from worst to best.}
  \item{optimisticUnsorted}{ranked results of optimistic ranking procedure unsorted (sorted by alternatives)}
}

\references{
Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7

FIGUEIRA, J.R., Greco, S., Roy, B., Slowinski, R. ELECTRE Methods : Main Features and Recent Developments. Laboratoire d'Analyse et Modélisation de Systèmes pour l'Aide à la Décision, Paris: 2010, 34 pp., available from \url{https://hal.archives-ouvertes.fr/hal-00876980/document}.

Prombo, M. Package OutrankingTools, CRAN: 2015, available from: \url{https://cran.r-project.org/web/packages/OutrankingTools/}

Shofade, Olanrewaju Joseph Soniran. Considering hierarchical structure of criteria in ELECTRE decision aiding methods. Universitat Rovira i Virgili, Escola Tecnica Superior d'Enginyeria, Tarragona: 2011 \url{https://deim.urv.cat/~itaka/itaka2/PDF/acabats/ThesisJoseph-ELECTRE-H.pdf}
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
# the performance table
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
#  vector defining profiles' names
profiles_names <-c("b1","b2")
# thresholds vector
I <- c(15,80,1,0.5,1) # indifference threshold
P <- c(40,350,3,3.5,5) # prefference threshold
V <- c(100,850,5,4.5,8) # veto threshold
Electre_TRI(PM,
			profiles, profiles_names,
			w, I, P, V)
}

\keyword{ELECTRE methods}
\keyword{ELECTRE TRI}
\keyword{Sorting problem}
\keyword{Aggregation/disaggregation approaches}
\keyword{Multi-criteria decision aiding}

