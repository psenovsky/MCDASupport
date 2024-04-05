\name{Electre_4}
\alias{Electre_4}
\title{ ELECTRE IV method for ranking alternatives}
\description{ELECTRE IV is alternative ranking approach based on ELECTRE III method (see \code{\link{Electre_3}}), but without using any weighting of the criteria. Since weighting is required for concordance and discordance matrixes, these are not available in ELECTRE IV. Instead method uses more complex system of outranking relations:

\itemize{
  \item mp(b,a) - number of criteria for which option b is strictly preferred to a
  \item mq(b,a) - number of criteria for which option b is weakly preferred to a
  \item mj(b,a) - number of criteria for which option b is judged indifferent to a
  \item mo(b,a) = mo(a,b) - number of criteria on which options a and b perform identically
}

These are used as a base for computation of credibility matrix.Reamining computations are same as for ELECTRE III methods including construction of descending and ascending pre-order and deriving final outranking relation.

Please refer further to \code{\link{Electre_3}} for more in detail information on computational aparatus.
}
\usage{
Electre_4(PM, P, Q, V, minmaxcriteria = 'max', VERBOSE = FALSE)
}

\arguments{
  \item{PM}{Performance matrix as matrix or data frame containing the performance table. Data frame with named columns (criteria) and rows (alternatives) is prefered.}
  \item{P}{Preference threshold - Vector containing the preference thresholds constraints defined for each criterion.}
  \item{Q}{Indifference threshold - Vector containing the indifference thresholds constraints defined for each criterion.}
  \item{V}{Veto threshold - Vector containing the veto thresholds constraints defined for each criterion.}
  \item{minmaxcriteria}{Vector containing the preference direction on each of the criteria ("min" or "max"). If all criteria are to be minimized or maximized, single min resp. max value can be used.}
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
	The function returns a list structured as follows:
	\item{PM}{Performance Matrix - A matrix containing the performance table. Each row corresponds to an alternative, and each column to a criterion}
	\item{CredibilityMatrix}{matrix assessing the strength of the assertion that “a is at least as good as b".}
	\item{rank_D}{Descending distillation ranking - final partial preorder orders the alternatives from the best
to the worst.}
  \item{rank_A}{Ascending distillation ranking - final partial preorder orders the alternatives from worst to best.}
  \item{rank_P}{Pre-order matrix specifying identified relations between the alternatives - values are P+ (a prefered to b), P- (b prefered to a), I (indifferent), R (incomparable)}
  \item{adjancancyMatrix}{Adjancency Matrix allows to visualize results as network diagram}
  \item{graph}{processed adajncency matrix into network digram.}
  \item{final_ranking}{final order of the alternatives.}
  \item{finalRankingUnsorted}{named vector of unsorted order of the alternatives, added for purposes of sensitivity analysis}
}

\references{
Balamurali, M.: pyDecisions - A Python Library of management decision making techniques. Avilable on-line from \url{https://github.com/Valdecy/pyDecisions}

Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7

Roy B. : "The outranking approach and the foundations of ELECTRE methods", Theory and Decision 31, 1991, 49-73.

Vallée, D.; Zielniewicz, P. 1994. ELECTRE III-IV, version 3.x, Aspects Méthodologiques (tome 1), Guide
d’utilisation (tome 2). Document du LAMSADE 85 et 85bis, Université Paris Dauphine

Meyer, P. at al. MCDA package. GitHub: 2021, available from: \url{https://github.com/paterijk/MCDA/blob/master/}
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
# the performance table
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
Electre_4(PM, P, Q, V, minmaxcriteria)
}

\keyword{ELECTRE methods}
\keyword{ELECTRE IV}
\keyword{Outranking approaches}
\keyword{preference modelling}
\keyword{multicriteria analysis}
\keyword{Discrimination thresholds}
\keyword{credibility matrix}
