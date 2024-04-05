\name{Electre3_ConcordanceIndex}
\alias{Electre3_ConcordanceIndex}
\title{Electre3_ConcordanceIndex : Method used to compute concordance matrix for ELECTRE III and 1S methods}
\description{
\loadmathjax
Internal method for computing concordance matrix for ELECTRE III and 1S methods. Concordance matrix is one of two angles ELECTRE methods use to derive preference for the alternatives, the other being Discordance matrix.

Concordancce matrix (index) measures strength of the statement that alternative a outranks alternative b, while discordance matrix (index) together with discordance threshold (exceeding this threshold) can prevent such outranking.

To compute concordance matrix, partial concordance matrix \mjseqn{c_j} of the criteria needs to be computed first. The partial concordance matrix considers concordance by comparing performances of the alternatives in criterium \mjseqn{j}.

\mjsdeqn{c_j(a,b) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(a) + Q_j(a) \ge PM_j(b) \cr 0 & \;if\; PM_j(a) + P_j(a) < PM_j(b)) \cr \frac{PM_j(a) - PM_j(b) + P_j(a)}{P_j(a) - Q_j(a)} & else\end{array}\right.}

Based on partial concordance matrixes we compute concordence matrix \mjseqn{C} as:

\mjsdeqn{C(a,b) = \frac{\sum (w_j \cdot c_j(a,b))}{\sum w_j}}

where

PM ... performance of alternative in criterion, Q ... indifference threshold, P ... prefference threshold, w ... weights.
}
\usage{
Electre3_ConcordanceIndex(PM, P, Q, w)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. only numeric values expercted. Rows and columns are expected to be named and criteria are expected to be maximized (you can use function util_prepare_minmax to do that).}
  \item{P}{preference threshold vector}
  \item{Q}{indefference threshold}
  \item{w}{vector containing the weights of the criteria.}
}

\value{
	Returns computed concordancce matrix.
}

\references{
Balamurali, M.: pyDecisions - A Python Library of management decision making techniques. Avilable on-line from \url{https://github.com/Valdecy/pyDecisions}

Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7

Roy B. : "The outranking approach and the foundations of ELECTRE methods", Theory and Decision 31, 1991, 49-73.

Vallée, D.; Zielniewicz, P. 1994. ELECTRE III-IV, version 3.x, Aspects Méthodologiques (tome 1), Guide
d’utilisation (tome 2). Document du LAMSADE 85 et 85bis, Université Paris Dauphine

Prombo, M. Package OutrankingTools, CRAN: 2015, available from: \url{https://cran.r-project.org/web/packages/OutrankingTools/}

Meyer, P. at al. MCDA package. GitHub: 2021, available from: \url{https://github.com/paterijk/MCDA/blob/master/}
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\seealso{
  ELECTRE III: \link{Electre_3}

  ELECTRE 1S: \link{Electre_1S}
}

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
Q <- c(25,16,0,12,10) #Indifference thresholds
P <- c(50,24,1,24,20) #Preference thresholds
w <- c(1,1,1,1,1) #weights
v <- Electre3_ConcordanceIndex(PM, P, Q, w)
}

\keyword{ ELECTRE III }
\keyword{ ELECTRE 1S }
\keyword{ concordance matrix }
