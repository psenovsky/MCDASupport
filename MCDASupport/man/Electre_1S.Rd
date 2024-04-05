\name{Electre_1S}
\alias{Electre_1S}
\title{Electre_1S : ELECTRE 1S method used to solve multiple criteria decision making}
\description{
Method for supporting multicriteria decision making used to identify so called kernel of solution as set of alternatives which are not dominated by any other alternative. Dominated alternatives can be ommited from decision making as they clearly represent sub-optimal solution for the problem.

Method does not provide ranking of the alternatives.

Computationally the method can be seen as a hybrid of ELECTRE III (\code{\link{Electre_3}}) and I \code{\link{Electre_1}}methods. From ELECTRE III it takes concordance matrix computation as it works with fuzzy defined preference (P), indifference (Q) and veto (V) thresholds. From ELECTRE I it takes procedure to identify kernel of the solution.

The procedure itself is as follows. First we compute Concordance matrix. Concordancce matrix (index) measures strength of the statement that alternative a outranks alternative b.

To compute concordance matrix, partial concordance matrix \mjseqn{c_j} of the criteria needs to be computed first. The partial concordance matrix considers concordance by comparing performances of the alternatives in criterium \mjseqn{j}.

\mjsdeqn{c_j(a,b) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(a) + Q_j(a) \ge PM_j(b) \cr 0 & \;if\; PM_j(a) + P_j(a) < PM_j(b)) \cr \frac{PM_j(a) - PM_j(b) + P_j(a)}{P_j(a) - Q_j(a)} & else\end{array}\right.}

Since the procedure is same as for ELECTRE III, the code has been refactored into separate function \code{\link{ELECTRE_ConcordanceMatrix}} and only called form this function.

Based on partial concordance matrixes we compute concordence matrix \mjseqn{C} as:

\mjsdeqn{C(a,b) = \frac{\sum (w_j \cdot c_j(a,b))}{\sum w_j}}

where

PM ... performance of alternative in criterion, Q ... indifference threshold, P ... prefference threshold, w ... weights.

Discordance matrix \mjseqn{d} consist of discordance indexes which provide together with discordance threshold (exceeding this threshold) information preventing outranking.

\mjsdeqn{d[i,j] = \left\lbrace\begin{array}{ll} 1 & \;if\; PM[j,k] - PM[i,k] \ge V_k - Q_k \cdot \frac{1 - C[i,j] - (w_k/\sum w)}{1 - \lambda - (w_k/\sum w)}  \cr 0 & otherwise \end{array}\right.}

Where d ... discordance matrix, i ... variable used to iterate over alternatives, j,k ... variables use to iterate over criteria, \mjseqn{\lambda} ... cutoff criterium, V ... veto threshold, othervise same notation as in previous equations has been used.

Next credibility matrix needs to be computed. Credibility matrix takes into account concordance and discordance indexes to evaluate haw credible it is that a over prerforms b (aSb).

\mjsdeqn{cred(a,b) = \left\lbrace\begin{array}{ll} 1 & \;if\; C(a,b) \ge \lambda \; and \; d(a,b) = 0 \cr 0 & otherwise \end{array}\right.}

Finally kernel of solution is computed in same manner as for ELECTRE I method using \code{\link{ELECTRE1_Kernel}}. We use credibility matrix as input parameter for the function as it is also adjacancy matrix. The matrix needs to be simplified by removing loops from it using for example Johnson's algorithm.

The kernel is returned in form of graph, vectors of dominated alternatives and vector of alternatives forming kernel.
}

\usage{
Electre_1S(PM, w, Q, P, V, minmaxcriteria = 'max', lambda = 0.5, VERBOSE = FALSE)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. only numeric values expercted. Rows and columns are expected to be named.}
  \item{w}{vector containing the weights of the criteria.}
  \item{Q}{vector of indifference thresholds}
  \item{P}{vector of preference thresholds}
  \item{V}{vector of veto thresholds}
  \item{minmaxcriteria}{criteriaMinMax Vector containing the preference direction on each of the criteria. "min" (resp."max") indicates that the criterion has to be minimized (maximized).}
  \item{lambda}{parameter defining concordance threshold. The default value is 0.5, but the value can be in interval <0.5;1>.}
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
	The function returns a list structured as follows :
	\item{PerformanceMatrix}{A matrix containing the performance table. Each row corresponds to an alternative, and each column to a criterion}
	\item{ConcordanceMatrix}{Concordance matrix is one of two working relations (concordance and discordance) which are subsequently used to construct the final dominance relation. For an outranking aSb to be validated, a sufficient majority of criteria should be in favor of this assertion.}
	\item{DiscordanceIndex}{Discordance matrix is one of two working relations (concordance and discordance) which are subsequently used to construct the final dominance relation. The concept of discordance is complementary to the one of (concordance and represents the discomfort experienced in the choosing of alternative a above alternative b}
	\item{CredibilityIndex}{basically preference matrix as computed by ELECTRE I method. Represents domination relation between the alternatives. Provides value 1 where for relation between alternatives where such domination exists}
	\item{Kernel}{Kernel consist of subset of non-dominated alternatives}
  \item{Dominated}{Identified clearly dominated alternatives.}
  \item{graph}{Visualization of the preference matrix using network graph. A -> B means, that A outranks B.}
}

\references{
Balamurali, M.: pyDecisions - A Python Library of management decision making techniques. Avilable on-line from \url{https://github.com/Valdecy/pyDecisions}

Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7

Prombo, M. Package OutrankingTools, CRAN: 2015, available from: \url{https://cran.r-project.org/web/packages/OutrankingTools/}

Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7
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
w <- c(1,1,1,1,1) #weights
Electre_1S(PM, w, Q, P, V, minmaxcriteria)
}
\keyword{ ELECTRE I }
\keyword{ ELECTRE III }
\keyword{ ELECTRE 1S }
\keyword{ concordance matrix }
\keyword{ discordane matrix }
\keyword{ credibility matrix }
\keyword{ kernel }
