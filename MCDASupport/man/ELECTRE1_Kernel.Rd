\name{ELECTRE1_Kernel}
\alias{ELECTRE1_Kernel}
\title{ ELECTRE1_Kernel - computes so called kernell of the decision for ELECTRE I and 1S methods}
\description{
Computes kernell of the solution as the set of alternatives which are not dominated by any other alternative. Other alternatives then for example can be excluded from decision making as they are clearly suboptimal.

The results are presented in graphical form as network diagram with flows representing domination relation and two vectors with alternatives in kernel and dominated alternatives.
}
\usage{
ELECTRE1_Kernel(am)
}

\arguments{
  \item{am}{adjacancy matrix or credibility matrix (ELECTRE 1S)}
}

\value{
	Returns:
  \item{graph}{graphical representation of domination of one alternative over another}
  \item{dominated}{vector of alternatives identified as dominated}
  \item{kernel}{oposite to dominated vector. Consist for alternatives not dominated by other alternatives, forming kernel of the solution.}
}

\references{
Balamurali, M.: pyDecisions - A Python Library of management decision making techniques. Avilable on-line from \url{https://github.com/Valdecy/pyDecisions}

Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7

Prombo, M. Package OutrankingTools, CRAN: 2015, available from: \url{https://cran.r-project.org/web/packages/OutrankingTools/}
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\seealso{
  ELECTRE I: \link{Electre_1}

  ELECTRE 1S: \link{Electre_1S}
}

\keyword{ELECTRE methods}
\keyword{ELECTRE I}
\keyword{ELECTRE 1S}
\keyword{kernel}
