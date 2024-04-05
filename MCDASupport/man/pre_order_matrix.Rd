\name{pre_order_matrix}
\alias{pre_order_matrix}
\title{ pre_order_matrix - function to create preorder matrix}
\description{Function takes outputs of descending and ascending distilation pre-order and created pre-order matrix describing outranking relation between the alternatives.

The relation can be P+ (a outranks b), P- (b outranks a), I (indifference) and R (incomparable). Information from preorder matrix can be utilized to create adjancancy matrix and construct final ranking.
}
\usage{
pre_order_matrix(rank_D, rank_A, alt)
}

\arguments{
  \item{rank_D}{descending distilation ranking in the form of ordered dataframe}
  \item{rank_A}{ascending distilation ranking in the form of ordered dataframe}
  \item{alt}{vector of alternatives names}
}

\value{
	returns pre-order matrix.
}

\references{
Meyer, P. at al. MCDA package. GitHub: 2021, available from: \url{https://github.com/paterijk/MCDA/blob/master/}

Prombo, M. Package OutrankingTools, CRAN: 2015, available from: \url{https://cran.r-project.org/web/packages/OutrankingTools/}
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ELECTRE methods}
\keyword{ELECTRE III}
\keyword{ELECTRE IV}
\keyword{pre-order matrix}
\keyword{pre-order}
