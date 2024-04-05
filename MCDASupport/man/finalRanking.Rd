\name{finalRanking}
\alias{finalRanking}
\title{ finalRanking - computes final ranking based on pre-order matrix}
\description{
Takes pre-order matrix and computes final ranking from it
}
\usage{
finalRanking(alt, rank_P)
}

\arguments{
  \item{alt}{alternatives}
  \item{rank_P}{pre-order matrix}
}

\value{
	returns data frame with final ranking in sorted and unsorted form:
	\item{final_ranking}{final ranking of the alternatives in sorted form}
	\item{finalRankingUnsorted}{final ranking of the alternatives in unsorted form (useful for sensitivity analyses)}
}

\references{
Meyer, P. at al. MCDA package. GitHub: 2021, available from: \url{https://github.com/paterijk/MCDA/blob/master/}
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ELECTRE methods}
\keyword{ELECTRE III}
\keyword{ELECTRE IV}
\keyword{final ranking}
