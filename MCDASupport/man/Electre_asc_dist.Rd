\name{Electre_asc_dist}
\alias{Electre_asc_dist}
\title{ Electre_asc_dist: algrithm for ascending distilation}
\description{
Algorithm to establish partial preorder by the means of ascending distilation. Preorder is achieved by distilling alternatives using progresively lower cut-off thresholds.

This aproach is complementary to descending distilation process which creates second pre-order.

The algorithm is used in Electre_3 and 4 methods.
}

\usage{
Electre_asc_dist(sm)
}

\arguments{
  \item{sm}{confidence matrix}
}

\value{
	returns list of alternatives in ranks from worst-to best.
}

\references{
Meyer, P. at al. MCDA package. GitHub: 2021, available from: \url{https://github.com/paterijk/MCDA/blob/master/}
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ELECTRE methods}
\keyword{ELECTRE III}
\keyword{ELECTRE IV}
\keyword{ascending distillation}
\keyword{pre-order}
