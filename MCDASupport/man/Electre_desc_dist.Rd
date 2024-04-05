\name{Electre_desc_dist}
\alias{Electre_desc_dist}
\title{ Electre_desc_dist - algrithm for descending distilation}
\description{Algorithm to establish partial preorder by the means of descending distilation. Preorder is achieved by distilling alternatives using progresively lower cutoff thresholds.

This aproach is complementary to ascending distilation process \code{\link{Electre_asc_dist}} which creates second preorder.

The algorithm is used in Electre_3 and 4 methods.
}
\usage{
Electre_desc_dist(sm)
}

\arguments{
  \item{sm}{confidence matrix}
}

\value{
	returns list of alternatives in ranks from worst to best.
}

\references{
Meyer, P. at al. MCDA package. GitHub: 2021, available from: \url{https://github.com/paterijk/MCDA/blob/master/}
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ELECTRE methods}
\keyword{ELECTRE III}
\keyword{ELECTRE IV}
\keyword{descending distillation}
\keyword{pre-order}
