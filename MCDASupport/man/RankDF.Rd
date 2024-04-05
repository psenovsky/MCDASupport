\name{rankDF}
\alias{rankDF}
\title{ rankDF - function to unpack list of ranks into ordered dataframe of alternatives in ranks}
\description{
Takes list of ranks (ie. [1] "A1" [2] "A4" "A5" [3] "A2" [4] "A3") and unpacks them into dataframe with columns action and rank.

Example leads to dataframe: (action, rak) (A1, 1), (A4, 2), (A5, 2), (A2, 3), (A3, 4).
}
\usage{
rankDF(ranklist)
}

\arguments{
  \item{ranklist}{ordered list of ranked alternatives.}
}

\value{
	returns dataframe wtih ranked alternatives from worst-to best.
}

\references{
Meyer, P. at al. MCDA package. GitHub: 2021, available from: \url{https://github.com/paterijk/MCDA/blob/master/}
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ELECTRE methods}
\keyword{ELECTRE III}
\keyword{ELECTRE IV}
\keyword{descending distillation}
\keyword{ascending distillation}
\keyword{pre-order}
