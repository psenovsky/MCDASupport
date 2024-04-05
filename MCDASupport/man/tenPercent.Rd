\name{tenPercent}
\alias{tenPercent}
\title{tenPercent: +/- 10\% sequence }
\description{
Function returns sequence +/- 10 \% of given number by 1 \%. The function is used by sensitivity analysis functions such as \code{\link{Electre_2_sensitivity}}.
}
\usage{
tenPercent(x)
}

\arguments{
  \item{x}{number}
}

\value{
	returns vector of numbers +/- 10 \% of x by 1 \%.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}
\keyword{ sensitivity }
