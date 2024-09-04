\name{tenPercent}
\alias{tenPercent}
\title{tenPercent: +/- 10\% sequence }
\description{
Function returns sequence +/- 10 \% of given number by 1 \%. The function is used by sensitivity analysis functions in version 0.30 of the package. 

For newer versions of the package the function is depreciated and will be eventualy removed.
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
