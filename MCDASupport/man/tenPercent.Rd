% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tenPercent.R
\name{tenPercent}
\alias{tenPercent}
\title{function for creating a sequence +/- 10\% of valaue}
\usage{
tenPercent(x)
}
\arguments{
\item{x}{a number for which the sequence should be generated}
}
\value{
vector of numbers +/- 10 \% of x by 1 \%.
}
\description{
Function returns sequence +/- 10 \% of given number by 1 \%. The function is
 used by sensitivity analysis functions in version 0.30 of the package.

For newer versions of the package the function is depreciated and will be
 eventualy removed.
}
\author{
Pavel Šenovský \email{pavel.senovsky@vsb.cz}
}
\keyword{sensitivity}
