\name{norm_toaverage}
\alias{norm_toaverage}
\title{ Normalize values by comparing them to average.}
\description{Normalized values are computed by comparing the values to average.

\mjsdeqn{z = \frac{x}{\mu} \cdot 100}

}
\usage{
  norm_toaverage(tonorm)
}

\arguments{
  \item{tonorm}{vector of numeric values to be normalized}
}

\value{
  returns normalized vector of values.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  tonorm <- c(1,2,3)
  normalized <- norm_toaverage(tonorm)
}

\keyword{normalization}
\keyword{normalize to average value}
