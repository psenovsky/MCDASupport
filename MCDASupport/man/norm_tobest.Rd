\name{norm_tobest}
\alias{norm_tobest}
\title{ Normalize values by comparing them to maximum in the vector.}
\description{Normalized values are computed by comparing the values to maximum. Results show how close the values are to this maximum (in percents).

\mjsdeqn{z = \frac{x}{max(x)} \cdot 100}

}
\usage{
  norm_tobest(tonorm)
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
  normalized <- norm_tobest(tonorm)
}

\keyword{normalization}
\keyword{normalize to best value}
