\name{norm_logarithm}
\alias{norm_logarithm}
\title{ Normalize values by applying logarithmic transformation.}
\description{Normalized values are computed as follows.

For maximization transformation

\mjsdeqn{z = \frac{ln(x)}{ln(\prod_{i=1}^m x_i)}}

For minimization transformation

\mjsdeqn{z = \frac{1 - \frac{ln(x)}{ln(\prod_{i=1}^m x_i)}}{m - 1}}

where m is number of observation in vector to normalize

}
\usage{
  norm_logarithm(tonorm, minmax = 'max')
}

\arguments{
  \item{tonorm}{vector of numeric values to be normalized}
  \item{minmax}{min or max, provided to support situation when we want to revert direction of used scale. If value not provided, max presumed.}
}

\value{
  returns normalized vector of values.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  tonorm <- c(1,2,3)
  normalized <- norm_logarithm(tonorm)
}

\keyword{normalization}
\keyword{logarithmic normalization}
