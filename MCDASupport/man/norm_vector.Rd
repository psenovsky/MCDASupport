\name{norm_vector}
\alias{norm_vector}
\title{ Performs so called vector normalization}
\description{Normalized values are computed as follows.

For maximization transformation

\mjsdeqn{z = \frac{x}{\sqrt{\sum_{i=1}^m x_i^2}}}

For minimization transformation

%\mjsdeqn{z = \frac{1/x}{\sqrt{\sum_{i=1}^m 1/x_i^2}}}
\mjsdeqn{z = \frac{\frac{1}{x}}{\sqrt{\sum_{i=1}^m \frac{1}{x_i^2}}}}

where m is number of observation in vector to normalize

}
\usage{
  norm_vector(tonorm, minmax = 'max')
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
  normalized <- norm_vector(tonorm)
}

\keyword{normalization}
\keyword{vector normalization}
