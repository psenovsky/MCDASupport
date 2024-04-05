\name{norm_linearagreg}
\alias{norm_linearagreg}
\title{ Linear normalization using aggregation of values}
\description{Normalized values are computed as follows.

For maximization transformation

\mjsdeqn{z = \frac{x}{\sum_{i=1}^m x_i}}

For minimization transformation

\mjsdeqn{z = \frac{\frac{1}{x}}{\sum_{i=1}^m \frac{1}{x_i}}}

where m is number of observation in vector to normalize

}
\usage{
  norm_linearagreg(tonorm, minmax = 'max')
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
  normalized <- norm_linearagreg(tonorm)
}

\keyword{normalization}
\keyword{linear normalization by aggregating values}
