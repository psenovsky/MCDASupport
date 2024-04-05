\name{norm_minmax}
\alias{norm_minmax}
\title{ Min-max method for normalization of the values}
\description{Min-max normalization is one of most used normalization methods. It takes values in any ascale an normalizes them into 0-1 scale.

Normalized values z are computed:

\mjsdeqn{z = \frac{x - min(x)}{max(x) - min(x)}}

}
\usage{
  norm_minmax(tonorm, minmax = 'max')
}

\arguments{
  \item{tonorm}{vector of numeric values to be normalized}
  \item{minmax}{min or max, provided to support situation when we want to revert direction of used scale. If value not provided, max presumed.}
}

\value{
  returns normalized vector of values.
}

\references{
Normalization. Code academy, available from \url{https://www.codeacademy.com/articles/normalization} [cit. 2021-09-28]
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  tonorm <- c(1,2,3)
  normalized <- norm_minmax(tonorm)
}

\keyword{normalization}
\keyword{min-max}
