\name{norm_zscore}
\alias{norm_zscore}
\title{ Z-Score - standard score}
\description{Standard score is the number of standard deviations by which the value of raw score is above or below mean value. Values > mean will have positive scores, while values under mean will be negative.
Normalized values z are computed:

\mjsdeqn{z = \frac{x - \mu}{\sigma}}

}
\usage{
  norm_zscore(tonorm)
}

\arguments{
  \item{tonorm}{vector of numeric values to be normalized}
}

\value{
  returns standard score for presented vector.
}

\references{
Normalization. Code academy, available from \url{https://www.codeacademy.com/articles/normalization} [cit. 2021-09-28]
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  tonorm <- c(1,2,3)
  normalized <- norm_zscore(tonorm)
}

\keyword{normalization}
\keyword{Z-Score}
\keyword{standard score}
