\name{norm_markovic}
\alias{norm_markovic}
\title{ Normalize values Markovic approach}
\description{Normalize values using approach by Markovic (2010), see equation bellow. Main benefit is that the normalization works bor both benefit and cost criteria.

\mjsdeqn{z = 1 - \frac{x - min(x)}{max(x)}}

}
\usage{
  norm_markovic(tonorm)
}

\arguments{
  \item{tonorm}{vector of numeric values to be normalized}
}

\value{
  returns normalized vector of values.
}

\references{
MARKOVIC, Z. Modification of TOPSIS Method For Solving Multicriteria Tasks. Yugoslav Journal of Operations Researc, vol. 20, no. 1, pp. 117-143, DOI: 10.2298/YJOR1001117M
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  tonorm <- c(1,2,3)
  normalized <- norm_markovic(tonorm)
}

\keyword{normalization}
