\name{norm_ZavadskasTurskis}
\alias{norm_ZavadskasTurskis}
\title{Normalize values using Zavadskas and Turskis approach}
\description{Normalize values using approach by Zavadskas and Turskis (2008), see equations bellow.

For benefit criteria

\mjsdeqn{z = 1 - |\frac{max(x) - x}{max(x)}|}

For cost criteria

\mjsdeqn{z = 1 - |\frac{min(x) - x}{min(x)}|}
}
\usage{
  norm_ZavadskasTurskis(tonorm, minmax)
}

\arguments{
  \item{tonorm}{vector of numeric values to be normalized}
  \item{minmax}{'min' or 'max' to specify cost or benefit criterion}
}

\value{
  returns normalized vector of values.
}

\references{
Zavadskas, E.K., Turskis, Z. (2008). A New Logarithmic Normalization Method in Games Theory. Informatica, vol. 19, no. 2, pp. 303-314
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  tonorm <- c(1,2,3)
  normalized <- norm_ZavadskasTurskis(tonorm, 'max')
}

\keyword{normalization}
