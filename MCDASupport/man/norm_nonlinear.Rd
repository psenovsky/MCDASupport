\name{norm_nonlinear}
\alias{norm_nonlinear}
\title{ Normalize values using nonlinear normalization}
\description{Normalize values using approach by Oeldschus et al (1983), see equations bellow.

For benefit criteria

\mjsdeqn{z = (\frac{x}{max(x)})^2}

For cost criteria

\mjsdeqn{z = (\frac{max(x)}{x})^2}
}
\usage{
  norm_nonlinear(tonorm, minmax)
}

\arguments{
  \item{tonorm}{vector of numeric values to be normalized}
  \item{minmax}{'min' or 'max' to specify cost or benefit criterion}
}

\value{
  returns normalized vector of values.
}

\references{
Peldschus, F., Vaigauskas, E. and Zavadskas, E.K. (1983) Technologische Entscheidungen bei der Berücksichtigung mehrerer Ziele. Bauplanung - Bautechnik, vol. 37, no. 4, pp. 173-175.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  tonorm <- c(1,2,3)
  normalized <- norm_nonlinear(tonorm, 'max')
}

\keyword{normalization}
\keyword{nonlinear normalization}
