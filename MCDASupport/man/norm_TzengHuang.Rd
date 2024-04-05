\name{norm_TzengHuang}
\alias{norm_TzengHuang}
\title{ Normalize values using Tzeng and Huang approach}
\description{Normalize values using approach by Tzeng and Huang (2011), see equation bellow. Main benefit is that the normalization works bor both benefit and cost criteria.

\mjsdeqn{z = \frac{max(x)}{x}}

}
\usage{
  norm_TzengHuang(tonorm)
}

\arguments{
  \item{tonorm}{vector of numeric values to be normalized}
}

\value{
  returns normalized vector of values.
}

\references{
Tzeng, G. and Huang, J. (2011) Multiple Attribute Decision Making Methods and Applications. CRC Press, Taylor and Francis Group, A Chapman & Hall Book, Boca Raton. 350 p., ISBN 978-1-4398-6157-8
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  tonorm <- c(1,2,3)
  normalized <- norm_TzengHuang(tonorm)
}

\keyword{normalization}
