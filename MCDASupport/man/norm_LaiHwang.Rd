\name{norm_LaiHwang}
\alias{norm_LaiHwang}
\title{ Normalize values using Lai and Hwang approach}
\description{Normalize values using approach by Lai and Hwang (1994), see equations bellow.

For benefit criteria

\mjsdeqn{z = \frac{x}{max(x) - min(x)}}

For cost criteria

\mjsdeqn{z = \frac{x}{min(x) - max(x)}}
}
\usage{
  norm_LaiHwang(tonorm, minmax)
}

\arguments{
  \item{tonorm}{vector of numeric values to be normalized}
  \item{minmax}{'min' or 'max' to specify cost or benefit criterion}
}

\value{
  returns normalized vector of values.
}

\references{
Lai, Young-hou & Ching-Lai Hwang (1994). Fuzzy Multiple Objective Decision Making Method and Applications. Im Lecture Notes in Economics and Mathematical Systems 404. Berlin, Heidelberg: Springer-Verlag. DOI: 10.007/978-3-642-57949-3.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  tonorm <- c(1,2,3)
  normalized <- norm_LaiHwang(tonorm, 'max')
}

\keyword{normalization}
