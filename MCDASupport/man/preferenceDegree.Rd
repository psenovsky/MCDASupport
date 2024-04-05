\name{preferenceDegree}
\alias{preferenceDegree}
\title{preferenceDegree: computes preference degree for SIR and PROMETHEE dunctions}
\description{
The function is being used in \code{\link{SIR}} and \code{\link{PROMETHEE}}) functions.
}
\usage{
preferenceDegree(nalt, ncri, DK, d, qj, pj, sj, alt, cri)
}

\arguments{
  \item{nalt}{number of alternatives}
  \item{ncri}{number of criteria}
  \item{DK}{result of paiweise comparison}
  \item{d}{type of function (default, U-shape, V-shape, level), vector - sets the type for every criterion}
  \item{qj}{indifference threshold vector}
  \item{pj}{preference threshold vector}
  \item{sj}{intermediate threshold vector}
  \item{alt}{vector of alternative's names}
  \item{cri}{vector of criteria's names}
}

\value{
	returns preference degree matrix.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}
\keyword{ PROMETHEE }
\keyword{ SIR }

