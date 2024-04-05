\name{Electre_4_paramCheck}
\alias{Electre_4_paramCheck}
\title{
Electre_4_paramCheck: function for checking of the parameters fothe Electre 4 method
}
\description{
Checks parameters for ELECTRE 4 method (used by both Electre_4 and Electre_4_sensitivity methods)
}
\usage{
Electre_4_paramCheck(PM, P, Q, V, minmaxcriteria = 'max')
}
\arguments{
  \item{PM}{ performance of the alternatives in criteria. Alternatives are in rows, criteria in columns.}
  \item{P}{ vector of preference thresholds }
  \item{Q}{ vector of indiference thresholds }
  \item{V}{ vector of veto thresholds}
  \item{minmaxcriteria}{vector of optimization direction (min/max) for the criteria }
}

\value{
returns no value (if problem with input parameters is detected it describes the problem and ends the execution)
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}
\keyword{ ELECTRE 4 }
