\name{Electre_3_paramCheck}
\alias{Electre_3_paramCheck}
\title{
Electre_3_paramCheck: function for checking of the parameters fothe Electre 3 method
}
\description{
Checks parameters for ELECTRE 3 method (used by both Electre_3 and Electre_3_sensitivity methods)
}
\usage{
Electre_3_paramCheck(PM, w, P, Q, V, minmaxcriteria = 'max', alpha = 0.3, beta = 0.15)
}
\arguments{
  \item{PM}{ performance of the alternatives in criteria. Alternatives are in rows, criteria in columns.}
  \item{w}{ weights of the criteria in decision problem }
  \item{P}{ vector of preference thresholds }
  \item{Q}{ vector of indiference thresholds }
  \item{V}{ vector of veto thresholds}
  \item{minmaxcriteria}{vector of optimization direction (min/max) for the criteria }
  \item{alpha}{ first discrimination threshold (0.3 by default) }
  \item{beta}{ secind discrimination threshold (0.15 by default)}
}

\value{
returns no value (if problem with input parameters is detected it describes the problem and ends the execution)
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}
\keyword{ ELECTRE 3 }
