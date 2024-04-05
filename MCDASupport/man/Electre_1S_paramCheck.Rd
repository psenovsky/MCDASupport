\name{Electre_1S_paramCheck}
\alias{Electre_1S_paramCheck}
\title{
Electre_1S_paramCheck: function for checking of the parameters fothe Electre 1S method
}
\description{
Checks parameters for ELECTRE 1S method (used by both Electre_1S and Electre_1S_sensitivity methods)
}
\usage{
Electre_1S_paramCheck(PM, w, Q, P, V, minmaxcriteria = 'max', lambda = 0.5,
  VERBOSE = FALSE)
}
\arguments{
  \item{PM}{ performance of the alternatives in criteria. Alternatives are in rows, criteria in columns.}
  \item{w}{ weights of the criteria in decision problem }
  \item{Q}{ indifference threshold vector}
  \item{P}{ preference thresholds vector }
  \item{V}{ veto thresholds vector }
  \item{minmaxcriteria}{ vector of optimization direction for criteria (either min or max) }
  \item{lambda}{ cut-off criterion, set to 0.5 by default }
  \item{VERBOSE}{ T/F - can switch on verbose mode for Electre_1S function }
}

\value{
returns no value (if problem with input parameters is detected it describes the problem and ends the execution)
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}
\keyword{ ELECTRE 1S }
