\name{Electre_2_paramCheck}
\alias{Electre_2_paramCheck}
\title{
Electre_2_paramCheck: function for checking of the parameters fothe Electre 2 method
}
\description{
Checks parameters for ELECTRE 2 method (used by both Electre_2 and Electre_2_sensitivity methods)
}
\usage{
Electre_2_paramCheck(PM, w)
}
\arguments{
  \item{PM}{ performance of the alternatives in criteria. Alternatives are in rows, criteria in columns.}
  \item{w}{ weights of the criteria in decision problem }
}

\value{
returns no value (if problem with input parameters is detected it describes the problem and ends the execution)
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}
\keyword{ ELECTRE 2 }
