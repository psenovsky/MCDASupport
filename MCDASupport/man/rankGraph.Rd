\name{rankGraph}
\alias{rankGraph}
\title{rankGraph: function to plot ranking changes as result of changing certain parameter}
\description{
  Function creates graph object using Plot-Ly library to visualize ranking of the alternatives depending on changes of one parameter.

  Was developed as a support tool for sensitivity analyses.
}
\usage{
rankGraph(data, x, param = '')
}
\arguments{
  \item{data}{ matrix or dataframe with alternatives in the columns and rows with ranks derived from value of parameter x }
  \item{x}{parameter we are testing sensitivity of the results to. Formaly it is a vector of values}
  \item{param}{name of the parameter we are testing sensitivity of solution}
}

\value{
  returns plot_ly scatter graph of the alternatives ranks depending on changes of parameter x
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ ELECTRE II }
\keyword{ ELECTRE III }
\keyword{ sensitivity analysis }
\keyword{ graph }
