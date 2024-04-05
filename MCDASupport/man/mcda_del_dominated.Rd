\name{mcda_del_dominated}
\alias{mcda_del_dominated}
\title{mcda_del_dominated : Deletes dominated alternatives from preference matrix}
\description{Deletes dominated alternatives from preference matrix (should such alternatives exist in the matrix) and return the result.}

\usage{
  mcda_del_dominated(M, minmaxcriteria, digits)
}

\arguments{
  \item{M}{Normalized performance matrix - criteria in columns and alternatives in rows. Expected, that all criteria are to be maximized.}
  \item{minmaxcriteria}{either value (min or max) or vector providing guidance for orientation of the criteria in preference matrix. If parameter provides only single max or min value, the function will presume usige of this orientation for all criteria. Default value is 'max'.}
  \item{digits}{number of digits the resulting matrix cells should be rounded to. Default value is 2 (round to 2 digits).}
}

\value{
  Returns matrix containing only such alternatives, which were not clearly dominated by some other alternative.
}

\references{De Brouwer, Philippe J. S.: "The The Big R-Book: From Data Science to Learning Machines and Big Data ", Wiley, 2020, 928 p., ISBN  978-1119632726.}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  alternatives <- c('BLR', 'BOM', 'DEL', 'MNL', 'HYD', 'GRU', 'DUB', 'KRK', 'MAA', 'EZE')
  criteria <- c('tlnt', 'stab', 'cost', 'infl', 'tm-zn', 'infr', "life")
  M <- rbind(
    c(0.8181818, 0.1814159, 1.0000000, 0.1198582, 0, 0.6, 0.750),
    c(1.0000000, 0.1814159, 0.6666667, 0.1198582, 0, 0.6, 0.375),
    c(1.0000000, 0.1814159, 0.8333333, 0.1198582, 0, 0.6, 0.125),
    c(0.8181818, 0.0000000, 1.0000000, 0.3482143, 0, 0.6, 0.375),
    c(0.1818182, 0.1814159, 1.0000000, 0.1198582, 0, 0.2, 0.375),
    c(0.1818182, 0.1814159, 0.5000000, 0.1198582, 0, 0.2, 0.125),
    c(0.0000000, 1.0000000, 0.0000000, 0.5741667, 1, 1.0, 1.000),
    c(0.3636364, 0.7787611, 0.6666667, 1.0000000, 1, 0.0, 0.500),
    c(0.4545455, 0.1814159, 0.9166667, 0.1198582, 0, 0.4, 0.000),
    c(0.1818182, 0.6283186, 0.5833333, 0.0000000, 0, 0.4, 0.125)
  )
  rownames(M) <- alternatives
  colnames(M) <- criteria
  mcda_del_dominated(M)
}

\keyword{ MCDA }
\keyword{ domination }
