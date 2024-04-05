\name{mcda_wsm}
\alias{mcda_wsm}
\title{mcda_wsm : applies weighted sum method on performace matrix}
\description{
Weighted sum method (WSM) is one of simpliest and at the same time one of most used methods for evaluation of alternatives using different criteria. For the method to work it is neccessary to prepare performance matrix with alternatives in the rows and criteria in the columns.

Values in the matrix must be notmalized. Current version of the implementation also presumes, that all criteria in performance matrix are to be maximized.

The methods takes performance matrix, applies weights for the criteria on it and sums the results accros the criteria to get score for the alternatives. Such score is usable to rank the alternatives. Score is provided in both raw (just sum of values) and percentage forms.
}

\usage{
  mcda_wsm(M, w, minmaxcriteria = 'max', VERBOSE = FALSE)
}

\arguments{
  \item{M}{Performance matrix - criteria in columns and alternatives in rows. Expected, that all values in matrix are normalized and that the criteria are to be maximized. Only numeric values in the matrix are expected.}
  \item{w}{weights for the critera in the performance matrix.}
  \item{minmaxcriteria}{criteriaMinMax Vector containing the preference direction on each of the criteria. "min" (resp."max") indicates that the criterion has to be minimized (maximized).}
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
  Returns list of results in following structure:
  \item{performanceMatrix}{A matrix containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Output provides rescaled version of matrix.}
  \item{weightedPM}{Weighted performance matrix}
  \item{weightedSum}{vector containing weighted sum for alternatives accross the criteria}
  \item{weightedSumPrc}{weingtedSum expresed as percentage - max of weingtedSum value = 100 \%}
  \item{resultTable}{integrated results with weightedPM, summ and percentages into single table}
  \item{scoreM}{visualized contibution of criteria to overal score of the variant}
}

\references{Šenovský, P: "Modeling of Decision Processes (in czech)", 4th edition, VŠB - Technical University of Ostrava, 2012, 113 p.}

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
  w = c(0.125, 0.2, 0.2, 0.2, 0.175, 0.05, 0.05)
  wsm <- mcda_wsm(M, w)
  wsm$weightedPM
  wsm$weightedSum
  wsm$weightedSumPrc
}

\keyword{ MCDA }
\keyword{ WSM }
\keyword{ weighted sum method }
