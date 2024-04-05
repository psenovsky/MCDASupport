\name{plot.prefM}
\alias{plot.prefM}
\alias{plot}
\title{plot.prefM : creates network diagram for visualizing preference directions of alternatives}
\description{Takes preference matrix (generally adjacancy matrix) and uses it to prepare directed circular network diagram showing outrank relation between the alternatives. The networke is then returned to calling object.

The function has been revritten in 2023 as part of preparation for sensitivity analysis functionality addition to the package. Originaly it used plotmat fuction, which worked fine visually but instead of returning graph object it ploted it to screen, which was undesirable behavior for sensitivity analyses generating such graphs by dozens.

This version of the function uses visNetwork package to solve this problem, generated networks also looks better, and the newrok is interactive by adding capability to highlight nodes to ease up visual analysis of the network.
}

\usage{
  plot(x)
}

\arguments{
  \item{x}{preference matrix - values in matrix used to descibe strength of the relation. Generally it is adjacancy matrix.}
}

\value{
  returns directed circular newrok diagram (visNetwork object) visualizing outranking relations between the alternatives
}

\references{De Brouwer, Philippe J. S.: "The The Big R-Book: From Data Science to Learning Machines and Big Data ", Wiley, 2020, 928 p., ISBN  978-1119632726.}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  alternatives <- c('BLR', 'BOM', 'DEL', 'MNL', 'HYD', 'GRU', 'DUB', 'KRK', 'MAA', 'EZE')
  M <- rbind(
    c(0, 0, 0, 1, 1, 0, 0, 0, 1, 0),
    c(0, 0, 1, 1, 1, 1, 0, 0, 1, 0),
    c(0, 0, 0, 1, 1, 0, 0, 0, 1, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0)
  )
  rownames(M) <- alternatives
  colnames(M) <- alternatives
  class(M)    <- 'prefM'
  plot(M)
}

\keyword{ MCDA }
\keyword{ domination }
