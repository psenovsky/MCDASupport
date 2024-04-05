\name{Graph2AdjancancyMatrix}
\alias{Graph2AdjancancyMatrix}
\title{Graph2AdjancancyMatrix : Transforms graph (a->b format) to adjncancy matrix}
\description{Function transforms back graph specified by edges to adjancancy matrix, with 1 where edge exists and 0 where it does not.
}
\usage{
Graph2AdjancancyMatrix(G, alt)
}

\arguments{
  \item{G}{Graph which should be transformed}
  \item{alt}{alternatives names}
}

\value{
	Returns adjancancy matrix for the graph with 0/1 representing graph.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ graph }
\keyword{ adjancancy matrix }
