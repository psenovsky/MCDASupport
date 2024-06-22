\name{plot.threshold}
\alias{plot.threshold}
\title{plot.threshold : Function to plot alternatives participation kernel when changing concordance or discordance threshold}
\description{
    Plot visualized participation of the alternatives on the kernel in \code{\link{Electre_1}} function.

    Alternatives are organized on x axis. Visually each alternative has its own column to explore the participation. Participation is by point in the graph. If the alternative at given threshold does not participate, the point will not be rendered in the graph.

    The graph is ploting only one threshold at the time: either concordance or discordance threshold.
}

\usage{
  plot.threshold <- function(df, title = "Kernel participation", y_label = "threshold")
}

\arguments{
  \item{df}{ dataframe with alternatives and threshold }
  \item{title}{ main title of the graph }
  \item{y_label}{ label of the y axis, usually concordance or discordance threshold, default value threshold }
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ MCDA }
\keyword{ plot }
\keyword{ concordance threshold }
\keyword{ discordance threshold }