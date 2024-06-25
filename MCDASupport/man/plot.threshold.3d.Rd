\name{plot.threshold.3d}
\alias{plot.threshold.3d}
\title{plot.threshold.3d : Function to plot alternatives participation kernel when changing concordance both discordance thresholds}
\description{
    Plot visualized participation of the alternatives on the kernel in \code{\link{Electre_1}} function.

    Alternatives are organized on z axis. X represents concordance and z axis represents discordance threshold. Visually each alternative has its own row which allows us to explore how the participation in kernel changes in 3D.

    Look for missing dots for indication that the alternative is not participating in the kernel at that level of concordance and discordance threshold.

    Similarly to \code{\link{plot.threshold}} the altarnatives participation is indicated only by 0/1.
}

\usage{
  plot.threshold <- function(df, title = "Kernel participation", x_label = "concordance", y_label = "discordance")
}

\arguments{
  \item{df}{ dataframe with alternatives (0/1) and concordance, discordance columns }
  \item{title}{ main title of the graph }
  \item{x_label}{ label of the x axis representing concordance threshold }
  \item{y_label}{ label of the y axis representing discordance threshold }
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ MCDA }
\keyword{ plot }
\keyword{ concordance threshold }
\keyword{ discordance threshold }