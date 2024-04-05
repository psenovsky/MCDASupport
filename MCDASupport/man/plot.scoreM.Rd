\name{plot.scoreM}
\alias{plot.scoreM}
\title{plot.scoreM : Function to plot visualise contribution of the criteria to overal performance of the alternatives.}
\description{Takes weighted preference matrix and uses it to plot bar chart describing how the criteria contribute to the overal score of the alternatives. Visualized in form of the ordered bar plot.}

\usage{
  plot.scoreM(x, ...)
}

\arguments{
  \item{x}{weighted preference matrix - values in matrix used to descibe strength of the relation.}
  \item{...}{allows user to specify other parameters of plotmat function from diagram package.}
}

\references{De Brouwer, Philippe J. S.: "The The Big R-Book: From Data Science to Learning Machines and Big Data ", Wiley, 2020, 928 p., ISBN  978-1119632726.}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  KoboGlo <- c(80, 97, 50, 100, 100, 94)
SonyPRST3 <- c(80, 88, 100, 20, 70, 70)
KindlePaperwhite2 <- c(100, 85, 50, 100, 70, 100)
PBTouchLux <- c(80, 90, 100, 100, 70, 94)
BookeenCybookOdyssey <- c(80, 100, 100, 100, 85, 50)
criteria <- c("Display", "váha", "HWTlačítka", "FrontLight", "baterie", "cena")
PM <- as.data.frame(
  rbind(KoboGlo,
        SonyPRST3,
        KindlePaperwhite2,
        PBTouchLux,
        BookeenCybookOdyssey))
names(PM) <- criteria
w <- c(5, 3, 4, 5, 2, 1)
preferences <- mcda_wsm(PM, w, 'max')
plot.scoreM(preferences$weightedPM)
}

\keyword{ MCDA }
\keyword{ performance score }
