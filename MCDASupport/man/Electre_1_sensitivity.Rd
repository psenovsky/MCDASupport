\name{Electre_1_sensitivity}
\alias{Electre_1_sensitivity}
\title{
Electre_1_sensitivity: function for performing sensitivity analysis for Electre 1 method
}
\description{
Performs basic sensitivity analysis of the decision problem solution to changes in concordance (C) and discordance (D) thresholds.
}
\usage{
Electre_1_sensitivity(PM, w,
  minmaxcriteria = 'max',
  concordance_threshold,
  discordance_threshold)
}
\arguments{
  \item{PM}{  performance of the alternatives in criteria. Alternatives are in rows, criteria in columns.}
  \item{w}{ weights of the criteria in decision problem }
  \item{minmaxcriteria}{  vector specifying direction of the criterium (min/max). Use single min or max if all criteria have same direction}
  \item{concordance_threshold}{  concordance threshold. Set as vector c(from, to, step, default) }
  \item{discordance_threshold}{  discordance threshold. Set as vector c(from, to, step, default) }
}

\value{
returns dataframe: alternative 1, aplternative 2, ..., alternative n, threshold (C or D), value of threshold, where n is number of alternatives. Value of other threshold is not listed as it is = to default threshold value.
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
PM <- cbind(
  #c(-14,129,-10,44,-14),
  c(14,129,10,44,14),
  c(90,100,50,90,100),
  c(0,0,0,0,0),
  c(40,0,10,5,20),
  c(100,0,100,20,40)
)
rownames(PM) <- c("Project1","Project2","Project3","Project4","Project5")
colnames(PM) <- c( "CR1","CR2","CR3","CR4","CR5")
minmaxcriteria <- 'max'
w <- c(1,1,1,1,1) #weights
t <- Electre_1_sensitivity(PM, w, minmaxcriteria,
                           concordance_threshold = c(0.75, 0.85, 0.01, 0.8),
                           discordance_threshold = c(0.05, 0.15, 0.01, 0.1))
}
\keyword{ ELECTRE 1 }
\keyword{ sensitivity analysis }
\keyword{ kernel }
