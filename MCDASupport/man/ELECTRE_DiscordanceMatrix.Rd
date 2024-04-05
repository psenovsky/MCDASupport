\name{ELECTRE_DiscordanceMatrix}
\alias{ELECTRE_DiscordanceMatrix}
\title{ELECTRE_DiscordanceMatrix : Method used compute discordance matrix}
\description{
\loadmathjax
Internal method for computing discordance matrix for ELECTRE I and II methods. Discordance matrix is one of two angles ELECTRE methods use to derive preference for the alternatives, the other being concordance matrix.

Concordancce matrix (index) measures strength of the statement that alternative a outranks alternative b, while discordance matrix (index) together with discordance threshold (exceeding this threshold) can prevent such outranking.

Code is inspired by pyDecisions package.

Computationally discoradce matrix D(a,b) is defined for:

\mjsdeqn{g_j(a) \ge g_j(b) \forall j: D(a,b) = 0}

and for everything else:

\mjsdeqn{D(a,b) = \max_{j} \frac{g_j(b)-g_j(a)}{\delta_j}}

where

\mjsdeqn{\delta_j = \max_{j} g_j(a)-g_j(b)}

Where a, b ... are alternatives to be compared, \mjseqn{g_j(x)} ... performance of alternative x in criterium j and \mjseqn{\delta_j} is maximal difference of alternatives a and b preformance across the criteria.

}
\usage{
ELECTRE_DiscordanceMatrix(PM)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. only numeric values expercted. Rows and columns are expected to be named and criteria are expected to be maximized (you can use function util_prepare_minmax to do that).}
}

\value{
	Returns computed discordancce matrix.
}

\references{
Balamurali, M.: pyDecisions - A Python Library of management decision making techniques. Avilable on-line from \url{https://github.com/Valdecy/pyDecisions}

Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\seealso{
  ELECTRE I: \link{Electre_1}

  concorance matrix: \link{ELECTRE_ConcordanceMatrix}
}

\examples{
PM <- cbind(
  c(103000,101300,156400,267400,49900,103600,103000,170100,279700,405000),
  c(171.3,205.3,221.7,230.7,122.6,205.1,178.0,226.0,233.8,265.0),
  c(7.65,7.90,7.90,10.50,8.30,8.20,7.20,9.10,10.90,10.30),
  c(352,203,391,419,120,265,419,419,359,265),
  c(11.6,8.4,8.4,8.6,23.7,8.1,11.4,8.1,7.8,6.0),
  c(88.0,78.3,81.5,64.7,74.1,81.7,77.6,74.7,75.5,74.7),
  c(69.7,73.4,69.0,65.6,76.4,73.6,66.2,71.7,70.9,72.0))
rownames(PM) <- c("CBX16","P205G","P405M","P605S",
  "R4GTL","RCLIO","R21TS","R21TU","R25BA","ALPIN")
colnames(PM) <- c("Prix","Vmax","C120","Coff","Acce","Frei","Brui")
minmaxcriteria <-c("min","max","min","max","min","min","min")
PMmax <- util_pm_minmax(PM, minmaxcriteria)
dm <- ELECTRE_DiscordanceMatrix(PMmax)
}

\keyword{ ELECTRE I }
\keyword{ ELECTRE II }
\keyword{ discordance matrix }
