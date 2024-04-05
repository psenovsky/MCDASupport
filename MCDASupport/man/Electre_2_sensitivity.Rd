\name{Electre_2_sensitivity}
\alias{Electre_2_sensitivity}
\title{Electre_2_sensitivity : method to perform sensitivity analysis for ELECTRE II}
\description{
  sensitivity analysis for ELECTRE II (see \code{\link{Electre_2}} function documentation) is performed on values of concordance and discordance thresholds. Since these thresholds are defined as fuzzy numbers, the analysis is performed on five parameters c-, c0 and c+ for concordance thresholds, d- and d+ for discordance threshold.

  Analysis always explores changes of a single parametr, while all other are held static (non changing).

  Analysis provides two types of result. A dataframe with alternatives ranks to explore how the ranking changes as a result of changes in a single parameter. Function also returns graphical representation of the this dataframe to see these changes visually.

  Range of used hyperparameters is also provided for scale.
}
\usage{
Electre_2_sensitivity(PM, w,
  minmaxcriteria = 'max',
  c_minus = c(0.51, 0.74, 0.01, 0.65),
  c_zero = c(0.66, 0.84, 0.01, 0.75),
  c_plus = c(0.76, 0.99, 0.01, 0.85),
  d_minus = c(0.01, 0.49, 0.01, 0.25),
  d_plus = c(0.26, 0.66, 0.01, 0.5))
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. only numeric values expercted. Rows and columns are expected to be named.}
  \item{w}{vector containing the weights of the criteria.}
  \item{minmaxcriteria}{vector containing the preference direction on each of the criteria. "min" (resp."max") indicates that the criterion has to be minimized (maximized).}
  \item{c_minus}{first of three concordance threshold parameters. Parameter is in form of vector c(from, to, step, default).}
  \item{c_zero}{second of three concordance threshold parameters. Parameter is in form of vector c(from, to, step, default).}
  \item{c_plus}{third of three concordance threshold parameters. Parameter is in form of vector c(from, to, step, default).}
  \item{d_minus}{first of two parameters defining discordance threshold. Parameter is in form of vector c(from, to, step, default).}
  \item{d_plus}{first of two parameters defining discordance threshold. Parameter is in form of vector c(from, to, step, default).}
}

\value{
	The function returns a list structured as follows:
	\item{c_minus_sensitivity}{alternative ranks considering changes in c- parameter}
	\item{c_zero_sensitivity}{alternative ranks considering changes in c0 parameter}
	\item{c_plus_sensitivity}{alternative ranks considering changes in c+ parameter}
	\item{d_minus_sensitivity}{alternative ranks considering changes in d- parameter}
	\item{d_plus_sensitivity}{alternative ranks considering changes in d+ parameter}
	\item{c_minus_graph}{graph sensitivity analysis of c-}
	\item{c_zero_graph}{graph sensitivity analysis of c0}
	\item{c_plus_graph}{graph sensitivity analysis of c+}
	\item{d_minus_graph}{graph sensitivity analysis of d-}
	\item{d_plus_graph}{graph sensitivity analysis of d+}
	\item{c_minus}{used hyperspace for c- parameter}
	\item{c_zero}{used hyperspace for c0 parameter}
	\item{c_plus}{used hyperspace for c+ parameter}
	\item{d_minus}{used hyperspace for d- parameter}
	\item{d_plus}{used hyperspace for d+ parameter}
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

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
w <- c(0.3,0.1,0.3,0.2,0.1,0.2,0.1)
t <- Electre_2_sensitivity(PM, w, minmaxcriteria)
}

\keyword{ ELECTRE II }
\keyword{ sensitivity analysis }
