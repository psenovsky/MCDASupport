% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/critic.R
\name{critic}
\alias{critic}
\title{Criteria Importance through intercriteria correlations}
\usage{
critic(pm, minmax = "max")
}
\arguments{
\item{pm}{transformed performance matrix, all criteria are maximized}

\item{minmax}{directio of optimization, can be replaced by single min
or max if all criteria have same optimization direction.}
}
\value{
vector of weights
}
\description{
Method is used to derive weight coefficients of criteria based on
 correlations. Weight derivation does not require using expert oppinions
 to map prefferences for criteria. Instead it uses correlation to derive
 these weights.

In this sense it is simmilar to methods such as \link{ewm}. In comparison
 the authors claim that this method provides better results than EWM.

We start process by formulating performance matrix PM (criteria in columns,
 alternatives in rows). Numeric values are expected. Then the performance
 matrix is normalized using min-max method.

For each criterium we then compute standard deviation.

Basis of the evaluation is correlation matrix R we compute using Pearson's
 correlation coefficien. This information can be used to compute the
 conflict created by the criterium with respect to other criteria:

\mjsdeqn{\sum_{k=1}^m (1 - r_{jk})}

Then we determine quality of the information provided by multiplying the
 result by standard deviation

\mjsdeqn{C_j = \sigma_j \sum_{k=1}^m (1 - r_{jk}) }

Finally we can compute the weight by normalizing the C indicator

\mjsdeqn{w_j = \frac{C_j}{\sum_{k=1}^m C_j}}
}
\examples{
PM <- rbind(
  c(0.446, 1.785, 6.643, 6.843, 50),
  c(1.113, 2.425, 10.525, 2.902, 250),
  c(1.246, 3.321, 14.224, 3.885, 600),
  c(1.935, 3.678, 17.852, 4.406, 1000),
  c(0.446, 3.062, 5.238, 3.112, 200),
  c(1.064, 3.814, 14.558, 4.121, 250),
  c(1.654, 4.581, 17.888, 4.886, 1600),
  c(1.924, 5.226, 22.224, 5.702, 1500),
  c(0.337, 4.444, 24.708, 4.123, 450),
  c(0.998, 5.12, 18.012, 5.206, 1500),
  c(1.622, 5.886, 22.226, 6.226, 600),
  c(1.844, 6.234, 26.128, 6.786, 1500),
  c(0.531, 5.6, 18.883, 5.405, 800),
  c(1.023, 6.123, 21.987, 6.501, 1500),
  c(1.664, 7.244, 27.012, 7.421, 1600),
  c(2.012, 7.345, 28.021, 7.923, 1000)
)
rownames(PM) <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9",
  "A10", "A11", "A12", "A13", "A14", "A15", "A16")
colnames(PM) <- c("Ra", "Ax", "Ay", "Az", "Q")
minmax <- c("min", "min", "min", "min", "max")
w <- critic(PM, minmax)
}
\references{
Diakonlaki, D., Mavrotas, G., Papayannadis, J. (1995). Datamining Objective
 Weights in Multiple Criteria Problems: the CRITIC Method. Computers and
 Operations Research, 22(7), pp. 763-770.
}
\author{
Pavel Šenovský \email{pavel.senovsky@vsb.cz}
}
\keyword{CRITIC}
