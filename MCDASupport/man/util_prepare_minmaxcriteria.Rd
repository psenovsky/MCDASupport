\name{util_prepare_minmaxcriteria}
\alias{util_prepare_minmaxcriteria}
\title{util_prepare_minmaxcriteria : validates and prepares minmax vector for processing}
\description{
Internal function package uses to validate vector containing information on criteria optimization direction (minimizaing or maximizing). Since this operation is required for almost all decision analysis functions, it has been refactored into separate function.

If provided with direction to min or max - it will create the minmaxcriteria vector minimizing or maximizing all the criteria.
}

\usage{
  util_prepare_minmaxcriteria(ncrit, minmaxcriteria)
}

\arguments{
  \item{ncrit}{number of criteria used in decision problem (must be > 2)}
  \item{minmaxcriteria}{provides either 'min' or 'max' (default) values if all criteria should be minimalized or maximalized. If it is not case provide vector specifying either 'max' or 'min' for each criterion.}
}

\value{
  Returns validated and completed vector with information on minimizing or maximizing criteria.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
  #5 maximizzed criteria
  minmax <- util_prepare_minmaxcriteria(5)
  #5 minimized criteria
  minmax <- util_prepare_minmaxcriteria(5, minmaxcriteria = 'min')
  #mix
  minmax <- util_prepare_minmaxcriteria(5, c('min', 'min', 'max', 'max', 'min'))
}

\keyword{ MCDA }
