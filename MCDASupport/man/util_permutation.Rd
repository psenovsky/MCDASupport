% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_permutation.R
\name{util_permutation}
\alias{util_permutation}
\title{a function to generate all possible permutations}
\usage{
util_permutation(vec)
}
\arguments{
\item{vec}{vector to derive the permutations}
}
\value{
all permutations of vec values
}
\description{
of values provided in vec vector
}
\examples{
alternatives <- c("A", "B", "C")
permutations <- util_permutation(alternatives)
}
\author{
Pavel Šenovský \email{pavel.senovsky@vsb.cz}
}
