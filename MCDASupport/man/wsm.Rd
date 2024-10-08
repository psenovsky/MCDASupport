% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wsm.R
\name{wsm}
\alias{wsm}
\title{WSM (Weighted Sum Method)}
\description{
Weighted sum method (WSM) is one of simpliest and at the same time one of
 most used methods for evaluation of alternatives using different criteria.
 For the method to work it is neccessary to prepare performance matrix with
 alternatives in the rows and criteria in the columns.

The decision problem is expressed by means of numeric values characterizing
 various alternatives in criteria relevant for decision making. The method
 expects the values to be normalized.

The methods takes normalized performance matrix, applies weights for the
 criteria on it and sums the results accros the criteria to get score for
 the alternatives. Such score is usable to rank the alternatives. Score
 is provided in both raw (just sum of values) and percentage forms.
}
\examples{

## ------------------------------------------------
## Method `wsm$new`
## ------------------------------------------------

alternatives <- c('BLR', 'BOM', 'DEL', 'MNL', 'HYD', 'GRU', 'DUB',
  'KRK', 'MAA', 'EZE')
criteria <- c('tlnt', 'stab', 'cost', 'infl', 'tm-zn', 'infr', "life")
M <- rbind(
  c(0.8181818, 0.1814159, 1.0000000, 0.1198582, 0, 0.6, 0.750),
  c(1.0000000, 0.1814159, 0.6666667, 0.1198582, 0, 0.6, 0.375),
  c(1.0000000, 0.1814159, 0.8333333, 0.1198582, 0, 0.6, 0.125),
  c(0.8181818, 0.0000000, 1.0000000, 0.3482143, 0, 0.6, 0.375),
  c(0.1818182, 0.1814159, 1.0000000, 0.1198582, 0, 0.2, 0.375),
  c(0.1818182, 0.1814159, 0.5000000, 0.1198582, 0, 0.2, 0.125),
  c(0.0000000, 1.0000000, 0.0000000, 0.5741667, 1, 1.0, 1.000),
  c(0.3636364, 0.7787611, 0.6666667, 1.0000000, 1, 0.0, 0.500),
  c(0.4545455, 0.1814159, 0.9166667, 0.1198582, 0, 0.4, 0.000),
  c(0.1818182, 0.6283186, 0.5833333, 0.0000000, 0, 0.4, 0.125)
)
rownames(M) <- alternatives
colnames(M) <- criteria
w = c(0.125, 0.2, 0.2, 0.2, 0.175, 0.05, 0.05)
t <- wsm$new(M, w)
}
\references{
Šenovský, P: "Modeling of Decision Processes (in czech)", 4th edition, VŠB
 - Technical University of Ostrava, 2012, 113 p.
}
\keyword{MCDA}
\keyword{Method}
\keyword{Sum}
\keyword{WSM}
\keyword{Weighted}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{pm_orig}}{original (not normalized) performance matrix}

\item{\code{pm}}{normalized performance matrix}

\item{\code{w}}{vector of weights (there are no other limitations on weights)}

\item{\code{minmax}}{vector specifying optimalization direction for the
criteria. Values max/min are expected. If all criteria are optimalized
in same direction the vector can be replaced by single value. Max value
is default.}

\item{\code{result_table}}{has weighted performance matrix with added columns
to summ performance and extress this sum as a percentage of the best
alternative}

\item{\code{weighted_sum_prc}}{vector specifying how close the alternatives are
to the best aleternative (expresed as the percentage of best) sorted
descending (from best to worst)}

\item{\code{scoreM}}{results of the WSM presented as stacked bar chart. The
graph clearly shows contribution of the criteria to overall performance
of the alternative.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-wsm-new}{\code{wsm$new()}}
\item \href{#method-wsm-compute}{\code{wsm$compute()}}
\item \href{#method-wsm-summary}{\code{wsm$summary()}}
\item \href{#method-wsm-clone}{\code{wsm$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-wsm-new"></a>}}
\if{latex}{\out{\hypertarget{method-wsm-new}{}}}
\subsection{Method \code{new()}}{
Public constructor for the class. Checks validity of input parameters
 and performs computation of of the model based on them.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{wsm$new(pm, w, minmax = "max")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pm}}{normalized performance matrix}

\item{\code{w}}{vector of weights}

\item{\code{minmax}}{minmax vector specifying optimalization direction for the
criteria. Values max/min are expected. If all criteria are optimalized
in same direction the vector can be replaced by single value. Max value
is default.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{alternatives <- c('BLR', 'BOM', 'DEL', 'MNL', 'HYD', 'GRU', 'DUB',
  'KRK', 'MAA', 'EZE')
criteria <- c('tlnt', 'stab', 'cost', 'infl', 'tm-zn', 'infr', "life")
M <- rbind(
  c(0.8181818, 0.1814159, 1.0000000, 0.1198582, 0, 0.6, 0.750),
  c(1.0000000, 0.1814159, 0.6666667, 0.1198582, 0, 0.6, 0.375),
  c(1.0000000, 0.1814159, 0.8333333, 0.1198582, 0, 0.6, 0.125),
  c(0.8181818, 0.0000000, 1.0000000, 0.3482143, 0, 0.6, 0.375),
  c(0.1818182, 0.1814159, 1.0000000, 0.1198582, 0, 0.2, 0.375),
  c(0.1818182, 0.1814159, 0.5000000, 0.1198582, 0, 0.2, 0.125),
  c(0.0000000, 1.0000000, 0.0000000, 0.5741667, 1, 1.0, 1.000),
  c(0.3636364, 0.7787611, 0.6666667, 1.0000000, 1, 0.0, 0.500),
  c(0.4545455, 0.1814159, 0.9166667, 0.1198582, 0, 0.4, 0.000),
  c(0.1818182, 0.6283186, 0.5833333, 0.0000000, 0, 0.4, 0.125)
)
rownames(M) <- alternatives
colnames(M) <- criteria
w = c(0.125, 0.2, 0.2, 0.2, 0.175, 0.05, 0.05)
t <- wsm$new(M, w)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-wsm-compute"></a>}}
\if{latex}{\out{\hypertarget{method-wsm-compute}{}}}
\subsection{Method \code{compute()}}{
performs computation of WSM model based on class properties. Usually
 is not run manually as constructor calls this method automatically.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{wsm$compute()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-wsm-summary"></a>}}
\if{latex}{\out{\hypertarget{method-wsm-summary}{}}}
\subsection{Method \code{summary()}}{
prepares summary of the WSM method resutls and outputs them
 to the console.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{wsm$summary()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-wsm-clone"></a>}}
\if{latex}{\out{\hypertarget{method-wsm-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{wsm$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
