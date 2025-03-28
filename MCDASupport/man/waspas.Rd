% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waspas.R
\name{waspas}
\alias{waspas}
\title{Weighted Aggregated Sum Product Assessment}
\description{
Method developed by Zavadkas as a compromise approach to \link{wsm} and
 \link{wpm} methods. The idea is that using both methods together will
 minimize negative properties of the methods providing bettor solution to
 the decision problem.

For example WSM is compensatory method, meaning that worse or even
 insufficient performance in one criterium can be compensated by better
 performance in other criteria. This may lead to bad solution if nature of
 the decision problem is not fully compensatory.

The computation is following. First performance matrix is normalized to
 maximum for benefit criteria

\mjsdeqn{\overline{x_{ij}} = \frac{x_{ij}}{max_i x_{ij}}}

and for cost criteria

\mjsdeqn{\overline{x_{ij}} = \frac{min_i x_{ij}}{x_{ij}}}

Then the performance of alternatives is computed

\mjsdeqn{Q_i = \lambda \sum_{j=1}^n \overline{x_{ij}}w_j + (1 - \lambda) \prod_{j = 1}^n x_{ij}^{w_j}}

Q value is then directly interpretable for purposes of sorting (max = best).

Lambda value in original Zavadkas article was 0.5 - leading to same
 proportion of WSM and WPM influence to problem solution.
}
\references{
Zavadskas, E.K., Turskis, Z., Antucheviciene, J., Zakarevicius, A. (2012),
 Optimization of Weighted Aggregated Sum Product Assessment. Elektronika
ir Elektrotechnika (6), 3–6;

Zavadskas, E. K., Antucheviciene, J., Saparauskas, J., Turskis, Z.
 (2013a), Multi-criteria Assessment of Facades’ Alternatives: Peculiarities
 of Ranking Methodology. Procedia Engineering 57, 107–112;
}
\author{
Pavel Šenovský \email{pavel.senovsky@vsb.cz}
}
\keyword{WASPAS}
\keyword{WPM}
\keyword{WSM}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{pm_orig}}{original percormance matrix (alternatives in rows,
criteria in columns)}

\item{\code{pm}}{normalized performance matrix}

\item{\code{minmax}}{vector of optimization direction for criteria (min/max)}

\item{\code{w}}{weight vector}

\item{\code{lambda}}{proportion of the WSM}

\item{\code{q}}{performance of alternatives (results of method)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-waspas-new}{\code{waspas$new()}}
\item \href{#method-waspas-compute}{\code{waspas$compute()}}
\item \href{#method-waspas-summary}{\code{waspas$summary()}}
\item \href{#method-waspas-clone}{\code{waspas$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-waspas-new"></a>}}
\if{latex}{\out{\hypertarget{method-waspas-new}{}}}
\subsection{Method \code{new()}}{
validates parameters and computes the results.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{waspas$new(pm, w, minmax = "max", lambda = 0.5)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pm}}{percormance matrix (alternatives in rows, criteria in columns)}

\item{\code{w}}{weight vector (sum w = 1)}

\item{\code{minmax}}{vector of optimization direction for criteria (min/max),
can be substitued for single min/max if all directions are same}

\item{\code{lambda}}{proportion of WSM, rest to 1 will be WPM proportion
(default 0.5)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
instance of the WASPAS class
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-waspas-compute"></a>}}
\if{latex}{\out{\hypertarget{method-waspas-compute}{}}}
\subsection{Method \code{compute()}}{
Computed the WASPAS model based on params provided in fields of the
 class. Usually it is not required to call this method manualy as it is
 called automatically in the constructor.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{waspas$compute()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-waspas-summary"></a>}}
\if{latex}{\out{\hypertarget{method-waspas-summary}{}}}
\subsection{Method \code{summary()}}{
prepares summary of the WPM method resutls and outputs them
 to the console.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{waspas$summary()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-waspas-clone"></a>}}
\if{latex}{\out{\hypertarget{method-waspas-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{waspas$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
