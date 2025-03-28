% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moosra.R
\name{moosra}
\alias{moosra}
\title{Multi-Objective Optimization on the basis of Simple Ratio Analysis}
\description{
The method is very similar to \link{moora}. It uses same approach to
 normalization

\mjsdeqn{\overline{x_{ij}} = \frac{x_{ij}}{\sqrt{\sum_{j=1}^m x_{ij}^2}}}

Again all criteria regardless of whether beneficial or cost are normalized
 using same equation.

MOORA solved this by computing difference betweeen sums of beneficial and
 cost criteria. MOOSRA on the other hand computes the ratios of these.

\mjsdeqn{v_i = \frac{\sum_{j in bc} w_j r_{ij}}{\sum_{j in cc} w_j r_{ij}}}

Where bc are benefit and cc are cost criteria.
}
\examples{

## ------------------------------------------------
## Method `moosra$new`
## ------------------------------------------------

# from: https://www.youtube.com/watch?v=wQD8df_jVF0
alternatives <- c("car 1", "car 2", "car 3", "car 4", "car 5", "car 6",
 "car 7", "car 8", "car 9", "car 10")
criteria <- c("quality", "condition", "security", "delivery days",
 "fuel consumption", "price")
M <- rbind(
  c(3, 6, 4, 20, 2, 30000),
  c(4, 4, 6, 15, 2.2, 32000),
  c(6, 5, 9, 18, 3, 32100),
  c(5, 6, 3, 23, 2.8, 28000),
  c(4, 8, 7, 30, 1.5, 29000),
  c(8, 3, 6, 35, 1.9, 27000),
  c(7, 2, 5, 33, 1.7, 28500),
  c(3, 8, 3, 34, 1.6, 30500),
  c(8, 4, 8, 40, 2.5, 33000),
  c(9, 3, 7, 34, 2, 29800)
)
rownames(M) <- alternatives
colnames(M) <- criteria
w <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.3)
minmax <- c("max", "max", "max", "min", "min", "min")
t <- moosra$new(M, w, minmax)
}
\references{
ULUTAŞ, Alptekin et al. Developing of a Novel Integrated MCDM MULTIMOOSRAL
 Approach for Supplier Selection. Informatica. 2021, vol. 32, no. 1,
 pp. 145–161, available from: https://doi.org/10.15388/21-INFOR445,
 ISSN 0868-4952, 1822-8844.
}
\author{
Pavel Šenovský \email{pavel.senovsky@vsb.cz}
}
\keyword{MOORA}
\keyword{MOOSRA}
\keyword{WSM}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{pm_orig}}{original (not normalized) performance matrix}

\item{\code{pm}}{normalized performance matrix}

\item{\code{w}}{vector of weights (sum of weights = 1)}

\item{\code{v}}{normalized assessment of alternatives with respect to all
objectives}

\item{\code{v_sorted}}{sorted normalized assessment of alternatives with
respect to all objectives from best to worst.}

\item{\code{minmax}}{vector specifying optimalization direction for the
criteria. Values max/min are expected. If all criteria are optimalized
in same direction the vector can be replaced by single value. Max value
is default.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-moosra-new}{\code{moosra$new()}}
\item \href{#method-moosra-compute}{\code{moosra$compute()}}
\item \href{#method-moosra-summary}{\code{moosra$summary()}}
\item \href{#method-moosra-clone}{\code{moosra$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-moosra-new"></a>}}
\if{latex}{\out{\hypertarget{method-moosra-new}{}}}
\subsection{Method \code{new()}}{
Public constructor for the class. Checks validity of input parameters
 and performs computation of of the model based on them.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{moosra$new(pm, w, minmax = "max")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pm}}{performance matrix (alternatives in rows and criteria in
columns)}

\item{\code{w}}{vector of weights, its sum must be equal to 1}

\item{\code{minmax}}{minmax vector specifying optimalization direction for the
criteria. Values max/min are expected. If all criteria are optimalized
in same direction the vector can be replaced by single value. Max value
is default.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# from: https://www.youtube.com/watch?v=wQD8df_jVF0
alternatives <- c("car 1", "car 2", "car 3", "car 4", "car 5", "car 6",
 "car 7", "car 8", "car 9", "car 10")
criteria <- c("quality", "condition", "security", "delivery days",
 "fuel consumption", "price")
M <- rbind(
  c(3, 6, 4, 20, 2, 30000),
  c(4, 4, 6, 15, 2.2, 32000),
  c(6, 5, 9, 18, 3, 32100),
  c(5, 6, 3, 23, 2.8, 28000),
  c(4, 8, 7, 30, 1.5, 29000),
  c(8, 3, 6, 35, 1.9, 27000),
  c(7, 2, 5, 33, 1.7, 28500),
  c(3, 8, 3, 34, 1.6, 30500),
  c(8, 4, 8, 40, 2.5, 33000),
  c(9, 3, 7, 34, 2, 29800)
)
rownames(M) <- alternatives
colnames(M) <- criteria
w <- c(0.1, 0.2, 0.1, 0.2, 0.1, 0.3)
minmax <- c("max", "max", "max", "min", "min", "min")
t <- moosra$new(M, w, minmax)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-moosra-compute"></a>}}
\if{latex}{\out{\hypertarget{method-moosra-compute}{}}}
\subsection{Method \code{compute()}}{
performs computation of MOOSRA model based on class properties. Usually
 is not run manually as constructor calls this method automatically.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{moosra$compute()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-moosra-summary"></a>}}
\if{latex}{\out{\hypertarget{method-moosra-summary}{}}}
\subsection{Method \code{summary()}}{
prepares summary of the MOORA method resutls and outputs them
 to the console.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{moosra$summary()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-moosra-clone"></a>}}
\if{latex}{\out{\hypertarget{method-moosra-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{moosra$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
