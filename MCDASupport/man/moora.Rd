% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moora.R
\name{moora}
\alias{moora}
\title{Multi-Objective Optimization on the basis of Ratio Analysis}
\description{
Another method in family of \ling{wsm}-based methods.

This time around the method uses vector normalization for benefit criteria.
 This form of normalization is used on both benefit and cost criteria.

\mjsdeqn{\overline{x_{ij}} = \frac{x_{ij}}{\sqrt{\sum_{j=1}^m x_{ij}^2}}}

Then it computes weighted assessment of the alternatives by computing
 difference between g weighted benefit criteria and n cost criteria.

\mjsdeqn{y_j = \sum_{i=1}^g \overline{x_{ij}} - \sum_{i = 1}^n \overline{x_{ij}}}
}
\examples{

## ------------------------------------------------
## Method `moora$new`
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
w = c(0.1, 0.2, 0.1, 0.2, 0.1, 0.3)
minmax <- c("max", "max", "max", "min", "min", "min")
t <- moora$new(M, w, minmax)
}
\references{
Brauers, W.K.M., Zavadskas, E.K. The MOORA method and its application to
 privatization in a transition economy. Control and Cybernetics. Vol. 32
 (2006), no. 2, 445-469
}
\author{
Pavel Šenovský \email{pavel.senovsky@vsb.cz}
}
\keyword{MOORA}
\keyword{WSM}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{pm_orig}}{original (not normalized) performance matrix}

\item{\code{pm}}{normalized performance matrix}

\item{\code{w}}{vector of weights (sum of weights = 1)}

\item{\code{y}}{normalized assessment of alternatives with respect to all
objectives}

\item{\code{y_sorted}}{sorted normalized assessment of alternatives with
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
\item \href{#method-moora-new}{\code{moora$new()}}
\item \href{#method-moora-compute}{\code{moora$compute()}}
\item \href{#method-moora-summary}{\code{moora$summary()}}
\item \href{#method-moora-clone}{\code{moora$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-moora-new"></a>}}
\if{latex}{\out{\hypertarget{method-moora-new}{}}}
\subsection{Method \code{new()}}{
Public constructor for the class. Checks validity of input parameters
 and performs computation of of the model based on them.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{moora$new(pm, w, minmax = "max")}\if{html}{\out{</div>}}
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
w = c(0.1, 0.2, 0.1, 0.2, 0.1, 0.3)
minmax <- c("max", "max", "max", "min", "min", "min")
t <- moora$new(M, w, minmax)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-moora-compute"></a>}}
\if{latex}{\out{\hypertarget{method-moora-compute}{}}}
\subsection{Method \code{compute()}}{
performs computation of MOORA model based on class properties. Usually
 is not run manually as constructor calls this method automatically.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{moora$compute()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-moora-summary"></a>}}
\if{latex}{\out{\hypertarget{method-moora-summary}{}}}
\subsection{Method \code{summary()}}{
prepares summary of the MOORA method resutls and outputs them
 to the console.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{moora$summary()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-moora-clone"></a>}}
\if{latex}{\out{\hypertarget{method-moora-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{moora$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
