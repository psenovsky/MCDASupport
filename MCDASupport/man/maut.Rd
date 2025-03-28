% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maut.R
\name{maut}
\alias{maut}
\title{Multi-Attribute Utility Theory}
\description{
Developed by Keeney and Raiffa in 1976. Provides a way to estimate overall
 utility of the alternatives characterized by the attributes (criteria).

Method presumes independent criteria expressed in numeric form. Criteria
 need to be independent. Method is also compensatory.

In first step the performance matrix (alternatives in rows, criteria in
 columns) is normalized using min-max normalization.

Next marginal utility score is computed:

\mjsdeqn{u_{ij} = \frac{exp(r_{ij}^2) - 1}{1.71}}

where \mjseqn{r_{ij}} is normalized value of performance of alternative i
 in criterium j.

Final utility score is then computer by:

\mjsdeqn{U_i = \sum_{j = 1}^n u_{ij} \cdot w_j}

Final ranking is derived from final utility score Ui by ordering in
 descending order.
}
\examples{

## ------------------------------------------------
## Method `maut$new`
## ------------------------------------------------

alternatives <- c("A1", "A2", "A3", "A4")
criteria <- c("C1", "C2", "C3", "C4")
pm <- rbind(
  c(429, 0.6, 5, 4),
  c(649, 0.7, 4, 5),
  c(459, 0.4, 1, 1),
  c(419, 0.5, 2, 2)
)
rownames(pm) <- alternatives
colnames(pm) <- criteria
w <-c(0.345, 0.35, 0.155, 0.15)
minmax <- c("min", "min", "max", "max")
t <- maut$new(pm, w, minmax)
}
\references{
Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
 Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
 233 p., ISBN 978-3-030-15009-9
}
\author{
Pavel Šenovský \email{pavel.senovsky@vsb.cz}
}
\keyword{MAUT}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{pm}}{performance matrix. Criteria (in columns) are expected to be
ordered from most influential to least influential.}

\item{\code{w}}{weight of the criteria}

\item{\code{minmax}}{max for benefit and min for cost criteria. Single min or
max value is also allowed in situation when all criteria share same
direction.}

\item{\code{utility_score}}{final utility score}

\item{\code{finalRank}}{final ranking of the alternatives}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-maut-new}{\code{maut$new()}}
\item \href{#method-maut-compute}{\code{maut$compute()}}
\item \href{#method-maut-summary}{\code{maut$summary()}}
\item \href{#method-maut-clone}{\code{maut$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-maut-new"></a>}}
\if{latex}{\out{\hypertarget{method-maut-new}{}}}
\subsection{Method \code{new()}}{
class constructor, validates data and computes the model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{maut$new(pm, w, minmax = "max")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pm}}{performance matrix. Criteria in columns, ordered from most to
least influential}

\item{\code{w}}{weight of the criteria}

\item{\code{minmax}}{max for benefit and min for cost criteria. Single min or
max value is also allowed in situation when all criteria share same
direction.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
instance of the class including computed model
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{alternatives <- c("A1", "A2", "A3", "A4")
criteria <- c("C1", "C2", "C3", "C4")
pm <- rbind(
  c(429, 0.6, 5, 4),
  c(649, 0.7, 4, 5),
  c(459, 0.4, 1, 1),
  c(419, 0.5, 2, 2)
)
rownames(pm) <- alternatives
colnames(pm) <- criteria
w <-c(0.345, 0.35, 0.155, 0.15)
minmax <- c("min", "min", "max", "max")
t <- maut$new(pm, w, minmax)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-maut-compute"></a>}}
\if{latex}{\out{\hypertarget{method-maut-compute}{}}}
\subsection{Method \code{compute()}}{
computes MAUT model based on parameters of the fields of the class.
 Usually run automatically as part of class inititation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{maut$compute()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-maut-summary"></a>}}
\if{latex}{\out{\hypertarget{method-maut-summary}{}}}
\subsection{Method \code{summary()}}{
prepares summary of the MAUT method resutls and outputs them
 to the console.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{maut$summary()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-maut-clone"></a>}}
\if{latex}{\out{\hypertarget{method-maut-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{maut$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
