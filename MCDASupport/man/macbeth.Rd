% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/macbeth.R
\name{macbeth}
\alias{macbeth}
\title{Measuring Alternatives by Categorical Based Evaluation Technique}
\description{
Developed by Bana e Costa and Vansnick in 1990.

Method presumes independent criteria expressed in numeric forms, either
 directly or by translating semantic ordinal scale into numeric form.

This implementation presumes that all criteria are expressed in such way,
 that it is possible to see them as beneficial. Original method lets the
 decision maker to do this manual translation as first step of the method.
 Method also presumes the weights of the criteria are known and takes them as
 input.

First true step of the method is establishing reference levels for each
 criterium:

\mjsdeqn{r_j^- = min(r_{ij})}

\mjsdeqn{r_j^+ = max(r_{ij})}

then the MACBETH score (v) is computed:

\mjsdeqn{v(r_{ij} = v(r_j^-) + \frac{r_{ij} - r_j^-}{r_j^+ - r_j^-}[v(r_j^+) - v(r_j^-)]}

where \mjseqn{r_{ij}} represents performance of i-th alternative in j-th
 criterium. \mjseqn{v(r_j^-)} and \mjseqn{v(r_j^+)} represents lower and
 upper limit of the scale, in which we want the result v to be in and can
 for example be set to 0 and 100, which are default values for the parametrs
 in this implementation.

Overal score is then computer by:

\mjsdeqn{V_i = \sum_{j = 1}^n v(r_{ij}) \cdot w_j}

Vi then can be ordered in descending manner to identify ranking of the
 alternatives.
}
\examples{

## ------------------------------------------------
## Method `macbeth$new`
## ------------------------------------------------

alternatives <- c("A1", "A2", "A3", "A4")
criteria <- c("C1", "C2", "C3", "C4", "C5")
pm <- rbind(
  c(4, 3000, 200, 2, 1),
  c(3, 1800, 140, 3, 4),
  c(5, 2200, 230, 1, 3),
  c(2, 2500, 180, 4, 2)
)
rownames(pm) <- alternatives
colnames(pm) <- criteria
w <-c(0.116, 0.207, 0.242, 0.34, 0.095)
t <- macbeth$new(pm, w)
}
\references{
Alinezhad, A., Khalili, J. New Methods and Applications in Multiple
 Attribute Decision Making (MADM). Springer Nature Switzerland AG, 2019,
 233 p., ISBN 978-3-030-15009-9
}
\author{
Pavel Šenovský \email{pavel.senovsky@vsb.cz}
}
\keyword{MACBETH}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{pm}}{performance matrix. Criteria (in columns) are expected to be
ordered from most influential to least influential.}

\item{\code{w}}{weight of the criteria}

\item{\code{v_plus}}{ideal MACBETH score (100 by default)}

\item{\code{v_minus}}{anti-ideal MACBETH score (0 by default)}

\item{\code{v}}{MACBETH score}

\item{\code{finalRank}}{final ranking of the alternatives}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-macbeth-new}{\code{macbeth$new()}}
\item \href{#method-macbeth-compute}{\code{macbeth$compute()}}
\item \href{#method-macbeth-summary}{\code{macbeth$summary()}}
\item \href{#method-macbeth-clone}{\code{macbeth$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-macbeth-new"></a>}}
\if{latex}{\out{\hypertarget{method-macbeth-new}{}}}
\subsection{Method \code{new()}}{
class constructor, validates data and computes the model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{macbeth$new(pm, w, v_min = 0, v_max = 100)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pm}}{performance matrix. Criteria in columns, ordered from most to
least influential}

\item{\code{w}}{weight of the criteria}

\item{\code{v_min}}{minimal MACBETH score (0 by default)}

\item{\code{v_max}}{maximal MACBETH score (100 by default)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
instance of the class including computed model
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{alternatives <- c("A1", "A2", "A3", "A4")
criteria <- c("C1", "C2", "C3", "C4", "C5")
pm <- rbind(
  c(4, 3000, 200, 2, 1),
  c(3, 1800, 140, 3, 4),
  c(5, 2200, 230, 1, 3),
  c(2, 2500, 180, 4, 2)
)
rownames(pm) <- alternatives
colnames(pm) <- criteria
w <-c(0.116, 0.207, 0.242, 0.34, 0.095)
t <- macbeth$new(pm, w)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-macbeth-compute"></a>}}
\if{latex}{\out{\hypertarget{method-macbeth-compute}{}}}
\subsection{Method \code{compute()}}{
computes MACBETH model based on parameters of the fields of the class.
 Usually run automatically as part of class inititation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{macbeth$compute()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-macbeth-summary"></a>}}
\if{latex}{\out{\hypertarget{method-macbeth-summary}{}}}
\subsection{Method \code{summary()}}{
prepares summary of the MACBETH method resutls and outputs them
 to the console.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{macbeth$summary()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-macbeth-clone"></a>}}
\if{latex}{\out{\hypertarget{method-macbeth-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{macbeth$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
