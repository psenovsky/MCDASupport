\name{Electre_2}
\alias{Electre_2}
\title{Electre_2 : ELECTRE II method used to solve multiple criteria decision making}
\description{The acronym ELECTRE stands for: ELimination Et Choix Traduisant la REalite (ELimination and Choice Expressing REality). ELECTRE II method is then designed for ranking purposes as oposed to ELECTRE I method which is useful for identification of kernel - as alternatives, which cannot be eliminated, are not obviously dominated by other alternatives.

ELECTRE II uses concordance and discordance indexes to construct two partial pre-orders by trying do formally describe so called strong and weak preferences between the alternatives. Strenght is this regard is taken as strength of belief, that alternative a is better then alternative b.

Two partial preorders and then used in aggregation procedure to construct final patrial preorder.

The code is partially inspired by code authored by M. Balamurali in pyDecisions (in Python) and reimplements it in R, thou some portions of the function are solved differently in here. For example whole graph simplification process in here relies on functionality of iGraph package. This method also differently implements the final recommendation (total partial order).

Considering that the ELECTRE II method is based on ELECTRE I method with additions to produce ranking, the computation starts with computing concordance \code{\link{ELECTRE_ConcordanceMatrix}} and discordance \code{\link{ELECTRE_DiscordanceMatrix}} matrixes. See \code{\link{Electre_1}} documentation for mathematical aparatus.

Based on concordance and discordance matrixes and comparing its values with the thresholds, we can establish strong \mjseqn{dom_s} or weak \mjseqn{dom_w} dominance.

\mjsdeqn{dom_s(a,b) = \left\lbrace\begin{array}{ll} 1 & if \; (C(a,b) \ge c^+ \; and \; d(a,b) \le d^+) \; or \; (C(a,b) \ge c^0 \; and \; d(a,b) \le d^-) \cr 0 & otherwise\end{array}\right.}

\mjsdeqn{dom_w(a,b) = \left\lbrace\begin{array}{ll} 1 & if \; (C(a,b) \ge c^0 \; and \; d(a,b) \le d^+) \; or \; (C(a,b) \ge c^- \; and \; d(a,b) \le d^-) \cr 0 & otherwise\end{array}\right.}

Next preference ratio S(a,b) needs to be computed for \mjseqn{dom_s} and \mjseqn{dom_w} matrixes. We establish sum of weights of criteria \mjseqn{w^+}, where a outperforms b and sum of criteria \mjseqn{w^-}, where the opposite applies.

\mjsdeqn{w^+ = \sum_{\forall k: PM_k(a)>PM_k(b)} w_k}

and

\mjsdeqn{w^- = \sum_{\forall k: PM_k(a)<PM_k(b)} w_k}

\mjsdeqn{S(a,b) = \left\lbrace\begin{array}{ll} 1 & if \; w^- = 0 \; or \; \frac{w^+}{w^-} \ge 1 \cr 0 & otherwise\end{array}\right.}

Technically the computation needs to be performed only on pairs of (a,b) where either \mjseqn{dom_s} and or \mjseqn{dom_w} showed possibility of a outranking b, otherwise we can directly establish, that S(a,b) = 0.

Strong and weak dominances are used to then to form first and second total preorder. Both matrixes need to be simplified to remove loops from it first.

These preorders aim to measure the haw much alternatives over-rank each other. The procedure is same for both orders. First we consolidate information on dominances by

\mjsdeqn{dom = 2 \cdot dom_s + dom_w}

Then we create from all options not strongly outranked by any other option the set D. Those options within D that are linked to each other by a weak outranking relationship constitute the set U. The set B consists of all options from U not weakly outranked by any other from within U. The single option or group of equally ranked options is defined by the union of the sets (D - U) and B. This provides enough information to form full ranking.

Second preorder uses same approach described in previous paragraph, but in reversed order. We achieve this by providing ranking algorithm transposed dominance matrixes and reversing resulting order.

Since we got two separate preorders, agregation procedure process these into single ranking order. To do that some simple rules are used:

\itemize{
  \item if a is preferred to b in both pre-orders, then this will also be the case in final order
  \item if a has an equivalent ranking to b in the one of the complete pre-orders, but is preferred in the other, then a precedes b in the final order
  \item if a is preferred to b in the first complete pre-order, but b is preferred to a in the second, then the two alternatives are incomparable in the final order.
}
}
\usage{
Electre_2(PM,
  w,
  minmaxcriteria = 'max',
  c_minus = 0.65,
  c_zero = 0.75,
  c_plus = 0.85,
  d_minus = 0.25,
  d_plus = 0.5,
  VERBOSE = FALSE)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. only numeric values expercted. Rows and columns are expected to be named.}
  \item{w}{vector containing the weights of the criteria.}
  \item{minmaxcriteria}{criteriaMinMax Vector containing the preference direction on each of the criteria. "min" (resp."max") indicates that the criterion has to be minimized (maximized).}
  \item{c_minus}{first of three concordance threshold parameters. ELECTRE II method implements the concotdance threshold as fuzzy triangle. Value is alway between 0-1 and 0 <= c- <= c0 <= c+ <= 1, default 0.65.}
  \item{c_zero}{second of three concordance threshold parameters. ELECTRE II method implements the concotdance threshold as fuzzy triangle. Value is alway between 0-1 and 0 <= c- <= c0 <= c+ <= 1, default 0.75.}
  \item{c_plus}{third of three concordance threshold parameters. ELECTRE II method implements the concotdance threshold as fuzzy triangle. Value is alway between 0-1 and 0 <= c- <= c0 <= c+ <= 1, degault 0.85.}
  \item{d_minus}{first of two parameters defining discordance threshold. Thershold is defined as range, where 0 <= d- <= d+ <= 1,default value 0.25.}
  \item{d_plus}{first of two parameters defining discordance threshold. Thershold is defined as range, where 0 <= d- <= d+ <= 1,default value 0.25.}
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
	The function returns a list structured as follows:
	\item{PerformanceMatrix}{A matrix containing the performance table. Each row corresponds to an alternative, and each column to a criterion}
	\item{ConcordanceMatrix}{Concordance matrix is one of two working relations (concordance and discordance) which are subsequently used to construct the final dominance relation. For an outranking aSb to be validated, a sufficient majority of criteria should be in favor of this assertion.}
	\item{DiscordanceMatrix}{Discordance matrix is one of two working relations (concordance and discordance) which are subsequently used to construct the final dominance relation. The concept of discordance is complementary to the one of (concordance and represents the discomfort experienced in the choosing of alternative a above alternative b}
	\item{StrongOutranking}{A graph, expresed as adjancancy matrix of alternative for which it was possible to establish strong outranking relation. (We strongly believe that alternative a is better then alternative b: aSFb).}
	\item{WeakOutranking}{A graph, expressed as adjancancy matrix of alternatives for which it was possible to establish weak outranking relation. Similar to strong outranking, only our belief of a outranking b is much weaker: aSfb.}
	\item{firstTotalPreorder}{first total preorder V1}
	\item{secondTotalPreorder}{second total preorder V2}
	\item{finalPreorderMatrix}{final partial preorder expressed as adjancancy matrix.}
	\item{incomparableAlternatives}{adjacancy matrix identifying the alternatives, which cannot be directly compared (are incomparable).}
	\item{graphResult}{plot of finalPreorderMatrix}
	\item{finalPreorder}{provides unsorted vector of alternatives, with identification of how many times was the alternative preffered to other alternatives.}
	\item{finalPreorderSorted}{ordered vector of alternatives from best to worst, with identification of how many times was the alternative preffered to other alternatives.}
}

\references{
Balamurali, M.: pyDecisions - A Python Library of management decision making techniques. Avilable on-line from \url{https://github.com/Valdecy/pyDecisions}

Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7

Prombo, M. Package OutrankingTools, CRAN: 2015, available from: \url{https://cran.r-project.org/web/packages/OutrankingTools/}
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
M <- Electre_2(PM, w, minmaxcriteria)
}

\keyword{ ELECTRE II }
\keyword{ strong outranking }
\keyword{ weak outranking }
\keyword{ concordance matrix }
\keyword{ discordance matrix }

