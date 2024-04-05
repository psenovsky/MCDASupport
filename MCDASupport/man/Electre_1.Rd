\name{Electre_1}
\alias{Electre_1}
\title{Electre_1 : ELECTRE I method used to solve multiple criteria decision making}
\description{The acronym ELECTRE stands for: ELimination Et Choix Traduisant la REalite (ELimination and Choice Expressing REality). This method is based on the concept of concordance and discordance.

ELECTRE I does not provide rank, but allows the user to identify so called kernel of the decision - the alternatives, which cannot be eleminated from decision making as obviously inferios, thus simplifying the decision making problem. This is a core concept for variety of the methods in ELECTRE family, so much so, that the kernel computation procedure is also used in \code{\link{Electre_2}} and \code{\link{Electre_1S}} methods, so much so that the procedure was separated into \code{\link{ELECTRE1_Kernel}} function to be utilized by all these methods.

Similarly the Concordance and discordance indexes, the outranking for the alternatives is based on are more general concept shared by other methods. Computational procedures for them are available in \code{\link{ELECTRE_ConcordanceMatrix}} and \code{\link{ELECTRE_DiscordanceMatrix}} function.

Computation procedure for the method is realized in following steps:

\itemize{
  \item compute concordance matrix C using \code{\link{ELECTRE_ConcordanceMatrix}}
  \item compute discordance matrix D using \code{\link{ELECTRE_DiscordanceMatrix}}
  \item compute dominance matrix by comparing values of C to value of concordance threshold and D to discordance threshold
  \item compute solution's kernel based on dominance matrix using \code{\link{ELECTRE1_Kernel}} function.
}

Concordancce matrix (index) measures strength of the statement that alternative a outranks alternative b, while discordance matrix (index) together with discordance threshold (exceeding this threshold) can prevent such outranking.

Concoradce matrix C(a,b) is defined as:

\mjsdeqn{C(a,b) = \frac{1}{W} \sum_{\forall j: g_j(a) \ge g_j(b)}w_j}

where

\mjsdeqn{W = \sum_{j=1}^{n}w_j}

While discoradce matrix D(a,b) is defined for:

\mjsdeqn{g_j(a) \ge g_j(b) \forall j: D(a,b) = 0}

and for everything else:

\mjsdeqn{D(a,b) = \max_{j} \frac{g_j(b)-g_j(a)}{\delta_j}}

where

\mjsdeqn{\delta_j = \max_{j} g_j(a)-g_j(b)}

Where a, b ... are alternatives to be compared, \mjseqn{g_j(x)} ... performance of alternative x in criterium j, C ... comcordance matrix consisting of concordance indexes C(a,b), D ... discordance matrix consisting of discordance indexes D(a,b), \mjseqn{w_j} ... weight of criterium j and \mjseqn{\delta_j} is maximal difference of alternatives a and b preformance across the criteria.

Alternative a strongly outranks alternative b if and only in value of their concordance index is greater or equal to concordance threshold and at same time discordance index of this comparison is lower or equal do discordance index. In such case value in dominance matrix will be equal to 1 (aSb). Otherwise the value will be 0.

\mjsdeqn{Dom(a,b) = \left\lbrace\begin{array}{ll} 1 & iff \; C(a,b) \ge c_{thres} \; and \; D(a,b) \le d_{thres} \cr 0 & otherwise\end{array}\right.}

Kernel K is defined as a subset of alternatives (nodes in graph) which are incomparable in terms of stron outranking (aSb), and the alternatives not contained in K are outranked by at least one alternative belonging to K. Kernel is being computed from dominance matrix. We compute sums of rows for the dominance matrix and select the alternatives, where this sum is equal to 0 as a basis for the kernel estimation. If there is no such alternative, that there is no clear kernel.

Othervise more throughtfull exploration of the dominance relation is needed.

Technically the function implements approach authored by M. Balamurali in pyDecisions (in Python) and reimplements it in R.
}
\usage{
Electre_1(PM,
			w,
			minmaxcriteria,
			concordance_threshold = 1,
			discordance_threshold = 0,
    VERBOSE = FALSE)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. only numeric values expercted. Rows and columns are expected to be named.}
  \item{w}{vector containing the weights of the criteria.}
  \item{minmaxcriteria}{criteriaMinMax Vector containing the preference direction on each of the criteria. "min" (resp."max") indicates that the criterion has to be minimized (maximized).}
  \item{concordance_threshold}{parameter defining concordance threshold . The default value is 1. The user can set a new value between 0 and 1}
  \item{discordance_threshold}{parameter defining discordance threshold . The default value is 0. The user can set a new value between 0 and 1.}
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
	The function returns a list structured as follows :
	\item{PerformanceMatrix}{A matrix containing the performance table. Each row corresponds to an alternative, and each column to a criterion}
	\item{ConcordanceMatrix}{Concordance matrix is one of two working relations (concordance and discordance) which are subsequently used to construct the final dominance relation. For an outranking aSb to be validated, a sufficient majority of criteria should be in favor of this assertion.}
	\item{DiscordanceMatrix}{Discordance matrix is one of two working relations (concordance and discordance) which are subsequently used to construct the final dominance relation. The concept of discordance is complementary to the one of (concordance and represents the discomfort experienced in the choosing of alternative a above alternative b}
	\item{PreferenceMatrix}{A matrix with computed preferences for the alternatives. Represents domination relation between the alternatives. Provides value 1 where for relation between alternatives where such domination exists}
	\item{Kernel}{Kernel consist of subset of non-dominated alternatives}
  \item{Dominated}{Identified clearly dominated alternatives.}
  \item{GraphResult}{Visualization of the preference matrix using network graph. A -> B means, that A outranks B.}
}

\seealso{
  \itemize{
    \item \code{\link{ELECTRE1_Kernel}}
    \item \code{\link{Electre_2}}
    \item \code{\link{Electre_1S}}
    \item \code{\link{ELECTRE_ConcordanceMatrix}}
    \item \code{\link{ELECTRE_DiscordanceMatrix}}
  }
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
rownames(PM) <- c("CBX16","P205G","P405M","P605S","R4GTL",
  "RCLIO","R21TS","R21TU","R25BA","ALPIN")
colnames(PM) <- c("Prix","Vmax","C120","Coff","Acce","Frei","Brui")
minmaxcriteria <-c("min","max","min","max","min","min","min")
w <- c(0.3,0.1,0.3,0.2,0.1,0.2,0.1)
M <- Electre_1(PM, w, minmaxcriteria,
  concordance_threshold = 0.8,
  discordance_threshold = 0.1)
}

\keyword{ ELECTRE I }
\keyword{ concordance matrix }
\keyword{ discordane matrix }
\keyword{ kernel }
