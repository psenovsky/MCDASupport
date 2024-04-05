\name{Electre_3}
\alias{Electre_3}
\title{ ELECTRE III method for ranking alternatives}
\description{ELECTRE III  method aims to answer the following question: considering a finite set of actions, A, evaluated on a coherent family of pseudo-criteria, F, how to make a partition of A in classes of  quivalence and provide a necessarily complete pre-order expressing the relative position of these classes? In the first phase, ELECTRE III method involves the construction of a fuzzy outranking relation.

In the second phase, an algorithm is used for making a ranking in a final partial pre-order, that combines two complete pre-orders.

This implementation presents simplified version of the method using single value alpha, beta parameters. If more complex approach to ELECTRE III computation is required use OutrankingTools package for R, which provides variant allowing such computations.

The procedure itself is as follows. First we compute Concordance matrix. Concordancce matrix (index) measures strength of the statement that alternative a outranks alternative b.

To compute concordance matrix, partial concordance matrix \mjseqn{c_j} of the criteria needs to be computed first. The partial concordance matrix considers concordance by comparing performances of the alternatives in criterium \mjseqn{j}.

\mjsdeqn{c_j(a,b) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(a) + Q_j(a) \ge PM_j(b) \cr 0 & \;if\; PM_j(a) + P_j(a) < PM_j(b)) \cr \frac{PM_j(a) - PM_j(b) + P_j(a)}{P_j(a) - Q_j(a)} & else\end{array}\right.}

Based on partial concordance matrixes we compute concordence matrix \mjseqn{C} as:

\mjsdeqn{C(a,b) = \frac{\sum (w_j \cdot c_j(a,b))}{\sum w_j}}

Then we compute discordance matrixes \mjseqn{d_j} separately for each criterion using:

\mjsdeqn{d_j(a,b) = \left\lbrace\begin{array}{ll} 1 & \;if\; PM_j(b) > PM_j(a) + V_j(a) \cr 0 & \;if\; PMj(b) \le PM_j(a) + P_j(a) \cr \frac{PM_j(b) - PM_j(a) - P_j(a)}{V_j(a) - P_j(a)} & else\end{array}\right.}

where

PM ... performance of alternative in criterion, Q ... indifference threshold, P ... prefference threshold, V ... veto threshold, w ... weights.

Based on discrodance matrixes \mjseqn{d_j} we compute credibility matrix \mjseqn{S}

\mjsdeqn{S(a,b) = \left\lbrace\begin{array}{ll} C(a,b) & \;if\; d_j(a,b) \le C(a,b) \cr C(a,b) \cdot \prod_{\forall j \in J} \frac{1 - d_j(a,b)}{1 - C(a,b)} & \;otherwise\; \end{array}\right.}

where J is set of criteria \mjseqn{j} for which \mjseqn{d_j(a,b) > C(a,b)}

Finaly we may use the information to derive ranking of the alternatives.To do that we will perform descending rank distilation \code{\link{Electre_desc_dist}}, followed by ascending rank distilation \code{\link{Electre_asc_dist}}, then we agregate both into final rank \code{\link{finalRanking}}.

Preorder for descending rank distilation is achieved by distilling alternatives from credibility matrix using progresively lower cutoff thresholds \mjseqn{\lambda}. first value of the \mjseqn{\lambda} is computed:

\mjsdeqn{\lambda_1 = max_{a,b \in A}S(a,b)}

where A ... is set of alternatives.

Ascending distilation works similarly, but going up from lowest values in credibility matrix using progressively higher cutoff thresholds \mjseqn{\lambda}.

Since both preorders can be different gathered information needs to be further processed by creation of pre-order matrix. This matrix provides comparison between all alternatives by comparing their mutual rankings in descending and ascending preorders.

Suppose we compare alternatives a and b. We already have available its ranks in descending (\mjseqn{D_a, D_b}) and ascending (\mjseqn{A_a, A_b}) preorders. We will use this information to derive relation between the two alternatives.

If (\mjseqn{D_a < D_b} and at same time \mjseqn{A_a < A_b}) or (\mjseqn{D_a == D_b} and at same time \mjseqn{A_a < A_b})  or  (\mjseqn{D_a < B_b} and at same time \mjseqn{A_a == A_b}) we can say that that a is prefered to b (aPb). In other words aPb if it is either preffered in both preorders or when it is better ranked in one while being evaluated as equal in other.

Since we work with matrix we use [a, b] = P+ and [b, a] = P- notation to express it.

Same rules apply for bPa, leading to [a, b] = P- and [b, a] = P+.

We can also say that if the ranks of the alternatives in both preorders are same, then we are indifferent to them preferantion wise: aIb, leading to [a, b] = [b, a] = I.

Otherwise the alternatives are incomparable: aRb, leading to [a, b] = [b, a] = R. That can hapen if in one preorder aPb, while in second one bPa.

This information is directly usable for final ranking derivation.

Please note, that ELECTRE IV (see \code{\link{Electre_4}}) uses same procedure for rank derivation, but does not use weights.

}
\usage{
Electre_3(PM, w, P, Q, V, minmaxcriteria = 'max', alpha = 0.3,
beta = 0.15, VERBOSE = FALSE)
}

\arguments{
  \item{PM}{Performance matrix as matrix or data frame containing the performance table. Data frame with named columns (criteria) and rows (alternatives) is prefered.}
  \item{w}{vector of weights. Number of weights must be equal to number of criteria used in decision making.}
  \item{P}{Preference threshold - Vector containing the preference thresholds constraints defined for each criterion.}
  \item{Q}{Indifference threshold - Vector containing the indifference thresholds constraints defined for each criterion.}
  \item{V}{Veto threshold - Vector containing the veto thresholds constraints defined for each criterion.}
  \item{minmaxcriteria}{Vector containing the preference direction on each of the criteria either min or max. If all criteria are to be minimized or maximized, single min resp. max value can be used.}
  \item{alpha}{alpha and beta coefficients are used in downward and upward distilation procedure as wel as final ranking procerude to construct orders. Both coef. are used iteratively change thresholds limiting evaluation of the outranking relations between the alternatives.}
  \item{beta}{see alpha. Preset values of alpha = 0.3 and beta = 0.15 are set as per Vallee and Zielniewicz (1994). This version of method allows only to set single value for the purpose.}
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
	The function returns a list structured as follows:
	\item{PM}{Performance Matrix - A matrix containing the performance table. Each row corresponds to an alternative, and each column to a criterion}
	\item{cm}{Concordance matrix is one of two working relations (concordance and discordance) which are subsequently used to construct the final dominance relation. For an outranking aSb to be validated, a sufficient majority of criteria should be in favor of this assertion.}
	\item{DiscordanceMatrixCriteria}{Discordance matrix is one of two working relations (concordance and discordance) which are subsequently used to construct the final dominance relation. The concept of discordance is complementary to the one of (concordance and represents the discomfort experienced in the choosing of alternative a above alternative b. In comparison with ELECTRE I or II methods discordance matrix for ELECTRE III is computed separately for every criterion.}
	\item{CredibilityMatrix}{matrix assessing the strength of the assertion that a is at least as good as b.}
	\item{rank_D}{Descending distillation ranking - final partial preorder orders the alternatives from the best
to the worst.}
  \item{rank_A}{Ascending distillation ranking - final partial preorder orders the alternatives from worst to best.}
  \item{rank_P}{Pre-order matrix specifying identified relations between the alternatives - values are P+ (a prefered to b), P- (b prefered to a), I (indifferent), R (incomparable)}
  \item{adjancancyMatrix}{Adjancency Matrix allows to visualize results as network diagram}
  \item{graph}{processed adajncency matrix into network digram.}
  \item{final_ranking}{final order of the alternatives.}
  \item{finalRankingUnsorted}{named vector of unsorted order of the alternatives, added for purposes of sensitivity analysis}
}

\references{
Balamurali, M.: pyDecisions - A Python Library of management decision making techniques. Avilable on-line from \url{https://github.com/Valdecy/pyDecisions}

Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7

Roy B. : "The outranking approach and the foundations of ELECTRE methods", Theory and Decision 31, 1991, 49-73.

Vallée, D.; Zielniewicz, P. 1994. ELECTRE III-IV, version 3.x, Aspects Méthodologiques (tome 1), Guide
d’utilisation (tome 2). Document du LAMSADE 85 et 85bis, Université Paris Dauphine

Prombo, M. Package OutrankingTools, CRAN: 2015, available from: \url{https://cran.r-project.org/web/packages/OutrankingTools/}

Meyer, P. at al. MCDA package. GitHub: 2021, available from: \url{https://github.com/paterijk/MCDA/blob/master/}
}
\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
# the performance table
PM <- cbind(
  c(-14,129,-10,44,-14),
  c(90,100,50,90,100),
  c(0,0,0,0,0),
  c(40,0,10,5,20),
  c(100,0,100,20,40)
)
rownames(PM) <- c("Project1","Project2","Project3","Project4","Project5")
colnames(PM) <- c( "CR1","CR2","CR3","CR4","CR5")
minmaxcriteria <- 'max'
Q <- c(25,16,0,12,10) #Indifference thresholds
P <- c(50,24,1,24,20) #Preference thresholds
V <- c(100,60,2,48,90) #Veto thresholds
w <- c(1,1,1,1,1) #weights
Electre_3(PM, w, P, Q, V, minmaxcriteria)
}

\keyword{ELECTRE methods}
\keyword{ELECTRE III}
\keyword{Outranking approaches}
\keyword{preference modelling}
\keyword{multicriteria analysis}
\keyword{pseudo-criterion}
\keyword{Discrimination thresholds}
\keyword{global concordance matrix}
\keyword{credibility matrix}
