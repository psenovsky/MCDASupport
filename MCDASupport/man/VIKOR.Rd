\name{VIKOR}
\alias{VIKOR}
\title{VIKOR : method used to solve multiple criteria decision making}
\description{The acronym VIKOR stands for: VlseKriterijumska Optimizacija I Kompromisno Resenje, in Serbian multicriteria optimization and compromise solution. The method has been especially designed to deal with problematic situations when the alternatives are characterized by non-commensurable and conflicting criteria, for which VIKOR provides compromise solution. Methodologically VIKOR is close to another method TOPSIS. Original VIKOR uses five steps to derive such compromise solution.

Step 1: determine best and worst values of all criteria by serchinch for min and max values in the performance matrix for all criteria.

\mjsdeqn{f^*_j = max_i f_{ij}}

\mjsdeqn{f^-_j = min_i f_{ij}}

Step 2: compute values of Si and Ri

\mjsdeqn{S_i = \sum_{j=1}^n \frac{w_j(f^*_j - f_{ij})}{f^*_j - f^-_j}, i = 1,2,...,m, j = 1,2,...,n}

\mjsdeqn{R_i = max_i \frac{w_j(f^*_j - f_{ij})}{f^*_j - f^-_j}, i = 1,2,...,m, j = 1,2,...,n}

where n is number of criteria, m number of alternatives and w are criteria weights.

Step 3: compute values of Qi

\mjsdeqn{Q_i = v \frac{S_i - S^*}{S^- - S^*} + (1 - v) \frac{R_i - R^*}{R^- - R^*}, i = 1,2,...,m}

where \mjseqn{S^* = min_i S_i}, \mjseqn{S^- = max_i S_i}, \mjseqn{R^* = min_i R_i}, \mjseqn{R^- = max_i R_i} and v is the weight for strategy of majority of the criteria.

Note that the v has in Qi connection to 1 - v (individual regret). By specifying various values of v one can influence impact left or right term of the equation has on overal result. \mjseqn{v \in (0,1)}, where v = 0.5 means balance between both terms. If function's parameter v is not set the procedure will approximate its value by:

\mjsdeqn{v = \frac{n + 1}{2n}}

Step 4: order alternatives by S, R, Q

Step 5: propose compromise solution

The compromise solution is identified by order of Q, looking at the S or R depending on v.If the winner is the same, we have solution. Otherwise compromise solution needs to be made using 1/(m - 1) value as test criterion.Compromise is formed from all alternatives in Q where the difference between first and tested value is lover than this criterion.

}
\usage{
VIKOR(PM, w, minmax = 'max', v = NULL, VERBOSE = FALSE)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Only numeric values expercted. Rows and columns are expected to be named.}
  \item{w}{vector containing the weights of the criteria.}
  \item{minmax}{criteria MinMax Vector containing the preference direction on each of the criteria. "min" (resp."max") indicates that the criterion has to be minimized (maximized).}
  \item{v}{weight for strategy of majority of the criteria in interval (0-1). If no value provided procedure assigns value 0.5}
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
	The function returns a list structured as follows :
	\item{S}{ordered list of alternatives using S-metric}
	\item{R}{ordered list of alternatives using R-metric}
	\item{Q}{ordered list of alternatives using Q-metric}
  \item{compromiseSolution}{list of alternatives forming compromise solution (based on Q, S, R metrics)}
}

\references{
ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.

Papathanasiou, Jason, Ploskas, Nikolaos. Multiple Criteria Decision Aid Methods, Examples and Python Implementations. Springer, 173 p., ISBN 978-3-319-91648-4.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
PM <- cbind(
  c(8,7,2,1),
  c(5,3,7,5),
  c(7,5,6,4),
  c(9,9,7,3),
  c(11,10,3,7),
  c(6,9,5,4)
)
colnames(PM) <- c('Site 1', 'Site 2', 'Site 3', 'Site 4', 'Site 5', 'Site 6')
rownames(PM) <- c('Investment costs (million EUR)', 'Employment needs (hundred employees)',
                  'Social impact (1-7)', 'Environmental impact (1-7)')
PM <- t(PM)
minmax <- 'max'
w <- c(0.4, 0.4, 0.1, 0.2)
v <- 0.5
result <- VIKOR(PM, w, minmax, v)
}
\keyword{ VIKOR }
