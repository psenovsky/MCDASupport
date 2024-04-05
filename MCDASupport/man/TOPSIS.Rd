\name{TOPSIS}
\alias{TOPSIS}
\title{TOPSIS : method used to solve multiple criteria decision making}
\description{The acronym TOPSIS stands for: Technique of Order Preference Similarity to the Ideal Solution. As name suggests TOPSIS provides its guidance based on evaluation of the similarity to both ideal and anti-ideal variant of the solution.

Original method uses 5 steps for the procedure. In first step the procedure normalizes values in performance matrix nad applies weights to it in step 2. In step 3 ideal variant \mjseqn{A^*} and anti-ideal variant \mjseqn{A^-} is computed as maximums and minimums of the criteria in performance matrix.

In step 4 distance to ideal \mjseqn{D^*} and anti-ideal variant \mjseqn{D^-} is computed and in step 5 used to compute closenes criterium

\mjsdeqn{C_i = \frac{D^-_i}{D^-_i + D^*_i}}

Criterium C is then directly usable to rank alternatives. C is always in interval of 0-1, the closer the value is to 1, the closer it is to ideal variant.
}

\usage{
TOPSIS(PM, w, minmax = 'max', VERBOSE = FALSE)
}

\arguments{
  \item{PM}{Matrix or data frame containing the performance table. Each row corresponds to an alternative, and each column to a criterion. Only numeric values expercted. Rows and columns are expected to be named.}
  \item{w}{vector containing the weights of the criteria.}
  \item{minmax}{criteria MinMax Vector containing the preference direction on each of the criteria. "min" (resp."max") indicates that the criterion has to be minimized (maximized).}
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
	The function returns a list structured as follows :
	\item{C}{ordered list of alternatives using C criterium}
	\item{normPM}{normalized performance matrix}
	\item{weightPM}{weighted normalized performance matrix}
	\item{A_ideal}{positive ideal solution}
	\item{A_anti}{anti ideal solution}
	\item{D_ideal}{alternative closeness to ideal variant}
	\item{D_anti}{alternative closeness to anti-ideal variant}
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
result <- TOPSIS(PM, w, minmax)
}
\keyword{ TOPSIS }
