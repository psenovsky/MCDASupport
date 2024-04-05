\name{FuzzyTOPSIS}
\alias{FuzzyTOPSIS}
\title{FuzzyTOPSIS : method used to solve multiple criteria decision making}
\description{The acronym TOPSIS stands for: Technique of Order Preference Similarity to the Ideal Solution. As name suggests TOPSIS provides its guidance based on evaluation of the similarity to both ideal and anti-ideal variant of the solution.

Original method uses 5 steps for the procedure. In first step the procedure normalizes values in performance matrix nad applies weights to it in step 2. In step 3 ideal variant \mjseqn{A^*} and anti-ideal variant \mjseqn{A^-} is computed as maximums and minimums of the criteria in performance matrix.

In step 4 distance to ideal \mjseqn{D^*} and anti-ideal variant \mjseqn{D^-} is computed and in step 5 used to compute closenes criterium (CC).

Criterium CC is then directly usable to rank alternatives. CC is always in interval of 0-1, the closer the value is to 1, the closer it is to ideal variant.

The approach described above is same as for TOPSIS method (see \code{\link{TOPSIS}}), thou in case of FuzzyTOPSIS triangular fuzzy numbers are used to describe the values in both criteria weights and performance matrix.

Note: in present version of the function only benefit criteria are being supported.

Mathematically main difference to TOPSIS is requirement to create crips number from triangular fuzzy number using following formula to compute perceived distance between the alternatives:

\mjsdeqn{ dist = \sqrt{\frac{(a_1 - b_1)^2  + (a_2 - b_2)^2 + (a_3 - b_3)^2}{3}}}

Where dist ... distance between alternatives a and b, both a and b are specifed in the form of triangle \mjseqn{a = (a_1, a_2, a_3)} and \mjseqn{b = (b_1, b_2, b_3)}.

Technically we compute the distance as square root of average square of differences between the alternatives.
}

\usage{
FuzzyTOPSIS(PM, dictionaryPM, w, dictionaryW, alt, VERBOSE = FALSE)
}

\arguments{
  \item{PM}{performance matrix with n columns and no. criteria x no. of alternatives rows. I. e. with 3 criteria and 2 alternatives the rows are: 1: 1.cri-1.alt., 2: 1.cri.-2.alt, 3: 2.cri-1.alt, 4: 2.cri-2.alt, ...}
  \item{dictionaryPM}{dictionary of linguistic variables for criteria}
  \item{w}{weights (matrix of weights - decision makers in columns and criteria in rows)}
  \item{dictionaryW}{dictionary for wights (single matrix|dataframe)}
  \item{alt}{vector with names of the alternatives}
  \item{VERBOSE}{if set to TRUE it produces all the outputs in the console, otherwise the results are available only as values returned by function.}
}

\value{
	The function returns a list structured as follows:
	\item{fuzzy_weights}{fuzzy weights of the criteria}
  \item{fuzzy_decision_matrix}{fuzzy decision matrix of the criteria}
  \item{fuzzy_norm_decision_matrix}{fuzzy normalized decision matrix}
  \item{weighted_fuzzy_norm_decision_matrix}{weighted fuzzy normalized decision matrix}
  \item{a_plus}{ideal solution}
  \item{a_minus}{anti-ideal solution}
  \item{CC}{list of alternatives with computed closeness criterion [0-1], the closer to 1, the closer to ideal solution}
}

\references{
Papathanasiou, Jason, Ploskas, Nikolaos. Multiple Criteria Decision Aid Methods, Examples and Python Implementations. Springer, 173 p., ISBN 978-3-319-91648-4.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\examples{
dictionaryW <- rbind(
  c(0, 0, 0.1),
  c(0, 0.1, 0.3),
  c(0.1, 0.3, 0.5),
  c(0.3, 0.5, 0.7),
  c(0.5, 0.7, 0.9),
  c(0.7, 0.9, 1),
  c(0.9, 1, 1)
)
#V very, L low, M medium, H high
rownames(dictionaryW) <- c('VL', 'L', 'ML', 'M', 'MH', 'H', 'VH')
w <- rbind(
  c('H', 'VH', 'VH'),
  c('M', 'H', 'VH'),
  c('M', 'MH', 'ML'),
  c('H', 'VH', 'MH')
)
rownames(w) <- c('Investment cost', 'Employment needs', 'Social impact',
  'Environmental impact')
dictionaryPM <- rbind(
  c(0,0,1),
  c(0,1,3),
  c(1,3,5),
  c(3,5,7),
  c(5,7,9),
  c(7,9,10),
  c(9,10,10)
)
#V very, P poor, M medium, F fair, G good
rownames(dictionaryPM) <- c('VP', 'P', 'MP', 'F', 'MG', 'G', 'VG')
PM <- rbind(
  c('VG', 'G', 'MG'),
  c('MP', 'F', 'F'),
  c('MG', 'MP', 'F'),
  c('MG', 'MG', 'VG'),
  c('VP', 'P', 'G'),
  c('F', 'G', 'G'),
  c('F', 'MG', 'MG'),
  c('F', 'VG', 'G'),
  c('MG', 'MG', 'VG'),
  c('G', 'G', 'VG'),
  c('P', 'VP', 'MP'),
  c('F', 'MP', 'MG'),
  c('P', 'P', 'MP'),
  c('MG', 'VG', 'G'),
  c('MP', 'F', 'F'),
  c('MG', 'VG', 'G'),
  c('G', 'G', 'VG'),
  c('VG', 'MG', 'F'),
  c('G', 'VG', 'G'),
  c('MG', 'F', 'MP'),
  c('MP', 'P', 'P'),
  c('VP', 'F', 'P'),
  c('G', 'MG', 'MG'),
  c('P', 'MP', 'F')
)
alternatives <- c('site 1', 'site 2', 'site 3', 'site 4', 'site 5', 'site 6')
result <- FuzzyTOPSIS(PM, dictionaryPM, w, dictionaryW, alternatives)
}
\keyword{ FuzzyTOPSIS }
\keyword{ TOPSIS }
