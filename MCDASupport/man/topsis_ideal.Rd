\name{topsis_ideal}
\alias{topsis_ideal}
\title{topsis_ideal : function to compute ideal and antiedeal solution for TOPSIS and SIR (internal use only)}
\description{
    Function to compute ideal and antiideal solution to normalized perfomance matrix. 

    The function is utilized by \code{\link{SIR}} and \code{\link{TOPSIS}} methods. for this purpose. It incorporates steps 3 - 5 of the TOPSIS computation. Consult \code{\link{TOPSIS}} function's documentation for implementation details.
}

\usage{
topsis_ideal(pm, w, inferiority = FALSE)
}

\arguments{
  \item{pm}{normalized performance matrix}
  \item{inferiority}{FALSE by defaul to compute ideals as max from pm, TRUE will minimize ideal and maximize anitiideal}
}

\value{
	The function returns:

    \item{a_ideal}{Positive ideal soluation A*}
    \item{a_anti}{Anti-ideal solution A-}
    \item{d_ideal}{Closenes to ideal variant}
    \item{d_anti}{Closeness to anti-ideal variant}
    \item{closenes}{Relative closeness of the alternatives to ideal solution}
}

\references{
ALAOUI, Mohamed El. Fuzzy TOPSIS: Logic, Approaches, and Case Studies. Boca Raton: CRC Press, 2021. 216 s. ISBN 978-0-367-76748-8.

Papathanasiou, Jason, Ploskas, Nikolaos. Multiple Criteria Decision Aid Methods, Examples and Python Implementations. Springer, 173 p., ISBN 978-3-319-91648-4.
}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\keyword{ TOPSIS }
\keyword{ SIR }
\keyword{ ideal solution }
\keyword{ antiideal solution }
