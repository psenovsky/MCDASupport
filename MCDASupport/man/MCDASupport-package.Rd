\name{MCDASupport-package}
\alias{MCDASupport-package-package}
\alias{MCDASupport-package}
\docType{package}
\title{Functions for Solving Multiple-criteria Decision-making Problems}
\description{
The outranking methods constitute one of the most fruitful approach in the field of Multiple Criteria Decision Making (MCDM).

They main feature is to compare all feasible alternatives or actions by pair building up some binary relations, crisp or fuzzy, and then exploit in appropriate way these relations in order to obtain final recommendations.
}

\details{
\tabular{ll}{
Package: \tab MCDASupport\cr
Type: \tab Package\cr
Version: \tab 0.30\cr
Date: \tab 2024-06-17\cr
License: \tab GPL (>= 3)\cr
}

From methods point of view the package at present time supports:

\itemize{
  \item ELECTRE I
  \item ELECTRE 1S (experimental implementation - do not use in production environment)
  \item ELECTRE II
  \item ELECTRE III
  \item ELECTRE IV
  \item ELECTRE TRI
  \item PROMETHEE I (experimental implementation - do not use in production environment)
  \item PROMETHEE II (experimental implementation - do not use in production environment)
  \item PROMETHEE III (experimental implementation - do not use in production environment)
  \item WSM - weighted sum method
  \item SIR
  \item VIKOR
  \item FuzzyVIKOR
  \item TOPSIS
  \item FuzzyTOPSIS
  \item various normalization approaches (norm_* functions)
}

Main purpose of the package is to study inner workings of various methods used in multicriteria decision making and experiment with it. The provided functions may contain errors or the approaches represented by the functions may not be applicable to every decision problem. So bo carefull if you want to use the functions as actual decision support tool.

Authors of the package are not to be held liable for possible bad decision, you make, even if it is based on results of the functions contained in this package.

\strong{Revision history}

\strong{MCDASupport v0.30 (Release data: 2024-06-17)}

The main topic of version 0.30 is to further optimize the code and also make it more readable. As result there are changes in almast all functions.

\itemize{
    \item resolving some small issues with tracing of project's files in development repositories
    \item added normalize function which aggregates all the norm_* function into one clean implementation
    \item optimization of Electre_1_sensitivity - cyclomatic complexity reduced from 28 to 18
    \item optimization of Electre_1 and ELECTRE1_kernel
    \item better compliance with linting tools for Electre_1S_paramCheck, Electre_1S_sensitivity and Electre_1S
    \item optimization in Electre_2* functions
    \item error corrected in Electre_2_sensitivity, was not doing sensitivity check for d+ (d+ was same as d-)
    \item optimization in Electre_3* functions
    \item optimization in Electre_4* functions
    \item Electre_TRI_sensitivity - optimizations and veto thresholds sensitivity was reported wrongly
    \item FuzzyTOPSIS optimized
    \item optimization of PROMETHEE function
    \item refactored parameter check code for PROMETHEE and SIR into separate function
}

For full history of the changes for the package, check NEWS.md on GitHub page of the project (\url{https://github.com/psenovsky/MCDASupport}).

}

\author{Pavel Šenovský \email{pavel.senovsky@vsb.cz}}

\references{
Roy, B. (1996) Multiple Criteria Methodology for Decision Aiding, Dordrecht: Kluwer Academic.

Roy, B. and Bouyssou, D. (1985). An example of comparison of two decision-aid models,in G. Fandel and J. Spronk (eds)

Ballestero, E. and Romero, C. (1998) Multiple Criteria Decision Making and its Applications to Economic Problems, Boston-Dordrecht-London: Kluwer Academic.

Vincke, P. (1992) Multi-criteria Decision-Aid, John Wiley, Chichester.

Roy B. (1968) Classement et choix en presence de points de vue multiples (la methode Electre), Revue Francaise  d Informatique et de Recherche Operationnelle.

Prombo, M. Package OutrankingTools, CRAN: 2015, available from: \url{https://cran.r-project.org/web/packages/OutrankingTools/}

Rogers, Martin and Myastre, Lucien-Yves. ELECTRE and Decision Support: Methods and Applications in Engineering and Infrastructure investment. Springer 2000, 208 p., ISBN 978-1-4757-5057-7

Balamurali, M.: pyDecisions - A Python Library of management decision making techniques. Avilable on-line from \url{https://github.com/Valdecy/pyDecisions}
}
\keyword{package}
\keyword{ELECTRE methods}
\keyword{Outranking approaches}
\keyword{preference modelling}
\keyword{Sorting problem}
\keyword{multicriteria analysis}
\keyword{normalization}
