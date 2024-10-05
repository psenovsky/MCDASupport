\name{MCDASupport-package}
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
Version: \tab 0.32\cr
Date: \tab 2024-10-07\cr
License: \tab GPL (>= 3)\cr
}

From MCDA methods point of view the package at present time supports:

\itemize{
  \item ARAS
  \item COPRAS
  \item CRADIS
  \item ELECTRE I
  \item ELECTRE 1S (experimental implementation - do not use in production environment)
  \item ELECTRE II
  \item ELECTRE III
  \item ELECTRE IV
  \item ELECTRE TRI
  \item FuzzyTOPSIS
  \item FuzzyVIKOR
  \item MARCOS
  \item PROMETHEE I (experimental implementation - do not use in production environment)
  \item PROMETHEE II (experimental implementation - do not use in production environment)
  \item PROMETHEE III (experimental implementation - do not use in production environment)
  \item SAW
  \item SIR
  \item TOPSIS
  \item VIKOR
  \item WSM - weighted sum method
}

For weight derivation following methods are available:

\itemize{
  \item binary pair-wise comparison
  \item CRITIC
  \item DEMATEL
  \item EWM
}

For normalization:

\itemize{
  \item various normalization approaches (norm_* functions)
}

There are also some methods, which are probably never going to be supported:

For weight generation:

\itemize{
  \item FUCOM - available im fucom package \url{https://cran.r-project.org/package=fucom} by Mateus Vanzetta and Marcos Santos 
}

Main purpose of the package is to study inner workings of various methods used in multicriteria decision making and experiment with it. The provided functions may contain errors or the approaches represented by the functions may not be applicable to every decision problem. So bo carefull if you want to use the functions as actual decision support tool.

Authors of the package are not to be held liable for possible bad decision, you make, even if it is based on results of the functions contained in this package.

\strong{Revision history}

\strong{MCDASupport v0.32 (Release data: 2024-10-07)}

\itemize{
  \item Refactored parameter validation from WSM into separate function to be utilized by other functions, such as SAW.
  \item added implementation of SAW method
  \item added implementation of ARAS method
  \item added implementation of DEMATEL method for weight derivation
  \item added implementation of MARCOS method
  \item implemented binary pair-wise comparison for weight estionation
  \item added implementation of CRADIS method
  \item added implementation of CRITIC method for weight derivation
  \item added implementation of COPRAS method
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
