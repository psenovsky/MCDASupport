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
Version: \tab 0.31\cr
Date: \tab 2024-09-20\cr
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

\strong{MCDASupport v0.31 (Release data: 2024-09-20)}

The v0.31 presents probably largest ammount of changes between the the versions. During its development over 100 commits were pushed into the repository and basically whole code-base of the package changed. Main goal of the changes was:

\itemize{
  \item to implement models in form of R6 class, which improves readability of code
  \item and also allows to implement S3 functions for summary of objects, well at least the models have implemented support of this function.
  \item sensitivity analysis is also redone to more properly test possible ranges for various thresholds
  \item as part of changes the documentation switched to Roxygen documentation, making the documentation available directly from the code.
}

More detailed list of changes for the vesion:
\itemize{
  \item refactored common code for SIR and TOPSIS into topsis_ideal function
  \item some refactoring in FuzzyTOPSIS function
  \item added plot.threshold.3d and plot.threshold functions to visualize senzitivity, paradoxically new approach to computation using R6 classes makes no use of these. Possibly these functions will be removed in future versions (or never leave the development branch)
  \item refactored Electre_1 and Electre_1_sensitivity into single universally usable R6 class. This approach will be used as model for other methods. 
  \itemize{
    \item as side effect the summary function for the model object is now supported. 
    \item and sensitivity of the solution is computed (in past intervals for parametr testing had to be set manually)
    \item please note that this change is API breaking
  }
  \item refactored Electre_1S and Electre_1s_sensitivity into single R6 class (similarly to implementation of the Electre_1) - implementation should be still considered as experimental and bugy. As a side effect Electre_1s_paramCheck is no longer needed.
  \item refactored Electre_2 and Electre_2_sensitivity into single R6 class
  \item refactored sens_compare function from electre2 class into separate function vector_compare. Intention is to use it in all classes for detecting changes in the model solution.
  \item corrected error in documentation of electre1s::sensitivity function, which prevented the documentation for the function to be rendered
  \item corrected error for 3 fields in electre2 to render their documentation
  \item refactored Electre_3 and Electre_3_sensitivity into new R6 class
  \item refactored Electre_4 and Electre_4_sensitivity into R6 electre4 class
  \item refactored common parts of Electre III and IV sensitivity testing into sensitivity_e34 function.
  \item refactored Electre_TRI and Electre_TRI_sensitivity into R6 electretri R6 class
  \item refactored FuzzyTOPSIS into R6 fuzzytopsis class
  \item Fuzzy VIKOR
  \itemize{
    \item refactored FuzzyVIKOR into R6 fuzzyvikor class
    \item corrected error in result computation - in R metric the S metric was provided (the overal solution was correct)
  }
  \item mcda_wsm refactored into wsm R6 class
  \item rewriten function. Now it better checks the thresholds depending on used preference function. Also improved consistency check for weights.
  \item refactored PROMETHEE_I and PROMETHEE_I_sensitivity into single R6 class
  \item refactored PROMETHEE_II and PROMETHEE_II_sensitivity into single R6 class
  \item refactored shared code for sensitivity testing for PROMETHEE I and II into separate private function
  \item refactored PROMETHEE_III function into promethee3, including implementing summary function and sensitivity testing.
  \item refactored SIR function, including implementing summary function and sensitivity testing for SIR-TOPSIS.
  \item refactored TOPSIS into R6 class
  \item refactored VIKOR into R6 class. Also corrected error of method not properly showing R-metric (provided S-metric instead). The problem was in broadcasting R-metric only, the computation and overal results were correct.
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
