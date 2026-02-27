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
Version: \tab 0.36\cr
Date: \tab 2024-10-17\cr
License: \tab GPL (>= 3)\cr
}

From MCDA methods point of view the package at present time supports:

\itemize{
  \item ARAS
  \item CoCoSo
  \item CODAS
  \item COPRAS
  \item CRADIS
  \item EDAS
  \item ELECTRE (variants I, II, III, IV, 1S and TRI)
  \item EVAMIX
  \item GRA
  \item MABAC
  \item MACBETH
  \item MARCOS
  \item MAUT
  \item MOORA
  \item MOOSRA
  \item ORESTE
  \item PAMSSEM (variants I (not validated) and II)
  \item PROMETHEE (variants I, II and III)
  \item PSI (alse weight rerivation method)
  \item QUALIFLEX
  \item REGIME
  \item SAW
  \item SIR (normal and fuzzy)
  \item SMART
  \item SPOTIS
  \item TODIM
  \item TOPSIS (normal and fuzzy)
  \item VIKOR (normal and fuzzy)
  \item WASPAS
  \item WISP (and S-WIPS)
  \item WPM (Wieghted Product Methed)
  \item WSM (Weighted Sum Method)
}

For weight derivation following methods are available:

\itemize{
  \item binary pair-wise comparison
  \item CILOS (Criterion Impact LOSs)
  \item COPRAS
  \item CRITIC
  \item DEMATEL
  \item EWM
  \item IDOCRIW (Integrated Determination of Objective CRIteria Weights)
  \item MEREC
  \item MW (Mean Weighting)
  \item PSI (also MCDA method)
  \item SDW (Standard Deviation Weighting)
  \item SVW (Statistical Variance Weighting)
  \item SWARA
}

For normalization:

\itemize{
  \item Lai and Hwang normalization
  \item Linear normalization using aggregation of values
  \item Normalization of values by applying logarithmic transformation.
  \item Markovic normalization
  \item Min-max method
  \item nonlinear normalization
  \item value normalization to average value
  \item normaliozation to best value
  \item Tzeng and Huang normalization
  \item vector normalization
  \item Zavadskas and Turskis normalization
  \item Z-score
}

There are also some methods, which are probably never going to be supported:

For weight generation:

\itemize{
  \item FUCOM - available im fucom package \url{https://cran.r-project.org/package=fucom} by Mateus Vanzetta and Marcos Santos 
}

Main purpose of the package is to study inner workings of various methods used in multicriteria decision making and experiment with it. The provided functions may contain errors or the approaches represented by the functions may not be applicable to every decision problem. So bo carefull if you want to use the functions as actual decision support tool.

Authors of the package are not to be held liable for possible bad decision, you make, even if it is based on results of the functions contained in this package.

\strong{Revision history}

\strong{MCDASupport v0.36 (Release data: 2024-10-07)}

backward incompatible changes:
\itemize{
  \item original EWM function available in previous version is available only as a method for mcda_objective_weights(pm, method = "EWM")
  \item original CRITIC function available in previous version is now available only as mcda_objective_weights(pm, minmax, method = "CRITIC")
  \item original MEREC function available in previous version is now available only as mcda_objective_weights(pm, minmax, method = "MEREC")
  \item original CILOS function available in previous version is now available only as mcda_objective_weights(pm, minmax, method = "CILOS")
  \item original IDOCRIW function available in previous version is now available only as mcda_objective_weights(pm, minmax, method = "IDOCRIW")
}

new models:
\itemize{
  \item implemented SPOTIS model
  \item objective weighting methods
  \itemize{
    \item added MW - Mean Weighting method
    \item added SDW - Standard Deviation Weighting Method
    \item added SVW - Statistical Variance Weighting Method
    \item refactored EWM (Entropy Weight Method) into this framework
    \item refactored CRITIC (Criterion Importance Through Intercriteria Correlation) into this framework
    \item refactored MEREC (Method based on Removal Effects of Criteria) into this framework
    \item refactored CILOS (Criterion Impact LOSs) into this framework
    \item refactored IDOCRIW (Integrated Determination of Objective CRIteria Weights) into this framework
  }
}

bug fixes:
\itemize{
  \item corrected error in documentation in exmaple for SIR constructor
  \item small corrections in documentation
  \item normalization methods
  \itemize{
    \item added support for min and max parametres in minmax normalization to minimize risk of rank reversal (optional parameters)
    \item corrected documentation for to best normalization and added a synonym to it (max normalization)
    \item added synonym for minmax normalization (maxmin normalization)
    \item added synonym for Linear aggregation - sum normalization
  }
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
