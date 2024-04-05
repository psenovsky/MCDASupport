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
Version: \tab 0.28\cr
Date: \tab 2023-10-13\cr
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

\emph{MCDASupport v0.28 (Release date: 2023-10-13)}

In this version of the package support for Entropy weights method has been added (EWM function).

\emph{MCDASupport v0.27 (Release date: 2023-10-09)}

V 0.27 continues with implementation of the sensitivity analyzes for implemented MCDA functions

Sensitivity analyses

\itemize{
  \item ELECTRE IV in Electre_4_sensitivity function
  \item ELECTRE 1S in Electre_1S_sensitivity function
  \item ELECTRE 1 in Electre_1_sensitivity function
  \item ELECTRE TRI in Electre_TRI_senditivity function
  \item PROMETHEE I in PROMETHEE_I_sensitivity function
  \item PROMETHEE II in PROMETHEE_II_sensitivity function
}

Other changes

\itemize{
  \item refactored function tenPercent into separate function to be utilized by sensitivity analyzes using percentages to derive computation space to be evaluated for parameter
  \item streamlighned Graph2AdjancancyMatrix function
  \item plot.prefM
  \itemize{
    \item there was an error in preventing to visualize adjacancy matrixes in function Electre_1S (for Electre 1 and 2 the function worked fine)
    \item now properly comunicates that no overranking exist in the graph (no edges in the graph), instead of crashing when adjacancy matrix was all zeros.
    \item removed unnecessary library call (package already depends on it, so it's redundant)
  }
  \item corrected error in Electre_asc_dist function crashing the function when unranked alternatives remained after slicing through them using lambda (pottentially affects both Electre_4 a Electre_3 methods and their sensitivity functions)
  \item Electre_3_paramCheck
  \itemize{
    \item refactored function to take advantage of way of Electre 4 checks its parameter. Electre 4 uses subset of Electre 3 parameters.
    \item also adjusted signature of it to include minmaxcriteria (as the documentation correctly assumes it should be)
  }
  \item refactored finalRanking function for Electre 3 and 4 functions to provide both sorted and unsorted final ranking (the code was originally shared in Electre 3/4 functions)
  \item refactored Electre_1S function to take advantage of param checking in Electre_4_paramCheck
  \item as side effect improved consistency checking for P, Q, V thresholds as 0 <= Qj <= Pj < Vj, for j in <1;no. of criteria> for Electre_3 and 4 functions
  \item corrected error in ELECTRE_DiscordanceMatrix for cases when max(PM[,k]) - min(PM[,k]) == 0, now sets discordance to 1 instead of crashing
  \item ELECTRE_Kernel
  \itemize{
    \item adjusted documentation for the function to indicate, that other alternatives may be excluded from decision making if we are looking for best solution (not try to perform ranking of the alternatives)
    \item also refactored code of it to take advantage of capabilities of the R (as oposed to original Python-like implementation) - the code of the function is much smaller and faster now
  }
  \item made some minor adjustments in the documentation (usual small mistakes, errors in links, etc.)
  \item Electre_TRI
  \itemize{
    \item optimized function's parameter chceking to utilize Electre_4_parameter checking, as majority of parametrs are same
    \item added unsorted results of optimistic and pesimistic ranking procedure for purposes of sensitivity testing
  }
  \item PROMETHEE I - added VERBOSE mode to the function
}

\emph{MCDASupport v0.26 (Release date: 2023-09-15)}

This version focusses on introduction of functionality to facilitate effective sensitivity analysis of the decision support method function's parameters. This functionality is very much work in progress, so some maturing process is to be expected in few following versions.

Sensitivity analysis

\itemize{
  \item ELECTRE II in Electre_2_sensitivity function
  \item ELECTRE III in Electre_3_sensitivity function - initial implementation
  \item implemented function graphRank to visualise changes in ranking of the alternatives dependin on changes of single parameter
}

Other changes

\itemize{
  \item adjusted Electre_2 function to provide both sorted and unsorted final preorder. Unsorted preorder is used in sensitivity analyses.
  \item rewriten plot.prefM function - now it uses visNetwork package to draw the network and also returns network as an object instead of writing it to screen automatically regardless of VERBOSE parameter setting
  \item refactored part of code in Electre_2 function to separate function to share checks with Electre_2_sensitivity
  \item refactored part of code in Electre_3 function to allow for common function's parameter check for Electre_3 and Electre_3_sensitivity
  \item Electre_3 added finalRankingUnsorted output of the function for purposes of sensitivity analysis as previous versions returned only alternatives sorted by ranks
  \item adjusted results of mcda_wsm method to present results in integrated form (output resultTable)
  \item rewritten plot.scoreM function to use plotly library to better present graphical results
  \item adjusted serited results for Electre II to show best results first
}

\emph{MCDASupport v0.25 (Release date: 2023-04-03)}

This version focuses more on improving documentation for existing functions, so that the manual can better function as kind of textbook as the primary intention for the package was to allow students to experiment and learn about various MCDA methods.

Updated documentation for functions:

\itemize{
  \item Electre_1
  \item Electre_1S
  \item Electre_2
  \item Electre_3
  \item Electre_4
  \item Electre_TRI
  \item FuzzyTOPSIS
  \item FuzzyVIKOR
  \item PROMETHEE_I
  \item PROMETHEE
  \item PROMETHEE_II
  \item PROMETHEE_III
}

Other changes:

\itemize{
  \item ELECTRE1_Kernel - now uses plot.prefM in more appropriate way (as it should for S3 method)
  \item Electre_1 - corrected the way the function presents matrices in VERBOSE mode (to the console) and as the returned values
  \item Electre_2
  \itemize{
    \item corrected small error in evaluating of discordance's d+ threshold in strong domination evaluation
    \item was generating too extensive debug information
  }
  \item FuzzyTOPSIS - small adjustments of the code for better readibility
  \item PROMETHEE_III - corrected small issue in consistency of presenting partial prefference (was using p- instead of P-)
}

\emph{MCDASupport v0.24 (Release date: 2022-12-20)}

The version changes focuses on consistency of the package behavior regarding the VERBOSE parameter. Main motivation is to proceed on the road to prepare packege for batch computations  support in the future.

Other errors corrected:

\itemize{
  \item Electre_1 - did not correctly process outputs from ELECTRE1_Kernel (the graph did not correctly propagate to final outpust of the function)
  \item Electre_1S - error in the way the function communicated problem with consistency of the thresholds (check itself was fine)
  \item Electre_1S - did not correctly process outputs from ELECTRE1_Kernel (the graph did not correctly propagate to final outpust of the function)
  \item Electre_3 - adjusted some error messages to be more easy to understand
  \item Electre_TRI - error in the way the function communicated problem with consistency of the thresholds (check itself was fine)
}

\emph{MCDASupport v0.23 (Release date: 2022-10-22)}

This update focuses on improving documentation side of the package. Adjusted documentation for basic information on the package. Revision history is now part of dovumentation and so is list of supported MCDA methods.

\emph{MCDASupport v0.22 (Release date: 2022-09-30)}

Changes:
Relatively small change in code, which changes default behavior of the functions. In earlier versions they dumped their outputs into the console. If you want this behavios, you need to set VERBOSE parameter to TRUE. Intend was to make it easier to use functions in R markdown.

Allows users to better control outputs to the file. As a donwside users will need to activelly choose, what needs to be printed.

Adjusted functions are:

\itemize{
  \item ELECTRE I
  \item ELECTRE 1S
  \item ELECTRE II
  \item ELECTRE III
  \item ELECTRE IV
  \item ELECTRE TRI
  \item FuzzyTOPSIS
  \item FuzzyVIKOR
  \item WSM
  \item PROMETHEE II
  \item SIR
  \item TOPSIS
  \item VIKOR
}

Other changes:
\itemize{
  \item Electre_4 function was too much verbose and printed its outputs multiple times
  \item SIR corrected function documentation, minmax parameter was missing
  \item SIR replaced T value for SAW for TRUE (syntactically more correct)
  \item SIR documentation - improved examle - line was too long and could get truncated in PDF version of the manual
  \item SIR documentation - also corrected SAW = TRUE in example, to avoid errors
  \item SIR documentation - adjusted T/F to TRUE/FALSE as to correspond to the changes in code
  \item SIR documentation - also parametrs w and d were switched in documentation
  \item FuzzyVIKOR corrected function documentation, v parameter was missing
  \item FuzzyTOPSIS - mising comma in parameters (usage section)
  \item VIKORIndexes corrected function documentation, v parameter was missing
  \item VIKOR documentation - default parameters values were missing
  \item TOPSIS documentation - default parameters values were missing
  \item Electre_3 documentation - use FALSE to betters correspond to code
}

\emph{MCDASupport v0.21 (Release date: 2022-05-11)}

Changes:
\itemize{
  \item implementation of SIR method
  \item added support for SIR-SAW aggregation procedure (default mode of aggregation)
  \item added support for SIR-TOPSIS aggregation procedure - not validated and bugy
}

\emph{MCDASupport v0.20 (Release date: 2022-05-06)}

Changes:
\itemize{
  \item added FuzzyVIKOR method
  \item corrected small error in documentation of FuzzyTOPSIS method (usage section did not have alt parameter)
  \item refactored FuzzyVIKOR SR function to separated function to be used by both FuzzyVIKOR and VIKOR funcions
  \item refactored VIKOR function to improve original SR function to compute Q and compromise solution for both VIKOR and FuzzyVIKOR functions
}

\emph{MCDASupport v0.19 (Release date: 2022-04-12)}

Changes:
\itemize{
  \item implementation of FuzzyTOPSIS method
  \item the method has been validated againts expected results as per example in Multiple Criteria Decision Aid Methods, Examples and Python Implementations.
}

\emph{MCDASupport v0.18 (Release date: 2022-03-28)}

Changes:
\itemize{
  \item added additional check on consistency of performance matrix for all supported MCDA methods
  \item no changes in API (didn't generate documentation for v0.18, as v0.17 is still valid)
}

\emph{MCDASupport v0.17 (Release date: 2022-03-26)}

Changes:
\itemize{
  \item implementation of TOPSIS method
}

\emph{MCDASupport v0.16 (Release date: 2022-03-24)}

Changes:
\itemize{
  \item corrected small typo in PROMETHEE documentation
  \item implementation of VIKOR method
  \item VIKOR method validated
}

\emph{MCDASupport v0.15 (Release date: 2022-03-24)}

Changes:
\itemize{
  \item small changes due to validation process of PROMETHEE funcions
  \item rewritten PROMETHEE preference degree logic
  \item added logic to compute Gausian type of the 'shape' for PROMETHEE function (preference degree) as it was missing in previous version
  \item improved readibility of the method results
  \itemize{
    \item it is clearer now, what criterion is the pairweise comparison describing
    \item same treatment for preference degree
  }
  \item completely rewritten PROMETHEE flow logic and validated the procedure
  \item PROMETHEE methods are considered validated now
}

\emph{MCDASupport v0.14 (Release date: 2022-02-23)}

Changes:
\itemize{
  \item experimetnal implementation of PROMETHEE III function
  \item corrected slight error in example in PROMETHEE function
  \item PROMETHEE functions were not properly exported into package NAMESPACE
}

\emph{MCDASupport v0.13 (Release date: 2022-02-22)}

Changes:
\itemize{
  \item  experimental implementation of PROMETHEE function for future usage by PREMETHEE I-II functions
 experimental implementation of PROMETHEE_I function for (PROMETHEE I method)
  \item experimental implementation of PROMETHEE_II function for (PROMETHEE II method)
  \item the implementation of PROMETHEE methods should be considered experimental, while rudimentaly validation of results has been performed on them to prove, that the implementation works more or less as intended, procedure implementation may still contain various problems
}

\emph{MCDASupport v0.12 (Release date: 2022-02-16)}

Changes:
\itemize{
  \item dedicated to cleaning of the library code
  \item removing library and require calls to libraries attached by depends
  \item adjusted signatures of plot.prefM and plot.scoreM methods to safer work with generics
  \item registering S3 methods prefM and scoreM extending generic function plot
  \item Electre_2: consistency check for Discordance level d- > d+ (must be d- <= d+) was not functioning correctly (execution stoped by error of code generating error message instead of correctly stoping execution and reporting the problem)
  \item norm_ZavadskasTurskis: corrected typo in variable name
  \item revisited dependencies the library uses - should improve concistency of library behavior in different production environments
  \item corrected error in documnetation of Electre_1S function (API called Electre_1 function instead), thou the function parameters were correct (and so was an example).
  \item corrected typo in documentation of the mcda_get_dominated function (one parameter mispelled)
  \item mcda_wsm added minmaxcriteria parameter into function signature documentation
  \item util_pm_minmax: corrected typo in function's parameter PM documentation
}

\emph{MCDASupport v0.11 (Release date: 2022-01-15)}

Changes:
\itemize{
  \item implemented Markovic normalization (norm_markovic)
  \item implemented Tzeng and Huang method for normalization (norm_TzengHuang)
  \item implemented Peldschus et al. (1983) method for nonlinear normalization (norm_nonlinear)
  \item implemented Lai & Hwang (1994) mothod for normallization (norm_LaiHwang)
  \item implemented Zavadskas and Turskis (2008) normalization method (norm_ZavadkasTuskis)
  \item corrected some errrors in documentation of warious functions
  \item corrected error in preference matrix size detection in function mcda_rescale_pm
}

\emph{MCDASupport v0.10 (Release date: 2021-10-22)}

Changes:
\itemize{
  \item added implementation of Electre 1S method
  \item refactored ELECTRE III implementation - concordance index is now separate function to be also utilized by ELECTRE 1S
  \item refactored ELECTRE I implementation - procedure to identify kernel of the solution has been refactored into separate function as it is being used by both ELECTRE I and 1S methods.
}

\emph{MCDASupport v0.9 (Release date: 2021-10-19)}

Changes:
\itemize{
  \item small correction in error mesages in Electre 3, 4 - some were reffering to number of alternatives, when number of criteria was meant
  \item experimental implementation of ELECTRE Tri method
}

\emph{MCDASupport v0.8 (Release date: 2021-10-15)}

Changes:
\itemize{
  \item corrected small error in final ranking establishing in Electre 3 method
  \item refactored Electre 3 parts into separate functions to be used also for computation of Electre 4 method
  \item implementation of Electre 4 method
  \item improved implementation of distilation (descending/ascending) for cases when the distinguishion between the alternatives using cut-off, was not possible (these alternatives are assigned en-block at the end of the ranking as it is not possible to distinguidh between them)
}

\emph{MCDASupport v0.7 (Release date: 2021-10-12)}

Changes:
\itemize{
  \item improved final information preorder in Electre_2 method - now ordered from best to worst and changed index to start with 1 (istead of 0)
  \item continuing validation of Electre_3 method, now:
  \itemize{
    \item downward procedure D1 and first distilation validated
    \item reimplemented descending and ascending distillation precedures based on much more elegant implementation in MCDA package by Umit Yildirim. Computation in MCDA package ends with these procedures, so it does not provide final ranking.
    \item reviewed preorder matrix, slightly improved efficiency and also adjusted to use dataframe as input
  }
  \item package now requires dplyr to function properly
  \item removed dependency on igraph - the dependency is still implied by udage of dplyr
  \item removed Electre III method implementation from outranking tools
}

\emph{MCDASupport v0.6 (Release date: 2021-10-06)}

Changes:
\itemize{
  \item added rounding to concordance matrix to improve readibility of results and corrected small error in computation
  \item concordance matrix computation for Electre 1 and 2 methods has been sucessfully validated
  \item reimplemented discordance matrix computation for Electre_1 and Electre_2 methods
  \item Electre_1 method looks usable
  \item identified some problems in discordance matrix implementation in Electre_1OT (implementation from Outrankong tools) leading to again removing the method
}

\emph{MCDASupport v0.5 (Release date: 2021-10-05)}

Changes:
\itemize{
  \item Electre_3 method now partially usable - the computation is correc up to first distilate of downward distillation process. Method is still not to be used in production environment
  \item corrected error in plotting plot.prefM in Electre3* methods
  \item corrected errors in mcda_wsm method
  \item added sorting for alternatives in mcda_wsm and added performance matrix to method output (for consistency reasons as other methods in package do have it in output)
  \item minnor corrections in mcda_wsm method documentation
  \item corrected errors in mcda_get_dominated, also small optimizations of the code
  \item added plot.scoreM method to visualize performance of the alternatives
  \item adjusted mcda_wsm to make use of plot.scoreM to better visualize results
  \item identified problems with Electre_1 method - in the way method computes discordance matrixes, as result returned method Electre_1 from original outranking tools under name Electre_1OT
}

\emph{MCDASupport v0.4 (Release date: 2021-09-30)}

Changes:
\itemize{
  \item new implementation of ELECTRE III method (function Electre_3) inspired by API of pyDecisions, thou most parts of the method have been implementated from scratch.
  \item the method right now is in for testing purposes, for stable (usefull) outcomes use Electre3 family of functions (from original Outranking Tools packag)
  \item added few widely used normalization (and standardization) methods
  \itemize{
    \item norm_minmax - min-max normalization
    \item norm_zscore - standardized score
    \item norm_toaverage - normalize by comparing to the mean of the vector
    \item norm_tobest - normalize by comparing to highest value
    \item norm_logarithm - logarithmic normalization
    \item norm_vector - vector normalization
    \item norm_linearagreg - linear normalization by agreggating values
  }
}

\emph{MCDASupport v0.3 (Release date: 2021-09-14)}

Changes:
\itemize{
  \item Rewriten ELECTRE I (Electre_1) function - now the function is implementation of pyDecisions in R. Reimplemented for better readability
  \item refactored concordance and discordance indexes into separate functions as they are shared between ELECTRE I and II methods
  \item improved util_prepare_minmaxcriteria function (some edge cases were not working correctly)
  \item improved NAMESPACE (there were some problems with exported functions and hence ability for the user to install the package)
  \item implemented ELECTRE II (Electre_2) function
}

\emph{MCDASupport v0.2 (Release date: 2021-08-20)}

Changes:
\itemize{
  \item implemented mcda_get_dominated function
  \itemize{
    \item added basic input information validity checks
    \item added support for specifying orientation of the scriteria (min or max)
  }
  \item implemented mcda_del_dominated function
  \itemize{
    \item added basic validity chcecks
  }
  \item implemented plot.prefM function to better plot preference matrixes
  \itemize{
    \item added basic validity chcecks for parameters
  }
  \item adjusted Electre functions to use this prefM variant instead of normal plot function
  \item implemented mcda_rescale_pm for rescaling of numeric performance matrixes
  \itemize{
    \item added basic validity chcecks for parameters
  }
  \item created function util_pm_minmax and util_prepare_minmaxcriteria to simplify refactor and simplify other analytical functions
  \itemize{
    \item refactored all *_min.R functions to utilize these
  }
}

\emph{MCDASsupport v0.1 (Release date: 2021-08-12)}

Changes:
\itemize{
  \item Electre_1 method
  \itemize{
    \item cleaned
    \item added export of preference matrix representing computed dominance relation between alternatives from Electre_1 function
    \item Refactored Electre_1 - part responsible for saving output of the function into file has been exported into separate function Electre_1_output
    \item added Electre_1_min wrapper method to simplify usage of Electre_1 in situations when dataframe contains information on alternatives and criteria
    \item adjusted documentation to indicate Electre II method is Electre I using thresholds
  }
  \item Electre3 method Electre_SimpleThresholds
  \itemize{
    \item cleaned
    \item method adjusted to publish adjancy matrix - before matrix was used only internaly to draw graph but the user did not have access to it
    \item Added Electre3_SimpleThresholds_min to simplify usage if dataframe with row and col names is provided as input (performance matrix)
  }
  \item Electre method Electre3_AlphaBetaThresholds
  \itemize{
    \item cleaned
    \item method adjusted to publish adjancy matrix - before matrix was used only internaly to draw graph but the user did not have access to it
    \item Added Electre3_AlphaBetaThresholds_min to simplify usage if dataframe with row and col names is provided as input (performance matrix)
  }
  \item Electre TRI
  \itemize{
    \item cleaned
    \item function now returns computed objects
  }
}

\emph{MCDAsupport v0.0-0 (Release date: 2014)}

Original release of v1.0 of OutrankingTools package by Michel Prombo
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
