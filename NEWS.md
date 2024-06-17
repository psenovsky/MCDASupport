# MCDASupport v0.30 (Release data: TBD)

The main topic of version 0.30 is to further optimize the code and also make it more readable. As result there are changes in almast all functions.

* resolving some small issues with tracing of project's files in development repositories
* added normalize function which aggregates all the norm_* function into one clean implementation
* optimization of Electre_1_sensitivity - cyclomatic complexity reduced from 28 to 18
* optimization of Electre_1 and ELECTRE1_kernel
* better compliance with linting tools for Electre_1S_paramCheck, Electre_1S_sensitivity and Electre_1S
* optimization in Electre_2* functions
* error corrected in Electre_2_sensitivity, was not doing sensitivity check for d+ (d+ was same as d-)
* optimization in Electre_3* functions
* optimization in Electre_4* functions
* Electre_TRI_sensitivity - optimizations and veto thresholds sensitivity was reported wrongly
* FuzzyTOPSIS optimized
* optimization of PROMETHEE function
* refactored parameter check code for PROMETHEE and SIR into separate function

# MCDASupport v0.29 (Release date: 2023-12-19)

Main goal of version 0.29 is to go back to basics and look at the efficiency of the inner working of the functions. Since original implementation was inspired by classical programming languages, the functions relied extensively on the cycles to realize the computation. But R is optimized to different types of computations and provides alternative approaches to get same results but with highly efficient work with matrices instead.

Such efficiency is desirable as it will speed computation especially for more advanced sensitivity analyses, which should be implemented in the future. It slould also improves readability of the code by significantly simplifying it.

The optimizations have been applied to:

* ELECTRE I for aSb iff C(a,b) >= c_thres and D(a,b) <= d_thres
* also removed one nested cycle in ELECTRE_ConcordanceMatrix, which has impact on ELECTRE I, II and some other methods in ELECTRE family
* similarly ELECTRE discordance matrix computation has been slightly refactored
* ELECTRE 2
* ELECTRE 3 - only small improvement in the way of initializing empty (zero) matrices, and in computation of concordance matrix
* ELECTRE 4 - again only small improvements in matrices initialization and also inside computation cycles, but nothing revolutionary to see there.
* ELECTRE TRI - some basic simplifications
* FuzzyVIKOR - optimized agg_fuzzy_value and best_worst_fij functions
* mcda_get_dominated - optimized domination relation identification
* small optimizations in pre_order_matrix function
* optimized significantly preference matrix (comparison) generation in PROMETHEE_I function
* mcda_wsm - optimized weight application to the preference matrix
* PROMETHEE (general function utilized in PROMETHEE family of functions) improved efficiency of pairwise comparison and also optimized criteria flows computation
* PROMETHEE II simplified ranking algorithm and preference matrix computation
* PROMETHEE III simplified computation of x and y limit (netFlow +/- standard error of netFlow), also improved preference matrix generation
* SIR
  - improved efficiency of pairwise comparisons
  - for TOPSIS variant of Calculation of the Separation Measures improvements have been made (for both Si and Ii)
  - also partial ranking computation has been improved
* preferenceDegree - improved efficiency of computation. The function is being used by PROMETHEE and SIR
* TOPSIS - improved efficiency of normalization step a Calculation of the Separation Measures used from SIR (as it uses TOPSIS procedure as one option to compute flows)

other changes:

* FuzzyTOPSIS - VERBOSE mode is now more verbose, originally only printed closeness coefficients for alternatives.
* mcda_get_dominated - adjusted values returned by function now returns list with adjacency matrix representing domination relation and also returns
* adjusted mcda_del_dominated to work with changed return values of mcda_get_dominated
* mcda_wsm - the VERBOSE regime provided too little information, its more talkative now
* corrected error in pre_order_matrix function in ascension ranking
* adjusted readability of PROMETHEE_I preference matrix, now specifies P+ for aPb and P- for bPa
* refactored preference degree computation used in PROMETHEE and SIR functions to separate function
* corrected small error in EWM, which prevented function to properly VERBOSE all info
* corrected small error in Electre_TRI, which prevented function to properly VERBOSE all info
* Added documentation for some (primarily) internal function of packages, which have been introduced in version 0.27 of the package.
* corrected documentation for EWM method (VERBOSE parameter was missing in usage section)

# MCDASupport v0.28 (Release date: 2023-10-13)

In this version of the package support for Entropy weights method has been added (EWM function).

# MCDASupport v0.27 (Release date: 2023-10-09)

Version 0.27 continues with implementation of the sensitivity analyzes for implemented MCDA functions

Sensitivity analyses

* ELECTRE IV in Electre_4_sensitivity function
* ELECTRE 1S in Electre_1S_sensitivity function
* ELECTRE 1 in Electre_1_sensitivity function
* ELECTRE TRI in Electre_TRI_senditivity function
* PROMETHEE I in PROMETHEE_I_sensitivity function
* PROMETHEE II in PROMETHEE_II_sensitivity function

Other changes

* refactored function tenPercent into separate function to be utilized by sensitivity analyzes using percentages to derive computation space to be evaluated for parameter
* streamlighned Graph2AdjancancyMatrix function
* plot.prefM
  * there was an error in preventing to visualize adjacency matrices in function Electre_1S (for Electre 1 and 2 the function worked fine)
  * now properly communicates that no over ranking exist in the graph (no edges in the graph), instead of crashing when adjacency matrix was all zeros.
  * removed unnecessary library call (package already depends on it, so it's redundant)
* corrected error in Electre_asc_dist function crashing the function when unranked alternatives remained after slicing through them using lambda (potenciality affects both Electre_4 a Electre_3 methods and their sensitivity functions)
* Electre_3_paramCheck
  * refactored function to take advantage of way of Electre 4 checks its parameter. Electre 4 uses subset of Electre 3 parameters.
  * also adjusted signature of it to include minmax criteria (as the documentation correctly assumes it should be)
* refactored finalRanking function for Electre 3 and 4 functions to provide both sorted and unsorted final ranking (the code was originally shared in Electre 3/4 functions)
* refactored Electre_1S function to take advantage of param checking in Electre_4_paramCheck
* as side effect improved consistency checking for P, Q, V thresholds as 0 <= Qj <= Pj < Vj, for j in <1;no. of criteria> for Electre_3 and 4 functions
* corrected error in ELECTRE_DiscordanceMatrix for cases when max(PM[,k]) - min(PM[,k]) == 0, now sets discordance to 1 instead of crashing
* ELECTRE_Kernel
  * adjusted documentation for the function to indicate, that other alternatives may be excluded from decision making if we are looking for best solution (not try to perform ranking of the alternatives)
  * also refactored code of it to take advantage of capabilities of the R (as oposed to original Python-like implementation) - the code of the function is much smaller and faster now
* made some minor adjustments in the documentation (usual small mistakes, errors in links, etc.)
* Electre_TRI
  * optimized function's parameter checking to utilize Electre_4_parameter checking, as majority of parameters are same
  * added unsorted results of optimistic and pesimistic ranking procedure for purposes of sensitivity testing
* PROMETHEE I - added VERBOSE mode to the function

# MCDASupport v0.26 (Release date: 2023-09-13)

This version focuses on introduction of functionality to facilitate effective sensitivity analysis of the decision support method function's parameters. This functionality is very much work in progress, so some maturing process is to be expected in few following versions.

Sensitivity analysis

* ELECTRE II in Electre_2_sensitivity function
* ELECTRE III in Electre_3_sensitivity function - initial implementation
* implemented function graphRank to visualize changes in ranking of the alternatives depending on changes of single parameter

Other changes

* adjusted Electre_2 function to provide both sorted and unsorted final preorder. Unsorted preorder is used in sensitivity analyses.
* rewritten plot.prefM function - now it uses visNetwork package to draw the network and also returns network as an object instead of writing it to screen automatically regardless of VERBOSE parameter setting
* refactored part of code in Electre_2 function to separate function to share checks with Electre_2_sensitivity
* refactored part of code in Electre_3 function to allow for common function's parameter check for Electre_3 and Electre_3_sensitivity
* Electre_3 added finalRankingUnsorted output of the function for purposes of sensitivity analysis as previous versions returned only alternatives sorted by ranks
* adjusted results of mcda_wsm method to present results in integrated form (output resultTable)
* rewritten plot.scoreM function to use plotly library to better present graphical results
* adjusted results for Electre II to show best results first

# MCDASupport v0.25 (Release date: 2023-04-03)

This version focuses more on improving documentation for existing functions, so that the manual can better function as kind of textbook as the primary intention for the package was to allow students to experiment and learn about various MCDA methods.

Updated documentation for functions:
* Electre_1
* Electre_1S
* Electre_2
* Electre_3
* Electre_4
* Electre_TRI
* FuzzyTOPSIS
* FuzzyVIKOR
* PROMETHEE_I
* PROMETHEE
* PROMETHEE_II
* PROMETHEE_III

Other changes:
* ELECTRE1_Kernel - now uses plot.prefM in more appropriate way (as it should for S3 method)
* Electre_1 - corrected the way the function presents matrices in VERBOSE mode (to the console) and as the returned values
* Electre_2
  - corrected small error in evaluating of discordance's d+ threshold in strong domination evaluation
  - was generating too extensive debug information
* FuzzyTOPSIS - small adjustments of the code for better readability
* PROMETHEE_III - corrected small issue in consistency of presenting partial preference (was using p- instead of P-)

# MCDASupport v0.24 (Release date: 2022-12-20)

The version changes focuses on consistency of the package behavior regarding the VERBOSE parameter. Main motivation is to proceed on the road to prepare package for batch computations  support in the future.

Other errors corrected:
* Electre_1 - did not correctly process outputs from ELECTRE1_Kernel (the graph did not correctly propagate to final outpust of the function)
* Electre_1S - error in the way the function communicated problem with consistency of the thresholds (check itself was fine)
* Electre_1S - did not correctly process outputs from ELECTRE1_Kernel (the graph did not correctly propagate to final outpust of the function)
* Electre_3 - adjusted some error messages to be more easy to understand
* Electre_TRI - error in the way the function communicated problem with consistency of the thresholds (check itself was fine)

# MCDASupport v0.23 (Release date: 2022-10-22)

This update focuses on improving documentation side of the package. Adjusted documentation for basic information on the package. Revision history is now part of documentation and so is list of supported MCDA methods.

# MCDASupport v0.22 (Release date: 2022-09-30)

Changes: Relatively small change in code, which changes default behavior of the functions. In earlier versions they dumped their outputs into the console. If you want this behavior, you need to set VERBOSE parameter to TRUE. Intend was to make it easier to use functions in R markdown in "serial" mode to explore possible solution space with changes of input parameters (i. e. sensitivity analysis).

Allows users to better control outputs to the file. As a downside users will need to actively choose, what needs to be printed.

Adjusted functions are:

- ELECTRE I
- ELECTRE 1S
- ELECTRE II
- ELECTRE III
- ELECTRE IV
- ELECTRE TRI
- FuzzyTOPSIS
- FuzzyVIKOR
- WSM
- PROMETHEE II
- SIR
- TOPSIS
- VIKOR

Other changes:
* Electre_4 function was too much verbose and printed its outputs multiple times
* SIR corrected function documentation, minmax parameter was missing
* SIR replaced T value for SAW for TRUE (syntactically more correct)
* SIR documentation - improved example - line was too long and could get truncated in PDF version of the manual
* SIR documentation - also corrected SAW = TRUE in example, to avoid errors
* SIR documentation - adjusted T/F to TRUE/FALSE as to correspond to the changes in code
* SIR documentation - also parameters w and d were switched in documentation
* FuzzyVIKOR corrected function documentation, v parameter was missing
* FuzzyTOPSIS - missing comma in parameters (usage section)
* VIKORIndexes corrected function documentation, v parameter was missing
* VIKOR documentation - default parameters values were missing
* TOPSIS documentation - default parameters values were missing
* Electre_3 documentation - use FALSE to betters correspond to code

# MCDASupport v0.21 (Release date: 2022-05-11)

Changes:
* implementation of SIR method
* added support for SIR-SAW aggregation procedure (default mode of aggregation)
* added support for SIR-TOPSIS aggregation procedure - not validated and buggy

# MCDASupport v0.20 (Release date: 2022-05-06)

Changes:
* added FuzzyVIKOR method
* corrected small error in documentation of FuzzyTOPSIS method (usage section did not have alt parameter)
* refactored FuzzyVIKOR SR function to separated function to be used by both FuzzyVIKOR and VIKOR funcions
* refactored VIKOR function to improve original SR function to compute Q and compromise solution for both VIKOR and FuzzyVIKOR functions

# MCDASupport v0.19 (Release date: 2022-04-12)

Changes:
* implementation of FuzzyTOPSIS method
* the method has been validated against expected results as per example in Multiple Criteria Decision Aid Methods, Examples and Python Implementations.

# MCDASupport v0.18 (Release date: 2022-03-28)

Changes:
* added additional check on consistency of performance matrix for all supported MCDA methods
* no changes in API (didn't generate documentation for v0.18, as v0.17 is still valid)

# MCDASupport v0.17 (Release date: 2022-03-26)

Changes:
* implementation of TOPSIS method

# MCDASupport v0.16 (Release date: 2022-03-24)

Changes:
* corrected small typo in PROMETHEE documentation
* implementation of VIKOR method
* VIKOR method validated

# MCDASupport v0.15 (Release date: 2022-03-24)

Changes:
* small changes due to validation process of PROMETHEE functions
* rewritten PROMETHEE preference degree logic
* added logic to compute Gaussian type of the 'shape' for PROMETHEE function (preference degree) as it was missing in previous version
* improved readability of the method results
  - it is clearer now, what criterion is the pairwise comparison describing
  - same treatment for preference degree
* completely rewritten PROMETHEE flow logic and validated the procedure
* PROMETHEE methods are considered validated now

# MCDASupport v0.14 (Release date: 2022-02-23)

Changes:
* experimental implementation of PROMETHEE III function
* corrected slight error in example in PROMETHEE function
* PROMETHEE functions were not properly exported into package NAMESPACE

# MCDASupport v0.13 (Release date: 2022-02-22)

Changes:
* experimental implementation of PROMETHEE function for future usage by PREMETHEE I-II functions
* experimental implementation of PROMETHEE_I function for (PROMETHEE I method)
* experimental implementation of PROMETHEE_II function for (PROMETHEE II method)
* the implementation of PROMETHEE methods should be considered experimental, while rudimentary validation of results has been performed on them to prove, that the implementation works more or less as intended, procedure implementation may still contain various problems

# MCDASupport v0.12 (Release date: 2022-02-16)

Changes:
* dedicated to cleaning of the library code
* removing library and require calls to libraries attached by depends
* adjusted signatures of plot.prefM and plot.scoreM methods to safer work with generics
* registering S3 methods prefM and scoreM extending generic function plot
* Electre_2: consistency check for Discordance level d- > d+ (must be d- <= d+) was not functioning correctly (execution stoped by error of code generating error message instead of correctly stoping execution and reporting the problem)
* norm_ZavadskasTurskis: corrected typo in variable name
* revisited dependencies the library uses - should improve consistency of library behavior in different production environments
* corrected error in documentation of Electre_1S function (API called Electre_1 function instead), thou the function parameters were correct (and so was an example).
* corrected typo in documentation of the mcda_get_dominated function (one parameter misspelled)
* mcda_wsm added minmaxcriteria parameter into function signature documentation
* util_pm_minmax: corrected typo in function's parameter PM documentation

# MCDASupport v0.11 (Release date: 2022-01-15)

Changes:

* implemented Markovic normalization (norm_markovic)
* implemented Tzeng and Huang method for normalization (norm_TzengHuang)
* implemented Peldschus et al. (1983) method for nonlinear normalization (norm_nonlinear)
* implemented Lai & Hwang (1994) method for normalization (norm_LaiHwang)
* implemented Zavadskas and Turskis (2008) normalization method (norm_ZavadkasTuskis)
* corrected some errors in documentation of various functions
* corrected error in preference matrix size detection in function mcda_rescale_pm

# MCDASupport v0.10 (Release date: 2021-10-22)

Changes:
* added implementation of Electre 1S method
* refactored ELECTRE III implementation - concordance index is now separate function to be also utilized by ELECTRE 1S
* refactored ELECTRE I implementation - procedure to identify kernel of the solution has been refactored into separate function as it is being used by both ELECTRE I and 1S methods.

# MCDASupport v0.9 (Release date: 2021-10-19)

Changes:
* small correction in error messages in Electre 3, 4 - some were referring to number of alternatives, when number of criteria was meant
* experimental implementation of ELECTRE Tri method

# MCDASupport v0.8 (Release date: 2021-10-15)

Changes:
* corrected small error in final ranking establishing in Electre 3 method
* refactored Electre 3 parts into separate functions to be used also for computation of Electre 4 method
* implementation of Electre 4 method
* improved implementation of distilation (descending/ascending) for cases when the distinguishion between the alternatives using cut-off, was not possible (these alternatives are assigned en-block at the end of the ranking as it is not possible to distinguish between them)

# MCDASupport v0.7 (Release date: 2021-10-12)

Changes:
* improved final information preorder in Electre_2 method - now ordered from best to worst and changed index to start with 1 (istead of 0)
* continuing validation of Electre_3 method, now:
  - downward procedure D1 and first distilation validated
  - reimplemented descending and ascending distillation procedures based on much more elegant algorithm from MCDA package by Umit Yildirim. Computation in MCDA package ends with these procedures, so it does not provide final ranking.
  - reviewed preorder matrix, slightly improved efficiency and also adjusted to use dataframe as input
* package now requires dplyr to function properly
* removed dependency on igraph - the dependency is still implied by usage of dplyr
* removed Electre III method implementation from outranking tools

# MCDASupport v0.6 (Release date: 2021-10-06)

Changes:
* added rounding to concordance matrix to improve readability of results and corrected small error in computation
* concordance matrix computation for Electre 1 and 2 methods has been successfully validated
* reimplemented discordance matrix computation for Electre_1 and Electre_2 methods
* Electre_1 method looks usable
* identified some problems in discordance matrix implementation in Electre_1OT (implementation from Outranking tools) leading again to remove the method

# MCDASupport v0.5 (Release date: 2021-10-05)

Changes:
* Electre_3 method now partially usable - the computation is correct up to first distilate of downward distillation process. Method is still not to be used in production environment
* corrected error in plotting plot.prefM in Electre3* methods
* corrected errors in mcda_wsm method
* added sorting for alternatives in mcda_wsm and added performance matrix to method output (for consistency reasons as other methods in package do have it in output)
* minor corrections in mcda_wsm method documentation
* corrected errors in mcda_get_dominated, also small optimizations of the code
* added plot.scoreM method to visualize performance of the alternatives
* adjusted mcda_wsm to make use of plot.scoreM to better visualize results
* identified problems with Electre_1 method - in the way method computes discordance matrices, as result returned method Electre_1 from original outranking tools under name Electre_1OT

# MCDASupport v0.4 (Release date: 2021-09-30)

Changes:
* new implementation of ELECTRE III method (function Electre_3) inspired by API of pyDecisions, thou most parts of the method have been implementated from scratch.
* the method right now is in for testing purposes, for stable (usefull) outcomes use Electre3 family of functions (from original Outranking Tools package)
* added few widely used normalization (and standardization) methods
  * norm_minmax - min-max normalization
  * norm_zscore - standardized score
  * norm_toaverage - normalize by comparing to the mean of the vector
  * norm_tobest - normalize by comparing to highest value
  * norm_logarithm - logarithmic normalization
  * norm_vector - vector normalization
  * norm_linearagreg - linear normalization by aggregating values

# MCDASupport v0.3 (Release date: 2021-09-14)

Changes:
* Rewriten ELECTRE I (Electre_1) function - now the function is implementation of pyDecisions in R. Reimplemented for better readability
* refactored concordance and discordance indexes into separate functions as they are shared between ELECTRE I and II methods
* improved util_prepare_minmaxcriteria function (some edge cases were not working correctly)
* improved NAMESPACE (there were some problems with exported functions and hence ability for the user to install the package)
* implemented ELECTRE II (Electre_2) function

# MCDASupport v0.2 (Release date: 2021-08-20)

Changes:

* implemented mcda_get_dominated function
  * added basic input information validity checks
  * added support for specifying orientation of the criteria (min or max)
* implemented mcda_del_dominated function
  * added basic validity chcecks
* implemented plot.prefM function to better plot preference matrices
  * added basic validity checks for parameters
* adjusted Electre functions to use this prefM variant instead of normal plot function
* implemented mcda_rescale_pm for rescaling of numeric performance matrices
  * added basic validity chcecks for parameters
* created function util_pm_minmax and util_prepare_minmaxcriteria to simplify refactor and simplify other analytical functions
  * refactored all *_min.R functions to utilize these

# MCDASsupport v0.1 (Release date: 2021-08-12)

Changes:
* Electre_1 method
  * cleaned
  * added export of preference matrix representing computed dominance relation between alternatives from Electre_1 function
  * Refactored Electre_1 - part responsible for saving output of the function into file has been exported into separate function Electre_1_output
  * added Electre_1_min wrapper method to simplify usage of Electre_1 in situations when dataframe contains information on alternatives and criteria
  * adjusted documentation to indicate Electre II method is Electre I using thresholds
* Electre3 method Electre_SimpleThresholds
  * cleaned
  * method adjusted to publish adjancy matrix - before matrix was used only internaly to draw graph but the user did not have access to it
  * Added Electre3_SimpleThresholds_min to simplify usage if dataframe with row and col names is provided as input (performance matrix)
* Electre method Electre3_AlphaBetaThresholds
  * cleaned
  * method adjusted to publish adjancy matrix - before matrix was used only internally to draw graph but the user did not have access to it
  * Added Electre3_AlphaBetaThresholds_min to simplify usage if dataframe with row and col names is provided as input (performance matrix)
* Electre TRI
  * cleaned
  * function now returns computed objects
