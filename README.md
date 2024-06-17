# MCDASupport R Package

 MCDASupport R package aims to give the educators tool in their hands to both demonstrate the usage of modern multiple criteria decision analysis and explore inner working of them, its quirks and limitations in the code and mathematical apparatus in accompanying documentation. Apart from MCDA methods it supports also various normalization methods and EWM method for criteria weight estimation.
 
## Description

implemented MCDA methods:

- WSM (Weighted Sum Method)
- ELECTRE (variants I, II, III, IV, 1S and TRI)
- PROMETHEE (variants I, II and III)
- TOPSIS
- SIR
- VIKOR

TOPSIS, SIR and VIKOR are implemented for normal and fuzzy numbers.

For normalization package supports:

- Lai and Hwang normalization
- Linear normalization using aggregation of values
- Normalization of values by applying logarithmic transformation.
- Markovic normalization
- Min-max method
- nonlinear normalization
- value normalization to average value
- normaliozation to best value
- Tzeng and Huang normalization
- vector normalization
- Zavadskas and Turskis normalization
- Z-score

For weight establishment the package supports EWM (Entropy Weight Method).

The package has also some graphical capabilities to help visualizing network of outranking relations between the alternatives and tools to visualize changes in ranking due to changes in analysis parameters (sensitivity analysis).

Sensitivity analysis at present time is in its infancy. Package has quick and dirty implementation for sensitivity testing, but it seems that current implementation has no potential to actually induce rank changes. More robust implementation of the analysis will be required for that.

At present time the package is officially available only from this repository and not from CRAN as is usual for this type of packages. The reason is that the package IMHO doesn't meet quality criteria to be on CRAN. Main problems and limitations  are:

- there is too much functions available - consolidation of API (with maintaining current capabilities) is required
- functions do not support summary method, which is one of basic functions the users could (and should) expect to function across all packages
- the API is not stable at present time - It is changed quite liberally across versions
- it depends on too many other packages
- speed of development is rather slow, including closing issues
- and many more.

For educational purposes these limitations are not that significant, but for analytic purposes these are rather severe. In short - no CRAN in near future.

## Getting Started

### Dependencies

The "binary" version of the package is provided for last stable version of R (4.3, https://www.r-project.org), but since the the package is fully implemented in R, you should be able to make it work on older versions of R, provided the other required packages are successfully installed first, see installation process for details.

R packages MCDASupport depends on:

- mathjaxr
- igraph,
- diagram,
- dplyr,
- tidyr,
- stats,
- graphics,
- plotly,
- visNetwork

### Installing

You can use following R code to install package and its dependencies
```R
# check for missing dependencies
packages <- c("mathjaxr", "graphics", "igraph", "diagram", "stats", "dplyr", "visNetwork", "plotly", "tidyr")
install.packages(setdiff(packages, rownames(installed.packages())))  
# adjust name of the file to version you are installing
install.packages("MCDASupport_0.29.tar.gz", repos=NULL, type="source")  
```
### Executing program

Installed package needs to be activated in development environment of your choice, typically RStudio: https://posit.co/download/rstudio-desktop/, via GUI or in code:
```R
library(MCDASupport)
```
Example usage of ELECTRE IV method:
```R
# specify decision problem
# start with performance matrix
# alternatives are in the rows, criteria in columns
PM <- cbind(
  c(-14,129,-10,44,-14),
  c(90,100,50,90,100),
  c(0,0,0,0,0),
  c(40,0,10,5,20),
  c(100,0,100,20,40)
)
rownames(PM) <- c("Project1","Project2","Project3","Project4","Project5") # alternatives
colnames(PM) <- c( "CR1","CR2","CR3","CR4","CR5") # criteria
minmaxcriteria <- 'max' # all decision criteria are "benefical"
Q <- c(25,16,0,12,10) # Indifference thresholds
P <- c(50,24,1,24,20) # Preference thresholds
V <- c(100,60,2,48,90) # Veto thresholds
# compute the problem using the method of choice
result <- Electre_4(PM, P, Q, V, minmaxcriteria)
result$graph # outranking in graphical form
result$final_ranking # final ranking of alternatives
```
See package's built-in documentation for further detail on available functions, its required parameters and provided outputs and its mathematical apparatus.

## Help

TBD

## Authors

The project started as a fork of Outranking Tools package by M. Prombo. Original package is no longer maintained (since 2014) and is available only from CRAN archives at https://cran.r-project.org/src/contrib/Archive/OutrankingTools/. 

Outranking tools had implementations of  Electre I - III methods, which have been reimplemented in MCDASupport using Prombo's code with some updates. These updates, as well as all other methods been implemented by Pavel Šenovský (pavel.senovsky@vsb.cz). Who is also current the maintainer of the package.

See function's documentation for information on mathematics and theories methods in this package has been based on.

## Version History

### MCDASupport v0.30 (Release date: 2024-06-17)

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

### MCDASupport v0.29 (Release date: 2023-12-19)

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
* PROMETHEE (general function utilized in PROMETHEE family of functions) imroved efficiency of pairwise comparison and also optimized criteria flows computation
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

### Full version history

For full version history see [[NEWS]].md file.

## License

MCDASupport R package is a library with various functions for computations around multiple criteria analyses

  Copyright (C)  2024 by Pavel Šenovský

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License (GPL) as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program (see [[LICENSE]] ).  If not, see <http://www.gnu.org/licenses/>.
