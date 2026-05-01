# MCDASupport R Package

 MCDASupport R package aims to give the educators tool in their hands to both demonstrate the usage of modern multiple criteria decision analysis and explore inner working of them, its quirks and limitations in the code and mathematical apparatus in accompanying documentation. Apart from MCDA methods it supports also various normalization methods and some methods for criteria weight estimation.
 
## Description

implemented MCDA methods:

- ARAS
- AROMAN
- Balanced SPOTIS (AKA B-SPOTIS)
- Borda count method
- CoCoSo
- CODAS
- Copeland's method
- COPRAS
- CRADIS
- EDAS
- ELECTRE (variants I, II, III, IV, 1S and TRI)
- ERVD
- EVAMIX
- Extended AROMAN
- FUCA
- GRA
- MABAC
- MACBETH
- MAIRCA
- MARCOS
- MARE
- MAUT
- MOORA
- MOOSRA
- MSIM
- OCRA
- ORESTE
- PAMSSEM (variants I (not validated) and II)
- PIV (Proximity Indexed Value)
- PROMETHEE (variants I, II and III)
- PSI (also weight derivation method)
- PROBID (WIP - not validated)
- QUALIFLEX
- R method
- RAM
- RAFSI
- RAWEC
- REGIME
- RIM
- ROV
- SAW
- SECA
- SIR (normal and fuzzy)
- SMART
- SPOTIS
- sPROBID (WIP - not validated)
- TODIM
- TOPSIS (normal and fuzzy)
- VIKOR (normal and fuzzy)
- WASPAS
- WISP (and S-WIPS)
- WPM (Weighted Product Method AKA MEW)
- WSM (Weighted Sum Method)

For normalization, the package supports:

- Lai and Hwang normalization
- Linear normalization using aggregation of values
- Normalization of values by applying logarithmic transformation.
- Markovic normalization
- Min-max method (AKA max-min or Weitendorf normalization)
- nonlinear normalization
- RIM normalization
- to average normalization
- to best normalization
- Tzeng and Huang normalization
- vector normalization
- Zavadskas and Turskis normalization (AKA JKN method)
- Z-score

For weight establishment the package supports:

- BWM (Best-Worst Model)
- DEMATEL
- objective weighting methods:
  - angle weighting
  - CILOS (Criterion Impact LOSs)
  - CRITIC (Criterion Importance Through Intercriteria Correlation)
  - EWM (Entropy Weight Method)
  - GCW (Gini Coefficient Weighting Method) 
  - IDOCRIW (Integrated Determination of Objective CRIteria Weights)
  - MEREC (Methoc based on Removal Effects of Criteria)
  - MPSI (M Preference Selection Index)
  - MW (Mean Weighting)
  - SDW (Standard Deviation Weighting)
  - SVW (Statistical Variance Weighting)
- pairwise comparison methods
  - binary pair-wise comparison
  - RANCOM RANking COMparison
  - AHP (Analytic Hierarchy Process)
- PIPRECIA (Pivot Pairwise Relative Criteria Importance Assessment)
- PSI (also MCDA method)
- Rank Ordering Methods (ROMs) namely Rank Sum, Rank Exponent and Rank Reciprocal methods
- SWARA

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

- data.tree,
- diagram,
- dplyr,
- graphics,
- igraph,
- lpSolve
- plotly,
- quadprog
- stats,
- tidyr,
- visNetwork

### Installing

You can use following R code to install package and its dependencies
```R
# check for missing dependencies
packages <- c("mathjaxr", "graphics", "igraph", "diagram", "stats", "dplyr", "visNetwork", "plotly", "tidyr", "data.tree", "quadprog")
install.packages(setdiff(packages, rownames(installed.packages())))  
# adjust name of the file to version you are installing
install.packages("MCDASupport_0.37.tar.gz", repos=NULL, type="source")  
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
result <- electre4$new(PM, P, Q, V, minmaxcriteria)
result$graph # outranking in graphical form
result$final_ranking # final ranking of alternatives
```
See package's built-in documentation for further detail on available functions, its required parameters and provided outputs and its mathematical apparatus.

## Help

TBD

## Authors

The project started as a fork of Outranking Tools package by M. Prombo. Original package is no longer maintained (since 2014) and is available only from CRAN archives at https://cran.r-project.org/src/contrib/Archive/OutrankingTools/. 

Outranking tools had implementations of  Electre I - III methods, which have been reimplemented in MCDASupport using Prombo's code with lots of changes. These updates, as well as all other methods been implemented by Pavel Šenovský (pavel.senovsky@vsb.cz). Who is also current the maintainer of the package.

See function's documentation for information on mathematics and theories methods in this package has been based on.

## Version History

### MCDASupport v0.37 (Release date: 2026-05-01)

backward incompatible changes:
* refactored RANCOM and binary parwise comparison methodd for weight estimation into mcda_pairwise_weights call the function using mcda_pairwise_weights(pm, method = "RANCOM")

new methods:
* new objective weighting method - angle (or angular)
* implemented RANCOM weighting method
* implemented AHP method
* implemented Balanced SPOTIS (or B-SPOTIS) method
* implemented RAFSI method
* implemented SECA model
* implemented R method
* implemented RAM method
* implemented ERVD method
* implemented MARE method

bugfixes
* TOPSIS had broken aplication of weights
* PROMETHEE II function no longer has mandatory pref_function parameter. It will use default preference function if not told otherwise
* corrected error in implementation of VIKOR indexes where nQ was available only for C1 test, but was used in all tests

### Full version history

For full version history see [[NEWS]].md file.

## License

MCDASupport R package is a library with various functions for computations around multiple criteria analyses

Copyright (C)  2025 by Pavel Šenovský

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License (GPL) as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program (see [[LICENSE]] ).  If not, see <http://www.gnu.org/licenses/>.
