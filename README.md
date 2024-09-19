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
install.packages("MCDASupport_0.31.tar.gz", repos=NULL, type="source")  
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

### MCDASupport v0.31 (Release date: 2024-09-20)

The v0.31 presents probably largest ammount of changes between the the versions. During its development over 100 commits were pushed into the repository and basically whole code-base of the package changed. Main goal of the changes was:

* to implement models in form of R6 class, which improves readability of code
* and also allows to implement S3 functions for summary of objects, well at least the models have implemented support of this function.
* sensitivity analysis is also redone to more properly test possible ranges for various thresholds
* as part of changes the documentation switched to Roxygen documentation, making the documentation available directly from the code.

More detailed list of changes for the vesion:
* refactored common code for SIR and TOPSIS into topsis_ideal function
* some refactoring in FuzzyTOPSIS function
* added plot.threshold.3d and plot.threshold functions to visualize senzitivity, paradoxically new approach to computation using R6 classes makes no use of these. Possibly these functions will be removed in future versions (or never leave the development branch)
* refactored Electre_1 and Electre_1_sensitivity into single universally usable R6 class. This approach will be used as model for other methods. 
    * as side effect the summary function for the model object is now supported. 
    * and sensitivity of the solution is computed (in past intervals for parametr testing had to be set manually)
    * please note that this change is API breaking
* refactored Electre_1S and Electre_1s_sensitivity into single R6 class (similarly to implementation of the Electre_1) - implementation should be still considered as experimental and bugy. As a side effect Electre_1s_paramCheck is no longer needed.
* refactored Electre_2 and Electre_2_sensitivity into single R6 class
* refactored sens_compare function from electre2 class into separate function vector_compare. Intention is to use it in all classes for detecting changes in the model solution.
* corrected error in documentation of electre1s::sensitivity function, which prevented the documentation for the function to be rendered
* corrected error for 3 fields in electre2 to render their documentation
* refactored Electre_3 and Electre_3_sensitivity into new R6 class
* refactored Electre_4 and Electre_4_sensitivity into R6 electre4 class
* refactored common parts of Electre III and IV sensitivity testing into sensitivity_e34 function.
* refactored Electre_TRI and Electre_TRI_sensitivity into R6 electretri R6 class
* refactored FuzzyTOPSIS into R6 fuzzytopsis class
* Fuzzy VIKOR
    * refactored FuzzyVIKOR into R6 fuzzyvikor class
    * corrected error in result computation - in R metric the S metric was provided (the overal solution was correct)
* mcda_wsm refactored into wsm R6 class
* rewriten function. Now it better checks the thresholds depending on used preference function. Also improved consistency check for weights.
* refactored PROMETHEE_I and PROMETHEE_I_sensitivity into single R6 class
* refactored PROMETHEE_II and PROMETHEE_II_sensitivity into single R6 class
* refactored shared code for sensitivity testing for PROMETHEE I and II into separate private function
* refactored PROMETHEE_III function into promethee3, including implementing summary function and sensitivity testing.
* refactored SIR function, including implementing summary function and sensitivity testing for SIR-TOPSIS.
* refactored TOPSIS into R6 class
* refactored VIKOR into R6 class. Also corrected error of method not properly showing R-metric (provided S-metric instead). The problem was in broadcasting R-metric only, the computation and overal results were correct.

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

### Full version history

For full version history see [[NEWS]].md file.

## License

MCDASupport R package is a library with various functions for computations around multiple criteria analyses

  Copyright (C)  2024 by Pavel Šenovský

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License (GPL) as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program (see [[LICENSE]] ).  If not, see <http://www.gnu.org/licenses/>.
