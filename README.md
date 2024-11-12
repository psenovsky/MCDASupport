# MCDASupport R Package

 MCDASupport R package aims to give the educators tool in their hands to both demonstrate the usage of modern multiple criteria decision analysis and explore inner working of them, its quirks and limitations in the code and mathematical apparatus in accompanying documentation. Apart from MCDA methods it supports also various normalization methods and some methods for criteria weight estimation.
 
## Description

implemented MCDA methods:

- ARAS
- CoCoSo
- CODAS
- CRADIS
- CRITIC
- EDAS
- ELECTRE (variants I, II, III, IV, 1S and TRI)
- GRA
- MARCOS
- MOORA
- MOOSRA
- PROMETHEE (variants I, II and III)
- PSI (also weight derivation method)
- SAW
- SIR (normal and fuzzy)
- TODIM
- TOPSIS (normal and fuzzy)
- VIKOR (normal and fuzzy)
- WASPAS
- WISP (and S-WIPS)
- WPM (Weighted Product Method)
- WSM (Weighted Sum Method)

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

For weight establishment the package supports:

- binary pair-wise comparison
- COPRAS
- DEMATEL
- EWM (Entropy Weight Method)
- MEREC
- PSI (also MCDA method)

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
install.packages("MCDASupport_0.34.tar.gz", repos=NULL, type="source")  
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

### MCDASupport v0.34 (Release date: 2024-11-17)

new models:
* implemented WPM model
* implemented WASPAS model
* implemented MOORA model
* implemented MOOSRA model
* implemented GRA model
* implemented PSI model
* implemented EDAS model
* implemented MEREC model
* implemented WISP and S-WIPS model
* implemented TODIM model
* implemented CoCoSo model
* implemented CODAS model

other changes:
* corrected some typos in documentation
* reimplemented tobest normalization. Now returns values in <0;1> and allows processin both benefit and cost criteria
* added examples to valitation functions

### MCDASupport v0.33 (Release date: 2024-10-17)

In this version the codebase is again refactored. This time validation functions are separated into separate environment volidation to be used across whole codebase and simplify it to some extent. On one hand it reduces number of files required for purposes of parameter validation.

As side effect parameter requirements of the methods are easier to interpret as they are dirrectly incorporated into instance criation of the model.

Other changes:
* summary function in ELECTRE 1 now produces more consisten outputs to the console.
* there was en error for PROMETHEE preference validation checks (the check did not work properly)

### Full version history

For full version history see [[NEWS]].md file.

## License

MCDASupport R package is a library with various functions for computations around multiple criteria analyses

  Copyright (C)  2024 by Pavel Šenovský

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License (GPL) as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program (see [[LICENSE]] ).  If not, see <http://www.gnu.org/licenses/>.
