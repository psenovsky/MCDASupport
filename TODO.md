# TO DO

In this file some ideas for further development of the package. 

These ideas are provided AS IS - without any priority or promise of implementing them anytime soon, or even ever. But if you are interested, you can pick up idea and contribute its solution to the project :-).

Also the ideas are unsorted.

## Ideas

### Package

- explore possibility to work with the object returned by the function as a class.
- PROMETHEE functions family - it could be interesting to transform information provided by the flow vectors into single dataframe. At least it would simplify return sigrature for the function.
- MCDA Support package - implement more advanced sensitivity analysis. Existing implementation of the analysis creates search space using current parameters value +/- 10 %, which is mostly insufficient to demonstrate changes in provided results
- summary() function should be supported
- review some metainformation in the package
- fuzzy numbers
    - explore possibility in FuzzyTOPSIS generaliza deffuz function to triangle fuzzy number
    - oportunity - refactor internal deffuz function for trapezoid čísla to "crisp hodnotu" to normal function - could be usefull to provide fuzzy functionality to other functions in efficient way
- SIR
    - opportunity for SIR function to optimize as it shares relatively lot of code with PROMETHEE function. Maybe refactor common code to independent function and simplify the parent functions
    - same opportunity exists for SIR and TOPSIS functions
- Explore possibility to implement other methods in PROMETHEE family
    - PROMETHEE IV
    - PROMETHEE GAIA
    - PROMETHEE VI
    - PROMETHEE GDSS
    - PROMETHEE TRI
    - PROMETHEE CLUSTER
    - PROMETHEE GKS
- explore possibility to implement unit testing for the package
- revisit graphing capabilities
    - at present time the package has too many dependencies to be part of CRAN, with clever refactoring of the graph functions some required packages could be declared as recommended (but not required)
    - This should apply especially to visGraph and Plotly packages and functions: plot.prefM(), plot.scoreM(), rankgraph()
- an alternative way of how to express uncertainties in the decision making is to use grey theory. This theory can be seen as an alternative to fuzzy numbers.
    - example in literature could be grey-TOPSIS
    - robust implementation could open this functionality to other methods
- there are many methods to derive weights, such as: SWARA, KEMIRA, SIMOS, P-SWING, PIPRECIA, FUCOM, DEMATEL, CRiteria Importance Through Inter-criteria Correlation (CRITIC)
- review the code of the functions to decrease number of functions the package provides, while maintaining current (or even extended) functionality of it. 
    - good example could be normalization function - instead of using norm_* functions use normalize(vectorToNormalize, method = methodName)
    - this approach could be generally applicable on other similarly structured functions
    - (the real complexity of the package would remain same, it just would not be directly visible to the end user)

### Documentation

- while the functions at present time are documented, there is lot of typos and small issues in clarity of text. It would be nice if somebody went through the documentation and submited its corrections.

## Ideas that will probably be never implemented

- AHP and ANP methods - while these methods are quite popular, there are already many tools available, including in R. Implementation of the methods in the package would not have any impact.