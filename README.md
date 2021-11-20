![GitHub last commit](https://img.shields.io/github/last-commit/RobertASmith/HEAT_DRF?color=red&style=plastic)
![GitHub top language](https://img.shields.io/github/languages/top/RobertASmith/HEAT_DRF?style=plastic)
![GitHub repo size](https://img.shields.io/github/repo-size/RobertASmith/HEAT_DRF?style=plastic)
[![GitHub release](https://img.shields.io/badge/R-HEDS-green)](https://img.shields.io/badge/R-hello-green)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# A comparison of WHO-HEAT model results using a non-linear physical activity dose response function with results from the existing tool.

Authors: Robert Smith<sup>1</sup>, Chloe Thomas<sup>1</sup>, Hazel Squires<sup>1</sup> & Elizabeth Goyder<sup>1</sup>

<sup>1</sup> School of Health and Related Research, University of Shefﬁeld, Regents Court, UK, S1 4DA

Corresponding Author: Robert A Smith (rasmith3@sheffield.ac.uk)

Robert Smith: Conceptualization, Methodology, Visualization, Investigation, Writing- Original draft preparation.
Chloe Thomas: Supervision, Writing- Reviewing and Editing. 
Hazel Squires: Supervision, Writing- Reviewing and Editing. 
Elizabeth Goyder: Supervision, Writing- Reviewing and Editing.

Keywords: Physical Activity, Dose Response Function, HEAT, Walking, Cycling.

Acknowledgements:  The authors would like to thank Martin Stepanek & Marco Hafner for detailed explanations explaining their method to estimate country physical activity distributions. We would also like to thank Sonja Kahlmeier, Thomas Gostski & Alberto Castro Fernandez for providing details of and access to the HEAT model and data.

Funding Sources: Robert Smith is joint funded by the Wellcome Trust Doctoral Training Centre in Public Health Economics and Decision Science [108903/Z/19/Z] and the University of Shefﬁeld

## Repository structure & replication
- the 'scripts' folder contains the scripts necessary to replicate the analysis conducted in the paper. 
  -  The distributions_create.R file creates the country level physical activity distributions as described by Hafner et al. 
  -  The analysis.R undertakes the analysis discussed in the paper.
- the R folder contains all functions required for the analysis.
- the 'output' folder contains the outputs generated by running the analysis.R file.
- the data folder contains data which is read in during the cleaning phase.
- This work was originally undertaken during a trip to Zurich in 2018, when the author was new to R. I apologise sincerely for the structure of the code, ideally I would refactor this code & set up the repository from scratch.

To replicate the analysis run:

``` r
source("scripts/analysis.R")
```
This will run the 'analysis.R' script which:
``` r
1) loads and cleans all necessary data, stored in the 'data' folder, data and merges the data into a single dataframe.
2) reads in the model function used to compare the three scenarios using the HEAT with a linear and a dose-response function.
3) reads in the plotting functions to visualise the results.
4) uses ""R/relativerisksplot.R"" to plot the relative risk functions based upon Woodcock et al. 
5) runs the analysis described in the publication for three different scenarios and stores results as .
6) uses 'R/all_plots.R' to read in all the results from the csvs created above, and plots the results, saving them to the 'outputs' folder.
```
As I mentioned, it would be nice to have time to refactor this code, but unfortunately this project was time limited. If you are interested in building upon this work please contact me directly.

## Plot from the analysis
![The estimated benefits of increased walking for a country depend on the shape of the dose response function between physical activity and mortality, and the distribution of physical activty in the country.](https://github.com/RobertASmith/HEAT_DRF/blob/c9bc4c6306a71a8fedbe1906d27ee4da418c8324/output/t375/figures/S1_RelativeResults.pdf)
Many thanks,

Rob


