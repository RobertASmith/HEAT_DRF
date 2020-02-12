# ====== #
# Author:   Robert Smith
# Contact:  rasmith3@sheffield.ac.uk
# Project: HEAT Dose Response Function
# Description: This script loads all necessary functions and then runs the HEAT model, and the VSLY model for:
#              Each of three scenarios, with sensitivity analysis for various values of t. This means a lot of data.  
# ====== #


# ========= SETUP

rm(list=ls())

# multi package check, install and n load function courtesy of Paul Schneider
install_n_load <- function(packages){
  for(package in packages){
    if(package %in% rownames(installed.packages()) == FALSE) {
      print(paste("installing",package))
      install.packages(package)
    }
    eval(parse(text=paste('require(',package,')')))
  }
}

required_packages = c('knitr','survival','tidyverse',
                      'stringr','pdftools','reshape2','ggplot2',
                      'tidyr','mc2d','ggrepel','knitr','xtable',
                      'gridExtra','kableExtra','dplyr','rgeos','rworldmap',
                      'flextable','viridis','rlang','readxl','foreign','Rcpp')

install_n_load(required_packages) 

# load and clean data
  source(file = "functions/load_and_clean.R")
# load function which runs the model.
  source(file = "functions/model.R")
# load plotting functions
  source(file = "functions/plotfunctions.R")
# load model functions
#  source(file = "functions/modelfunctions.R") # DEPRECATED.
# load results table function
#  source(file = "functions/results_tables.R") # DEPRECATED.

#====
# Plot of the different relative risks using:
# linear relationship, power transformation 0.25,0.375,0.5,0.75
#====

  source(file = "functions/relativerisksplot.R")
  
#===
# See plot of PA distributions for 5 countries.
# Done in ggplot in seperate pdf.
#===

temp <- melt(data = metmins,value.name = "metmins",variable.name = "country") %>%    # melt dataset so can be used by ggplot
  group_by(country) %>%               # group by country
  mutate(percentile = row_number())   # give numbers based on percentiles

# create lineplot with percentiles on x axis and met-mins on right axis for several countries.
ggplot(data = temp %>% filter(country %in% c("Andorra","France","UK","Bulgaria","Greece","Spain")), 
       aes(x = percentile,y = metmins,col = country)) +
  geom_line()

temp <- kable(metmins)

#=================== INITIALISE ===============================#

countries <- intersect(merged$country,colnames(metmins))   # identify countries which can analyse
#merged$drf <- NA                              # create column for dose response estimate net benefit
#merged$lin <- NA                              # create column for linear response estimate net benefit

# set parameters as from HEAT manual, 168mins at 3METS, 0.89 RR.
b = 168 ; a = 0.89 ; p = 1:3000

#====
# ANALYSIS
#====

# creates figures and tables of results in output folder.
# each time compares results using t=0.375, t=0.25, t=0.5,t=0.75.

sensitivity <- list( 
  
  fig.path =     c('output/t375/figures',
                   'output/t25/figures',
                   'output/t50/figures',
                   'output/t75/figures'),
  
  results.path = c('output/t375/results',
                   'output/t25/results',
                   'output/t50/results',
                   'output/t75/results'),
  
  t =      c(0.375, 0.25, 0.5, 0.75),
  
  scenario_path = c("S1",
                    "S2", 
                    "S3"),
  
  scenario_names = c("Scenario 1: Every person walks an additional 10 mins daily",
                     "Scenario 2: Every person meets WHO Guidelines",
                     "Scenario 3: 10% Increase in PA for every person")
  
)

# RUN THE MODEL

# run the analysis for each value of t, send results to output results.
for(s in 1:4){

  # scenario 1
  
  f_model(t = sensitivity$t[s],
               metmins = metmins,
               a = a,
               increase = 210,
               b = b,
               merged = merged,
               scenario = 1,
               countries = countries,
               s = s,
               sensitivity = sensitivity)
  # scenario 2
       
  f_model(t = sensitivity$t[s],
                    metmins = metmins,
                    a = a,
                    b = b,
                    merged = merged,
                    scenario = 2,
                    countries = countries,
                    s = s,
                    sensitivity = sensitivity)
       
  # scenario 3
       
  f_model(t = sensitivity$t[s],
                    metmins = metmins,
                    a = a,
                    b = b,
                    merged = merged,
                    scenario = 3,
                    countries = countries,
                    s = s,
                    sensitivity = sensitivity)
       
}


# this file creates all the plots for t = 0.375, it can easily be adapted to create plots for other levels of t
source('functions/all_plots.R')


# fin :)




