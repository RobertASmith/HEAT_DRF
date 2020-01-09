# set-up libraries etc.
  source(file = "functions/setup.R")
# load plotting functions
  source(file = "functions/plotfunctions.R")
# load model functions
  source(file = "functions/modelfunctions.R")
# load results table function
  source(file = "functions/results_tables.R")
# load and clean data
  source(file = "functions/load_and_clean.R")

#====
# Plot of the different relative risks using:
# linear relationship, power transformation 0.25,0.375,0.5,0.75
#====

  source(file = "functions/relativerisksplot.R")
  
#===
# See plot of PA distributions for 5 countries.
# Done in ggplot in seperate pdf.
#===

#=================== INITIALISE ===============================#

countries <- intersect(merged$country,colnames(metmins))   # identify countries which can analyse
merged$drf <- NA                              # create column for dose response estimate net benefit
merged$lin <- NA                              # create column for linear response estimate net benefit

# set parameters as from HEAT manual, 168mins at 3METS, 0.89 RR.
b = 168*3 ; a = 0.89 ; p = 1:3000

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
  
  t =      c(0.375, 0.25, 0.5, 0.75)
  
)

# run the analysis for each value of t
for(s in 1:4){

  # change parameters, t is either 0.375, 0.25,0.5 or 0.75
  t <- sensitivity$t[s]
  
  # where the files are saved to changes based on t
  path = sensitivity$fig.path[s]
  
  # run scenarios given the new value of t
  source(file = "functions/scenario1.R")
  source(file = "functions/scenario2.R")
  source(file = "functions/scenario3.R")
  
  # save tables to appropriate path
  saveRDS(object = results.table,file = paste(sensitivity$results.path[s],sep = "/","results.R")) # store results table as R file.
  f.results.table(path = sensitivity$results.path[s])
  
  # create plots, uses path from above
  source('functions/all_plots.R')
}

