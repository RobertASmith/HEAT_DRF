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

#===
# CREATE DOSE/LINEAR RELATIVE RISKS 
#===

# New dose response relationship, using Woodcock et al. 2011 (https://doi.org/10.1093/ije/dyq104).
# Have therefore replaced other studies by Kelly et al., 2014 and Aram et al. 2015.
#
# Method basically turns linear relationship plotted here:
  x <-  1:3000        # range of met-mins on x axis
  y <- (1 - (1 - 0.89) * (x/168))  ;  y[y<0.7] <- 0.7   # y axis simply linear relationship

  plot(x = x,
       y = y,
       type = "l", 
       xlab = 'Weekly MET-mins', 
       ylim = c(0.5,1),
       ylab = "Relative Risk",
       main = "Relative risk using linear & non-linear dose response relationship"
       )
  
  legend('topright', 
         cex = 0.7,
         legend = c("Linear","0.25","0.375","0.5","0.75"),
         lty = c(1,2,3,4,5), 
         col = c("black","black","blue","black","black"),
         title = "Power Transformation",
         bty = "n"
         )

# Into a curve using equation:
# RR = a^(p/b)^t     # From Oliver Mytton PhD Thesis page 140
# where a = reference RR, b = reference metmins, t = log transformation, p = physical activity levels (met-mins/wk).
# there is considerable difference in the plots by the value of t

  # set parameters as from HEAT manual, 168mins at 3METS, 0.89 RR.
  b = 168*3 ; a = 0.89 ; p = x
  
  # 0.25 power transformation
  t = 0.25
  lines(x = p, y = a^(p/b)^t, lty = 2)
  
  # 0.5 power transformation
  t = 0.5
  lines(x = p, y = a^(p/b)^t, lty = 4)
  
  # 0.75 power transformation
  t = 0.75
  lines(x = p, y = a^(p/b)^t, lty = 5)
  
  # 0.375 power transformation (as per Woodcock et al.)
  t = 0.375 
  lines(x = p, y = a^(p/b)^t, lty = 3, col = "blue")
  
# I use the value 0.375 throughout as done by Woodcock et al. 2010 (https://doi.org/10.1093/ije/dyq104).
# Then I vary the analysis using the others as sensitivity analysis.

# RR = a^(p/b)^t     # From Oliver Mytton PhD Thesis page 140
# where a = reference RR, b = reference metmins, t = log transformation, p = physical activity levels (met-mins/wk).

# b = 168*3 ; a = 0.88 ; t = 0.375 ; p = 1:3500
# rr <- a^(p/b)^t
# lines(1:3500,rr)

#=================== INITIALISE ===============================#

countries <- intersect(merged$country,colnames(metmins))   # identify countries which can analyse
merged$drf <- NA                              # create column for dose response estimate net benefit
merged$lin <- NA                              # create column for linear response estimate net benefit

# set parameters as from HEAT manual, 168mins at 3METS, 0.89 RR.
b = 168*3 ; a = 0.89 ; p = 1:3000

#====
# SENSITIVITY ANALYSIS
#====

sensitivity <- list( 
  
  fig.path =      c('output/t375/figures',
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

