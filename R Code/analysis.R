# set-up libraries etc.
source(file = "functions/setup.R")
# load plotting functions
source(file = "functions/plotfunctions.R")
# load model functions
source(file = "functions/modelfunctions.R")
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
b = 168*3 ; a = 0.89 ; p = x
# t is difficult, varied in sensitivity analysis later
t <- 0.375

# run scenarios
source(file = "functions/scenario1.R")
source(file = "functions/scenario2.R")
source(file = "functions/scenario3.R")

saveRDS(object = results.table,file = "data/results.R") # store results table as R file.

#==================== SAVE TABLE AS PDF ============
# Scenario 1 Results
s1.results.table <- results.table %>% 
                      select(ISO_Code, country, S1_DA_drf, S1_DA_lin, S1_NMB_drf, S1_NMB_lin)
s2.results.table <- results.table %>% 
                      select(ISO_Code, country, S2_DA_drf, S2_DA_lin, S2_NMB_drf, S2_NMB_lin)
s3.results.table <- results.table %>% 
                      select(ISO_Code, country, S3_DA_drf, S3_DA_lin, S3_NMB_drf, S3_NMB_lin)

colnames(s1.results.table) <- c('ISO3 Code', 'Country', 'DRF', 'Lin','DRF','Lin')
colnames(s2.results.table) <- c('ISO3 Code', 'Country', 'DRF', 'Lin','DRF','Lin')
colnames(s3.results.table) <- c('ISO3 Code', 'Country', 'DRF', 'Lin','DRF','Lin')

# excel tables, for publication if latex not accepted.
write.csv(x =  s1.results.table,file = "s1_results.csv")
write.csv(x =  s2.results.table,file = "s2_results.csv")
write.csv(x =  s3.results.table,file = "s3_results.csv")

# latex table
#kable <- kable(x = s3.results.table,
#               caption = "Results from Scenario 1",
#               format = "latex",digits = 2) %>%
#  kable_styling(latex_options = c("striped", "scale_down"),font_size = 8) %>% 
#  add_header_above(c(" "," ", "Deaths Averted" = 2, "Net Monetary Benefit" = 2))
#
path = 'figures/t375'
# create all plots.
source('functions/all_plots.R')


#====
# SENSITIVITY ANALYSIS
#====

# change parameters
t <- 0.375
path = 'output/t375/figures'
# run scenarios
source(file = "functions/scenario1.R")
source(file = "functions/scenario2.R")
source(file = "functions/scenario3.R")
# save table
saveRDS(object = results.table,file = "output/t375/results.R") # store results table as R file.
# create plots
source('functions/all_plots.R')

# change parameters
t <- 0.25
path = 'output/t25/figures'
# run scenarios
source(file = "functions/scenario1.R")
source(file = "functions/scenario2.R")
source(file = "functions/scenario3.R")
# save table
saveRDS(object = results.table,file = "output/t25/results.R") # store results table as R file.
# create plots
source('functions/all_plots.R')

# change parameters
t <- 0.5
path = 'output/t50/figures'
# run scenarios
source(file = "functions/scenario1.R")
source(file = "functions/scenario2.R")
source(file = "functions/scenario3.R")
# save table
saveRDS(object = results.table,file = "output/t50/results.R") # store results table as R file.
# create plots
source('functions/all_plots.R')

# change parameters
t <- 0.75
path = 'output/t75/figures'
# run scenarios
source(file = "functions/scenario1.R")
source(file = "functions/scenario2.R")
source(file = "functions/scenario3.R")
# save table
saveRDS(object = results.table,file = "output/t75/results.R") # store results table as R file.
# create plots
source('functions/all_plots.R')


