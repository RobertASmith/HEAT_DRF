#===
# ANALYSIS SCENARIO 2
#===

# everyone meeting guidelines

# Estimate BEFORE scenario relative risks
rr.metmins <- a^(metmins/b*3)^t

# Change METmins according to scenario
metmins.new <- metmins  # create a new set of met-mins equal to old ones
metmins.new[metmins<600] <- 600 # Change metmins of those lower than 600 mets to 600.

# estimate AFTER scenario relative risks
rr.metmins.new <-  a^(metmins.new/b*3)^t

# estimate change in relative risk in linear model
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3
rr.S2.ldrf <- 1 - (1 - 0.89) * (increase.walk/168)  # scenario 1 is 10 mins walking per person

# estimate reductions in deaths per year.
results <- f.getresults(risk.change.lin = rr.S2.ldrf, 
                        df = merged, 
                        v.countries = countries)

# store results
results.table$S2_DA_drf       = results$drf
results.table$S2_DA_lin       = results$lin
results.table$S2_NMB_drf      = results$drf * merged$VSL/100000
results.table$S2_NMB_lin      = results$lin * merged$VSL/100000
results.table$S2_NMB_relative = (results$nmb_drf-results$nmb_lin)/results$nmb_lin*100
