#===
# ANALYSIS SCENARIO 1
#===
# additional 10mins daily walking

# Estimate BEFORE scenario relative risks
rr.metmins <-  a^(metmins/b*3)^t

# Change METmins according to scenario, 10mins, 7 days, 3METs = 210METmins
metmins.new <- metmins + 210

# estimate AFTER scenario relative risks
rr.metmins.new <- a^(metmins.new/b*3)^t

# estimate change in relative risk in linear model
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3
rr.S1.ldrf <- 1 - (1 - 0.89) * (increase.walk/168)  # scenario 1 is 10 mins walking per person

# estimate reductions in deaths per year.
results <- f.getresults(risk.change.lin = rr.S1.ldrf, 
                        df = merged, 
                        v.countries = countries)

# store results
results.table <- data.frame(ISO_Code = merged$ISO_Code,
                            country  = merged$country,
                            IPAP     = merged$IPAP,
                            S1_DA_drf= results$drf,
                            S1_DA_lin= results$lin,
                            S1_NMB_drf= results$drf * merged$VSL/100000,
                            S1_NMB_lin= results$lin * merged$VSL/100000,
                            S1_NMB_relative = (results$nmb_drf-results$nmb_lin)/results$nmb_lin*100)
