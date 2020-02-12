#===
# ANALYSIS SCENARIO 1
#===
# additional 10mins daily walking
f_model <- function(a = 0.89, b = 168, t = t, s = s, 
                         metmins = metmins, merged = merged, countries = countries,
                         sensitivity = sensitivity,
                         increase = NULL, scenario = NULL){
  # t is the power transformation, metmins the orginal metmins, 
  # a is the reference relative risk
  # b is the reference physical activity level in mets
  # increase is the extra physical activity in mets (only relevant for scenario 1)
  # metmins is the matrix of initial metmins
  # merged is the initial dataset of country characteristics
  # countries is a vector of country names used in the model.
  # sensitivity is a list of data used as inputs (e.g output names etc.)
  # scenario is the scenario chosen (1,2 or 3)

# =========== #
# NON -LINEAR #
# =========== #
  
# Estimate BEFORE scenario relative risks
rr.metmins <-  a^(metmins/b*3)^t


if(scenario == 1){
  # Change METmins according to scenario, 10mins, 7 days, 3METs = 210METmins
  metmins.new <- metmins + increase
  return(print("scenario1"))
}else{
  if(scenario == 2){
    # Change METmins according to scenario, all those below 600 mets increase to 600 met-mins.
    metmins.new <- metmins ;  metmins.new[metmins.new<600] <- 600
    return(print("scenario2"))
  }else{
    if(scenario == 3){
      metmins.new <- metmins*1.1  # create a new set of met-mins 10% higher
      return(print("scenario3"))
    }else{
      return(print("ERROR")) # return an error if the input is not 1,2 or 3.
    }
  }
}
# estimate AFTER scenario relative risks
rr.metmins.new <- a^(metmins.new/b*3)^t

# calculate risk change in non-linear model 
risk.change <- rr.metmins[,countries] - rr.metmins.new[,countries]

# multiply risk change for each percentile by mortality risk and take mean
merged$drf <- colMeans(risk.change * merged$mortrisk[match(merged$country,countries)])


# ====== #
# LINEAR #
# ====== #

# estimate change in minutes of walking in linear model
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3

# estimate linear function risk change.
rr.S1.ldrf <- 1 - (1 - a) * (increase.walk/b)  # scenario 1 is 10 mins walking per person

# calculate risk reduction in linear model
merged$lin <- (1-rr.S1.ldrf)* merged$mortrisk[match(merged$country,countries)]


#===
# MONETARY BENEFIT estimate monetary benefit for nmb and lin responses
#===

# multiply deaths averted by vsl, for non-linear & linear models
merged$nmb_drf  <- merged$drf * merged$VSL # non-linear
merged$nmb_lin  <- merged$lin * merged$VSL # linear
merged$relative <- (merged$nmb_drf - merged$nmb_lin)/ merged$nmb_lin # calculate dif relative to linear

# estimate reductions in deaths per year.
#results <- f.getresults(risk.change.lin = rr.S1.ldrf, 
#                        df = merged, 
#                        v.countries = countries)

# store results
results.table <- data.frame(ISO_Code = merged$ISO_Code,
                            country  = merged$country,
                            IPAP     = merged$IPAP,
                            S1_DA_drf= merged$drf,
                            S1_DA_lin= merged$lin,
                            S1_NMB_drf= merged$drf * merged$VSL/100000,
                            S1_NMB_lin= merged$lin * merged$VSL/100000,
                            S1_NMB_relative = (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin*100)

# save results as rda file:
fullpath <- paste(sensitivity$results.path[s], sep = "/",sensitivity$scenariopath[scenario])

saveRDS(object = results.table, file = fullpath)
write.csv(x = results.table, file = fullpath)

#return(results.table)
}
