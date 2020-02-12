#===
# ANALYSIS SCENARIO 3
#===
f_scenario_3 <- function(a = 0.89, b = 168, t = t,  
                         metmins = metmins, merged = merged, countries = countries,
                         results.path = sensitivity$results.path[s]){
  
  # t is the power transformation, metmins the orginal metmins, 
  # a is the reference relative risk
  # b is the reference physical activity level in mets
  
  # =========== #
  # NON -LINEAR #
  # =========== #
  
# Estimate BEFORE scenario relative risks
rr.metmins <-   a^(metmins/b*3)^t

# Change METmins according to scenario
metmins.new <- metmins*1.1  # create a new set of met-mins 10% higher

# estimate AFTER scenario relative risks
rr.metmins.new <-   a^(metmins.new/b*3)^t

# calculate risk change in non-linear model 
risk.change <- rr.metmins[,countries] - rr.metmins.new[,countries]

# multiply risk change for each percentile by mortality risk and take mean
merged$drf <- colMeans(risk.change * merged$mortrisk[match(merged$country,countries)])

  # ====== #
  # LINEAR #
  # ====== #

# estimate change in minutes of walking in linear model
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3  # calculating increase associated with intervention

# estimate linear function risk change.
rr.S1.ldrf <- 1 - (1 - 0.89) * (increase.walk/168) 

# calculate reductions in deaths per 100,000 in linear model.
merged$lin <- (1-rr.S1.ldrf)* merged$mortrisk[match(merged$country,countries)]

#===
# MONETARY BENEFIT estimate monetary benefit for nmb and lin responses
#===

# multiply deaths averted by vsl, for non-linear & linear models
merged$nmb_drf  <- merged$drf * merged$VSL # non-linear
merged$nmb_lin  <- merged$lin * merged$VSL # linear
merged$relative <- (merged$nmb_drf - merged$nmb_lin)/ merged$nmb_lin # calculate dif relative to linear

# store results
results.table <- data.frame(ISO_Code = merged$ISO_Code,
                            country  = merged$country,
                            IPAP     = merged$IPAP,
                            S3_DA_drf= merged$drf,
                            S3_DA_lin= merged$lin,
                            S3_NMB_drf= merged$drf * merged$VSL/100000,
                            S3_NMB_lin= merged$lin * merged$VSL/100000,
                            S3_NMB_relative = (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin*100)

# save results as rda file:
saveRDS(object = results.table, file = paste(results.path,sep = "/","S3results.Rda"))
write.csv(x = results.table,    file = paste(paste(results.path,sep = "/","S3results.csv")))

#return(results.table)

}

