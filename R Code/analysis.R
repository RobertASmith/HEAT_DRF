#===
# SETUP
#===

rm(list=ls())

library(tidyverse)
library(stringr)
library(pdftools)
library(reshape2)
library(ggplot2)
library(tidyr)
library(mc2d)
library(ggrepel) 
library(knitr)
library(dplyr)
library(rgeos)
library(rworldmap)
library(flextable)
library(viridis)
library(rlang)     #ensures can read from excel
library(readxl)    # Ensure can read from excel
library(foreign)   # Foreign Package ensures that read.dta works.
library(Rcpp)      # so can read from excel.

source(file = "R Code/plotfunctions.R")
#===
# LOAD & CLEAN DATA
#===

# Guthold et al. 2014 data on IPAP
df <- read.csv(file = "data/guthold.csv",
               stringsAsFactors = FALSE,
               row.names = 1)%>% 
            select(country,both) %>% 
            rename(IPAP = both)

# PA distributions for each country
metmins <- read.csv("data/distributions.csv",
                    row.names = 1,
                    check.names=FALSE, 
                    stringsAsFactors = F)

# general distribution (UK)
gen.perc  <- read.csv("data/general_dist.csv",
                      row.names = 1,
                      check.names=FALSE) %>%
                mutate(name = seq(0.01,1,0.01)) %>% 
                rename(pcnt = name)

# mortality rates
heat.mort <- read.csv("data/mortality_rates.txt",
                      header = TRUE,
                      stringsAsFactors = F)[,c(1,2,12,13)] %>%
  filter(age_group == "20-74") %>% 
  spread(key = age_group,value = value_heatdata) %>% 
  rename(country = country_name_heat,
         ISO_Code = iso3,
         #age2044  = '20-44',
         #age2064  = "20-64",
         mortrisk  = '20-74') %>%
         #age4564  = '45-64',
         #age4574  = '45-74')
  mutate(country = recode(country,  
                          'United Kingdom' = 'UK',
                          "Russian Federation" = "Russia",
                          "United States of America" = "USA",
                          "United Republic of Tanzania" = "Tanzania",
                          "Venezuela (Bolivarian Republic of)" = "Venezuela",
                          "Republic of Moldova" = "Moldova",
                          "Viet Nam" = "Vietnam",
                          "Republic of Korea" = "South Korea",
                          "Congo" = "Democratic Republic of the Congo",
                          "Cote d'Ivoire"= "Ivory Coast",
                          "Iran (Islamic Republic of)" = "Iran",
                          "Trinidad and Tobago" = "Trinidad"))

# add in HEAT VSL measure
vsl <- read.csv(file = "data/vsl_heat.csv",
                stringsAsFactors = F,
                col.names = c('ISO_Code','Country2','VSL','ISO_heat','modified')) %>% 
                select (ISO_Code,VSL) # read in value of statistical life estimates

# add in relative risks from xxxx et al. 

# Merge these datasets
merged <- merge(x = heat.mort,y = vsl)        # Merge the mortality rates and VSL measures
merged <- merge(x= merged, y = df)            # Merge the df too.

# constrain the metmins data to HEAT countries
metmins <- metmins[,intersect(merged$country,colnames(metmins))]

# remove unnecessary data
rm(df,gen.perc,heat.mort,vsl)                 # remove unnecessary data now merged.


#===
# CREATE DOSE/LINEAR RELATIVE RISKS 
#===

# get dose response relationship for mortality, from published paper by Aram et al. 2015

lit.rr <- data.frame(A.METhwk = c(0,3.75,11.25,18.75,31.25,57.5),
                     A.rr = c(1,0.8,0.69,0.63,0.61,0.61),
                     K.METhwk.wlk = c(0,8,22.5,50,NA,NA),
                     K.rr.wlk = c(1,0.91,0.88,0.80,NA,NA),
                     K.METhwk.cyc = c(0,11.5,32,65,NA,NA),
                     K.rr.cyc = c(1,0.83,0.76,0.70,NA,NA)  ) %>% 
  mutate(A.METmwk = A.METhwk*60,
         K.METmwk.wlk = K.METhwk.wlk*60,
         K.METmwk.cyc = K.METhwk.cyc*60)



#==================== ANALYSIS SCENARIO 1 ==========================

#===
# ESTIMATE RELATIVE RISKS BEFORE SCENARIO.
#===

rr.metmins <- metmins  ; rr.metmins[,] <- NA    # initialise to same rows etc again.

for(c in colnames(metmins)){
# the approx function estimates where within the dose response function each percentile is, assigns appropriate relative risk. 
  rr.metmins[,c] <- approx(x = lit.rr$A.METmwk,
                     y=lit.rr$A.rr,
                     method = "linear",
                     xout = metmins[,c],
                     rule = 2)$y
}

#===
# ESTIMATE NEW RELATIVE RISKS AFTER SCENARIO 
#===

metmins.new <- metmins + 210                                  # increase of 210 METs
rr.metmins.new <- rr.metmins  ; rr.metmins.new[,] <- NA   # initialise matrix

for(c in colnames(rr.metmins.new)){
# use the old distributions plus 210 to estimat the new relative risk functions.  
  rr.metmins.new[,c] <- approx(x = lit.rr$A.METmwk,
                           y = lit.rr$A.rr,
                           method = "linear",
                           xout = metmins.new[,c],
                           rule = 2)$y
}

#===
# ESTIMATE MORTALITY REDUCTION
#===

# heat methodology - reduction in risk is very small
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3
rr.S1.ldrf <- 1 - (1 - 0.89) * (increase.walk/168)  # scenario 1 is 10 mins walking per person



# 1. for a list of countries.
countries <- intersect(merged$country,colnames(metmins))   # identify countries which can analyse
merged$drf <- NA                              # create column for dose response estimate net benefit
merged$lin <- NA                              # create column for linear response estimate net benefit

# 2. Estimate reductions in deaths per year.
for(x in 1:length(countries)){
  temp.country <- countries[x]
  risk.change <- rr.metmins[,temp.country] - rr.metmins.new[,temp.country]
  merged$drf[x] <- mean(risk.change * merged$mortrisk[merged$country==temp.country])
  merged$lin[x] <- (1-rr.S1.ldrf)*merged$mortrisk[merged$country==temp.country]
}

#===
# MONETARY BENEFIT estimate monetary benefit for nmb and lin responses
#===

merged$nmb_drf  <- merged$drf * merged$VSL
merged$nmb_lin  <- merged$lin * merged$VSL
merged$relative <- (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin

#===
# STORE RESULTS
#===

results.table <- data.frame(ISO_Code = merged$ISO_Code,
                            country  = merged$country,
                            IPAP     = merged$IPAP,
                            S1_DA_drf= merged$drf,
                            S1_DA_lin= merged$lin,
                            S1_NMB_drf=merged$drf * merged$VSL/100000,
                            S1_NMB_lin=merged$lin * merged$VSL/100000,
                            S1_NMB_relative = (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin*100)





#==================== PLOTS SCENARIO 1 =============

pdf("figures/S1_RelativeResults.pdf")
f.deathsavertedplot(x = "S1_DA_lin", y = "S1_DA_drf" , col = "IPAP"  )
dev.off() 

pdf("figures/S1_MapRelative.pdf")
f.maprelative(relative = "S1_NMB_relative")
dev.off() 

pdf("figures/S1_DoseResponse.pdf")
f.mapdoseresponse(doseresponse = "S1_NMB_drf")
dev.off() 


#==================== ANALYSIS SCENARIO 2 ==================

#===
# ESTIMATE RELATIVE RISKS FROM MET-MINS DISTRIBUTION FOR EACH COUNTRY.
#===

rr.metmins <- metmins  ; rr.metmins[,] <- NA    # initialise to same rows etc again.

for(c in colnames(metmins)){
  # the approx function estimates where within the dose response function each percentile is, assigns appropriate relative risk. 
  rr.metmins[,c] <- approx(x = lit.rr$A.METmwk,
                           y=lit.rr$A.rr,
                           method = "linear",
                           xout = metmins[,c],
                           rule = 2)$y
}


#===
# ESTIMATE EFFECT OF INCREASING MET-MINS BY X AMOUNT ON RELATIVE RISKS.
#===

metmins.new <- metmins  # create a new set of met-mins

metmins.new[metmins<600] <- 600 # Change metmins of those lower than 600 mets to 600.

rr.metmins.new <- rr.metmins  ; rr.metmins.new[,] <- NA   # initialise matrix

for(c in colnames(rr.metmins.new)){
  # use the old distributions plus 210 to estimat the new relative risk functions.  
  rr.metmins.new[,c] <- approx(x = lit.rr$A.METmwk,
                               y = lit.rr$A.rr,
                               method = "linear",
                               xout = metmins.new[,c],
                               rule = 2)$y
}

# heat methodology - reduction in risk is very small
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3
rr.S2.ldrf <- 1 - (1 - 0.89) * (increase.walk/168)  # scenario 2 is everyone meeting targets


#===
# ESTIMATE MORTALITY REDUCTION
#===

# 1. for a list of countries.
# countries <- intersect(merged$country,colnames(metmins))   # identify countries which can analyse
 merged$drf <- NA                              # create column for dose response estimate net benefit
 merged$lin <- NA                              # create column for linear response estimate net benefit


# 2. Estimate reductions in deaths per year.
for(x in 1:length(countries)){
  temp.country <- countries[x]
  risk.change <- rr.metmins[,temp.country] - rr.metmins.new[,temp.country]
  merged$drf[x] <- mean(risk.change * merged$mortrisk[merged$country==temp.country])
  merged$lin[x] <- (1-rr.S2.ldrf)*merged$mortrisk[merged$country==temp.country]
}

#===
# MONETARY BENEFIT estimate monetary benefit for nmb and lin responses
#===

merged$nmb_drf  <- merged$drf * merged$VSL
merged$nmb_lin  <- merged$lin * merged$VSL
merged$relative <- (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin

#===
# STORE RESULTS
#===

results.table$S2_DA_drf  = merged$drf
results.table$S2_DA_lin  = merged$lin
results.table$S2_NMB_drf = merged$drf * merged$VSL
results.table$S2_NMB_lin = merged$lin * merged$VSL
results.table$S2_NMB_relative = (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin

#================= PLOTS SCENARIO 2  ==============

pdf("figures/S2_RelativeResults.pdf")

f.deathsavertedplot(x = "S2_DA_lin", y = "S2_DA_drf" , col = "IPAP"  )

dev.off() 


#==================== ANALYSIS SCENARIO 3 ====================

# 10% increase in physical activity for the whole population

#===
# ESTIMATE RELATIVE RISKS FROM MET-MINS DISTRIBUTION FOR EACH COUNTRY.
#===

rr.metmins <- metmins  ; rr.metmins[,] <- NA    # initialise to same rows etc again.

for(c in colnames(metmins)){
  # the approx function estimates where within the dose response function each percentile is, assigns appropriate relative risk. 
  rr.metmins[,c] <- approx(x = lit.rr$A.METmwk,
                           y=lit.rr$A.rr,
                           method = "linear",
                           xout = metmins[,c],
                           rule = 2)$y
}


#===
# ESTIMATE EFFECT OF INCREASING MET-MINS BY X AMOUNT ON RELATIVE RISKS.
#===

metmins.new <- metmins*1.1  # create a new set of met-mins 10% higher


rr.metmins.new <- rr.metmins  ; rr.metmins.new[,] <- NA   # initialise matrix

for(c in colnames(rr.metmins.new)){
  # use the old distributions plus 210 to estimat the new relative risk functions.  
  rr.metmins.new[,c] <- approx(x = lit.rr$A.METmwk,
                               y = lit.rr$A.rr,
                               method = "linear",
                               xout = metmins.new[,c],
                               rule = 2)$y
}

# heat methodology - reduction in risk is very small
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3
rr.S2.ldrf <- 1 - (1 - 0.89) * (increase.walk/168)  # scenario 3 is 10% higher activity per person


#===
# ESTIMATE MORTALITY REDUCTION
#===

# 1. for a list of countries.
# countries <- intersect(merged$country,colnames(metmins))   # identify countries which can analyse
merged$drf <- NA                              # create column for dose response estimate net benefit
merged$lin <- NA                              # create column for linear response estimate net benefit


# 2. Estimate reductions in deaths per year.
for(x in 1:length(countries)){
  temp.country <- countries[x]
  risk.change <- rr.metmins[,temp.country] - rr.metmins.new[,temp.country]
  merged$drf[x] <- mean(risk.change * merged$mortrisk[merged$country==temp.country])
  merged$lin[x] <- (1-rr.S2.ldrf)*merged$mortrisk[merged$country==temp.country]
}

#===
# MONETARY BENEFIT estimate monetary benefit for nmb and lin responses
#===

merged$nmb_drf  <- merged$drf * merged$VSL
merged$nmb_lin  <- merged$lin * merged$VSL
merged$relative <- (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin

#===
# STORE RESULTS
#===

results.table$S3_DA_drf  = merged$drf
results.table$S3_DA_lin  = merged$lin
results.table$S3_NMB_drf = merged$drf * merged$VSL
results.table$S3_NMB_lin = merged$lin * merged$VSL
results.table$S3_NMB_relative = (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin


#================= PLOTS SCENARIO 2  ==============

pdf("figures/S3_RelativeResults.pdf")

f.deathsavertedplot(x = "S3_DA_lin", y = "S3_DA_drf" , col = "IPAP"  )

dev.off() 

