#===
# SETUP
#===
#setwd("C:/Users/Robert/Google Drive/Other Projects/HEAT2/HEAT_DRF")

rm(list=ls())
#install.packages("kableExtra")
library(tidyverse)
library(stringr)
library(pdftools)
library(reshape2)
library(ggplot2)
library(tidyr)
library(mc2d)
library(ggrepel) 
library(knitr)
library(xtable)
library(gridExtra)
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
source(file = "R Code/modelfunctions.R")
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

#=== INITIALISE

countries <- intersect(merged$country,colnames(metmins))   # identify countries which can analyse
merged$drf <- NA                              # create column for dose response estimate net benefit
merged$lin <- NA                              # create column for linear response estimate net benefit


#==================== ANALYSIS SCENARIO 1 ==========================

# Estimate BEFORE scenario relative risks
rr.metmins <- f.getRR(mets = metmins)

# Change METmins according to scenario
metmins.new <- metmins + 210

# estimate AFTER scenario relative risks
rr.metmins.new <- f.getRR(mets = metmins.new)

# estimate change in relative risk in linear model
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3
rr.S1.ldrf <- 1 - (1 - 0.89) * (increase.walk/168)  # scenario 1 is 10 mins walking per person

# estimate reductions in deaths per year.
results <- f.getresults(risk.change.lin = rr.S1.ldrf, df = merged, v.countries = countries)

# store results

results.table <- data.frame(ISO_Code = merged$ISO_Code,
                            country  = merged$country,
                            IPAP     = merged$IPAP,
                            S1_DA_drf= results$drf,
                            S1_DA_lin= results$lin,
                            S1_NMB_drf= results$drf * merged$VSL/100000,
                            S1_NMB_lin= results$lin * merged$VSL/100000,
                            S1_NMB_relative = (results$nmb_drf-results$nmb_lin)/results$nmb_lin*100)


#==================== ANALYSIS SCENARIO 2 ==================

# Estimate BEFORE scenario relative risks
rr.metmins <- f.getRR(mets = metmins)

# Change METmins according to scenario
metmins.new <- metmins  # create a new set of met-mins equal to old ones
metmins.new[metmins<600] <- 600 # Change metmins of those lower than 600 mets to 600.

# estimate AFTER scenario relative risks
rr.metmins.new <- f.getRR(mets = metmins.new)

# estimate change in relative risk in linear model
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3
rr.S2.ldrf <- 1 - (1 - 0.89) * (increase.walk/168)  # scenario 1 is 10 mins walking per person

# estimate reductions in deaths per year.
results <- f.getresults(risk.change.lin = rr.S2.ldrf, df = merged, v.countries = countries)

# store results

results.table$S2_DA_drf       = results$drf
results.table$S2_DA_lin       = results$lin
results.table$S2_NMB_drf      = results$drf * merged$VSL/100000
results.table$S2_NMB_lin      = results$lin * merged$VSL/100000
results.table$S2_NMB_relative = (results$nmb_drf-results$nmb_lin)/results$nmb_lin*100

#==================== ANALYSIS SCENARIO 3 ====================

# 10% increase in physical activity for the whole population

# Estimate BEFORE scenario relative risks
rr.metmins <- f.getRR(mets = metmins)

# Change METmins according to scenario
metmins.new <- metmins*1.1  # create a new set of met-mins 10% higher

# estimate AFTER scenario relative risks
rr.metmins.new <- f.getRR(mets = metmins.new)

# estimate change in relative risk in linear model
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3
rr.S2.ldrf <- 1 - (1 - 0.89) * (increase.walk/168)  # scenario 1 is 10 mins walking per person

# estimate reductions in deaths per year.
results <- f.getresults(risk.change.lin = rr.S2.ldrf, df = merged, v.countries = countries)

# store results

results.table$S3_DA_drf       = results$drf
results.table$S3_DA_lin       = results$lin
results.table$S3_NMB_drf      = results$drf * merged$VSL/100000
results.table$S3_NMB_lin      = results$lin * merged$VSL/100000
results.table$S3_NMB_relative = (results$nmb_drf-results$nmb_lin)/results$nmb_lin*100

saveRDS(object = results.table,file = "data/results.R") # store results table as R file.

#==================== SAVE TABLE AS PDF ============

pdf("figures/ResultsTable.pdf",width = 30,height = 20,title = "Results Table")
grid.table(results.table)
dev.off()

#==================== PLOTS SCENARIO 1 =============

pdf("figures/S1_RelativeResults.pdf")
plotS1A <- f.deathsavertedplot(x = "S1_DA_lin", 
                    y = "S1_DA_drf" , 
                    col = "IPAP",
                    title = "Scenario 1: Additional 10 mins daily walking")
print(plotS1A)
dev.off() 

pdf("figures/S1_MapRelative.pdf")
f.maprelative(relative = "S1_NMB_relative",
              title = "Scenario 1: Additional 10 mins daily walking",
              limits = c(-100,100))
dev.off() 

pdf("figures/S1_DoseResponse.pdf")
f.mapdoseresponse(doseresponse = "S1_NMB_drf",
                  title = "Scenario 1: Additional 10 mins daily walking",
                  limits = c(0,1000))
dev.off() 

#==================== PLOTS SCENARIO 2 =============

# everyone meets WHO guidelines of 600 MET-mins/week

pdf("figures/S2_RelativeResults.pdf")
plotS2A <- f.deathsavertedplot(x = "S2_DA_lin", 
                    y = "S2_DA_drf" , 
                    col = "IPAP",
                    title = "Scenario 2: Every person meets WHO Guidelines")
print(plotS2A)
dev.off() 

pdf("figures/S2_MapRelative.pdf")
f.maprelative(relative = "S2_NMB_relative", 
              title = "Scenario 2: Every person meets WHO Guidelines", 
              limits = c(-200,200))
dev.off() 

pdf("figures/S2_DoseResponse.pdf")
f.mapdoseresponse(doseresponse = "S2_NMB_drf",
                  title = "Scenario 2: Every person meets WHO Guidelines",
                  limits = c(0,1500))
dev.off() 

#==================== PLOTS SCENARIO 3 =============
# 10% rise in physical activity for everyone

pdf("figures/S3_RelativeResults.pdf")
plotS3A <- f.deathsavertedplot(x = "S3_DA_lin", 
                    y = "S3_DA_drf" , 
                    col = "IPAP", 
                    title = "Scenario 3: 10% Increase in PA for every person" )
print(plotS3A)
dev.off() 

pdf("figures/S3_MapRelative.pdf")
f.maprelative(relative = "S3_NMB_relative",
              title = "Scenario 3: 10% Increase in PA for every person",
              limits = c(-200,200))
dev.off() 

pdf("figures/S3_DoseResponse.pdf")
f.mapdoseresponse(doseresponse = "S3_NMB_drf",
                  title = "Scenario 3: 10% Increase in PA for every person",
                  limits = c(0,1000))
dev.off() 

#=================== COMBINED PLOT =======================

pdf("figures/RelativeAll.pdf",height = 12,width = 8)
grid.arrange(plotS1A,plotS2A,plotS3A,nrow=3,newpage = T)
dev.off() 
