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

#===
# LOAD DATA
#===

df <- read.csv(file = "data/guthold.csv",stringsAsFactors = FALSE,row.names = 1)%>% 
          mutate(country = recode(country,  "United Kingdom" = 'UK',
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

metmins <- read.csv("data/distributions.csv",row.names = 1,check.names=FALSE)

gen.perc  <- read.csv("data/general_dist.csv",row.names = 1,check.names=FALSE) %>%
                mutate(name = seq(0.01,1,0.01))

# In the example below I use a daily 10 minute increase in walking wordwide.
# This gives a tidy example from which to estimate the benefits.

#===
# CREATE RELATIVE RISKS FOR EACH COUNTRY
#===

# heat methodology - reduction in risk is very small
rr.10walk <- 1 - (1 - 0.89) * (70/168)

# get dose response relationship for mortality, from published paper by Aram et al. 2015

temp <- data.frame(RR = c(1,0.8,0.69,0.63,0.61,0.61),
                   METhwk = c(0,3.75,11.25,18.75,31.25,57.5))%>%
          mutate(METminswk = METhwk*60,
                 METminssq = METminswk^2)

#plot(x = 1:50, 
#     y = predict(loess(formula = RR~METhwk,data = temp),
#                           newdata = data.frame(METhwk=1:50))
#     )
#====
# ESTIMATE RELATIVE RISKS FROM MET-MINS DISTRIBUTION.
#====

rr.metmins <- metmins  ; rr.metmins[,] <- NA    # initialise to same rows etc again.

for(c in colnames(metmins)){
# the approx function estimates where within the dose response function each percentile is, assigns appropriate relative risk. 
  rr.metmins[,c] <- approx(x = temp$METminswk,
                     y=temp$RR,
                     method = "linear",
                     xout = metmins[,c],
                     rule = 2)$y
}

#====
# ESTIMATE EFFECT OF INCREASING MET-MINS BY X AMOUNT ON RELATIVE RISKS.
#====

# increase the MET-mins of the population: in this case by 210, assuming walking a total of 70mins at 3mets.
increase <- 210
rr.metmins.new <- rr.metmins  ; rr.metmins.new[,] <- NA   # initialise matrix

for(c in colnames(rr.metmins.new)){
# use the old distributions plus 210 to estimat the new relative risk functions.  
  rr.metmins.new[,c] <- approx(x = temp$METminswk,
                           y=temp$RR,
                           method = "linear",
                           xout = metmins[,c] + increase,
                           rule = 2)$y
}

#===
#PLOT THIS CHANGE before and after
#===
#
#pdf("figures/RRBeforeAfter.pdf")
#
#plot(rr.metmins$Australia,
#     type = "l",
#     xlab = "Percentile",
#     ylab = "Relative Risk",
#     main = "Relative Risks of Mortality Before & After \n 10min increase in walking, Australia",
#     lty = 1)
#legend(x = 70,y = 1,
#       legend = c("Before","After"),
#       lty = c(1,2),box.lwd = NA,cex=0.7)
#
#lines(rr.metmins.new$Australia,lty = 2)
#
#dev.off() 


#===
# ESTIMATE MORTALITY REDUCTION
#===
#1. Load age standardised overall mortality rates by country.
# problem, rates are linked to ISOcodes.

heat.mort <- read.csv("data/mortality_rates.txt",header = TRUE)[,c(1,2,12,13)] %>%
                spread(key = age_group,value = value_heatdata) %>% 
                rename(country = country_name_heat,
                       ISO_Code = iso3,
                       age2044  = '20-44',
                       age2064  = "20-64",
                       age2074  = '20-74',
                       age4564  = '45-64',
                       age4574  = '45-74') %>%
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
vsl <- read.csv("data/vsl_heat.csv")[,c(1,3)] # read in value of statistical life estimates
colnames(vsl) <- c('ISO_Code','VSL')          # change the column names, just want ISO_Code and VSL
heat.mort <- merge(heat.mort,vsl)             # Merge the mortality rates and VSL measures

merged <- merge(x = df,y = heat.mort)         # Merge githuld data with heat data.
merged$drf <- NA                              # create column for dose response estimate net benefit
merged$lin <- NA                              # create column for linear response estimate net benefit

countries <- intersect(merged$country,colnames(metmins))   # identify countries which can analyse

# 2. Estimate reductions in deaths per year.
for(x in 1:length(countries)){
  temp.country <- countries[x]
  risk.change <- rr.metmins[,temp.country] - rr.metmins.new[,temp.country]
  merged$drf[x] <- mean(risk.change * merged$age2074[merged$country==temp.country])
  merged$lin[x] <- (1-rr.10walk)*merged$age2074[merged$country==temp.country]
}

# the dose response relationship is yielding higher estimates, probably because of the extreme effect on inactive
# people at the start of the dose response function.
merged <- merged %>% rename(IPAP = both)

#===
# MONETARY BENEFIT estimate monetary benefit for nmb and lin responses
#===

merged$nmb_drf <- merged$drf * merged$VSL
merged$nmb_lin <- merged$lin * merged$VSL
merged$relative <- (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin

#===
# STORE RESULTS
#===

results.table <- data.frame(ISO_Code = merged$ISO_Code,
                            country  = merged$country,
                            IPAP     = merged$IPAP,
                            S1_DA_drf= merged$drf,
                            S1_DA_lin= merged$lin,
                            S1_NMB_drf=merged$nmb_drf,
                            S1_NMB_lin=merged$nmb_lin)


#==================================== PLOTS ================================

#=== 
# DEATHS AVERTED PLOT
#===

pdf("figures/relativeresults.pdf")

ggplot(data = merged,
       aes(x=lin,y=drf,col = IPAP))+
  theme_classic()+
  geom_point()+
  geom_abline(slope = 1)+
  annotate(geom="text", x=40, y=40, 
           label="Equality", color="black")+
  labs(title = "Annual death's averted per 100,000: Non-linear relationship vs linear relationship",
       caption = "Data Sources: WHO Mort, GBD Pop, HEAT VSL", 
       x = "linear relationship ", 
       y = "Non-linear relationship")+
  #xlim(0, 150) + ylim(0,150)+
  geom_label_repel(data = as.data.frame(merged), 
                   label = merged$country, 
                   size = 2)#,
                   #nudge_x = 2, nudge_y = -5,
                   #direction = "y",
                   #segment.color = "blue",colour = "blue")+
  #theme(legend.position = c(0.9, 0.2))+
  NULL
dev.off() 

    
#===
# WORLD MAP PLOTS
#===

map.world <- map_data('world')

map <- left_join(map.world, merged, by = c('region' = 'country')) 

# plot 1 net monetary benefit of dose response function
plot1 <- (ggplot(data= map, 
                 aes(x = long, y = lat, group = group)) +
            
            geom_polygon(aes(fill = relative*100),
                         colour="aliceblue",
                         lty=4) +
            
            #scale_fill_continuous(name = "$nmb/pp",low = "white", high = "blue") +
            scale_fill_viridis(discrete=FALSE,name = "% Dif",limits = c(-100,+100)) +
            
            labs(title = "Dose Response Method vs Linear Method", 
                 subtitle = "% difference in NMB between non-linear relationship & linear relationship, Europe (2016)", 
                 caption = "Sources: Nonlinear DRF from Aram et al. 2015, VSL from HEAT") +

            coord_fixed(xlim = c(-20, 70),  
                        ylim = c(30, 70), 
                        ratio = 1.3) +
            
            theme(legend.position = c(0.1,0.2),
                  legend.background = element_blank(),
                  #legend.title = element_blank(),
                  panel.background = element_rect(fill = "aliceblue"),
                  legend.key.width = unit(0.5,"cm"),
                  axis.text = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank())            )

# plot 2 net monetary benefit of linear risk function
plot2 <- (ggplot(data= map, 
                 aes(x = long, y = lat, group = group)) +
            
            geom_polygon(aes(fill = nmb_drf/100000),
                         colour="aliceblue",
                         lty=4) +
            
            #scale_fill_continuous(name = "$nmb/pp",low = "white", high = "blue") +
            scale_fill_viridis(discrete=FALSE,name = "USD (2016)",limits = c(0,1000)) +
            labs(title = "NMB (USD) of 10mins walking using Non-linear Dose Response Function", 
                 subtitle = "Annual Monetary Benefit per person of 10mins of walking per capita, Europe (2016)", 
                 caption = "Sources: DRF from Aram et al. 2015, VSL from HEAT") +
            
            coord_fixed(xlim = c(-20, 70),  
                        ylim = c(30, 70), 
                        ratio = 1.3) +
            
            theme(legend.position = c(0.1,0.2),
                  legend.background = element_blank(),
                  #legend.title = element_blank(),
                  panel.background = element_rect(fill = "aliceblue"),
                  legend.key.width = unit(0.5,"cm"),
                  axis.text = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank())
)


pdf("figures/NMB_IncreasePA.pdf")
print(plot1)     # Plot 1 --> in the first page of PDF
print(plot2)     # Plot 2 --> in the second page of PDF
dev.off() 















