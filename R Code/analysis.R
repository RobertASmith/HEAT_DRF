#===
# setup
#===

rm(list=ls())
library("tidyverse")
library("stringr")
library("pdftools")
library(readxl)
library(reshape2)
library(ggplot2)
library(tidyr)
library("mc2d")
library("ggrepel") 
library(knitr)
library(dplyr)
library(rgeos)
library(rworldmap)
library(flextable)
library("viridis")
library(rlang)     #ensures can read from excel
library(readxl)    # Ensure can read from excel
library(foreign)   # Foreign Package ensures that read.dta works.
library(tidyverse)
library(Rcpp)      # so can read from excel.

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

m.c.dists <- read.csv("data/distributions.csv",row.names = 1,check.names=FALSE)
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

#====
# USE GENERAL DISTRIBUTION TO ASSIGN MET-MINS
#====

# assign these met mins to the percentiles of PA for each country.
metmins <-  m.c.dists  ; metmins[,] <- NA    # same dimensions as m.c.dists but clear data to NA
#plot(metmins[,c])
for(c in colnames(m.c.dists)){
  # for each country, estimate met-mins given percentiles of PA relative to general distribution.
metmins[,c] <- approx(x = m.c.dists[,c],
                     y = gen.perc$value,
                     method = "linear",
                     xout = gen.perc$name,
                     rule = 2)$y
#lines(metmins[,c])
}

#====
# ESTIMATE RELATIVE RISKS FROM MET-MINS DISTRIBUTION.
#====

rr.metmins <-  m.c.dists  ; rr.metmins[,] <- NA    # initialise to same rows etc again.

for(c in colnames(m.c.dists)){
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

for(c in colnames(m.c.dists)){
# use the old distributions plus 210 to estimat the new relative risk functions.  
  rr.metmins.new[,c] <- approx(x = temp$METminswk,
                           y=temp$RR,
                           method = "linear",
                           xout = metmins[,c] + increase,
                           rule = 2)$y
}

#===
#PLOT THIS CHANGE
#===

plot(rr.metmins$Australia,type = "l")
for(c in 1:ncol(m.c.dists)){
  lines(rr.metmins[,c],col=c)
}

plot(rr.metmins$Australia,type = "l") # before
lines(rr.metmins.new$Australia)

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
vsl <- read.csv("data/vsl_heat.csv")[,c(1,3)]
colnames(vsl) <- c('ISO_Code','VSL')
heat.mort <- merge(heat.mort,vsl)

merged <- merge(x = df,y = heat.mort) # new dataframe with added data.
merged$drf <- NA
merged$lin <- NA

countries <- intersect(merged$country,colnames(m.c.dists))
#2. Estimate reductions in deaths per year.
#3. Value according the VSL methodology used by HEAT.


for(x in 1:length(countries)){
  temp.country <- countries[x]
  risk.change <- rr.metmins[,temp.country] - rr.metmins.new[,temp.country]
  merged$drf[x] <- mean(risk.change * merged$age2074[merged$country==temp.country])
  merged$lin[x] <- (1-rr.10walk)*merged$age2074[merged$country==temp.country]
}

# the dose response relationship is yielding higher estimates, probably because of the extreme effect on inactive
# people at the start of the dose response function.
hist(merged$drf/merged$lin) 

# I can then use the VSL from HEAT to estimate monetary benefit of this.
merged$nmb <- merged$drf * merged$VSL
       
#===
# PLOTS
#===

#===
# PLOT IN GGPLOT THE INCREASE.
#===
map.world <- map_data('world')

map <- left_join(map.world, merged, by = c('region' = 'country')) 

plot1 <- (ggplot(data= map, 
                 aes(x = long, y = lat, group = group)) +
            
            geom_polygon(aes(fill = nmb/100000),
                         colour="aliceblue",
                         lty=4) +
            
            scale_fill_continuous(name = "$nmb/pp",low = "white", high = "blue") +
            
            labs(title = "Monetary Benefit of 10mins of walking, Europe (2016)", 
                 subtitle = "Assuming additional 10mins per day per person", 
                 caption = "Sources: DRF from Aram et al. 2015, VSL from HEAT") +

            coord_fixed(xlim = c(-20, 70),  
                        ylim = c(30, 70), 
                        ratio = 1.3) +
            
            theme(legend.position = c(0.1,0.2),
                  legend.background = element_rect(fill = "aliceblue"),
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

pdf("figures/da.per100k.drf.pdf")
print(plot1)     # Plot 1 --> in the first page of PDF
dev.off() 






#===
# ARCHIVE
#===




# increase in MET-mins.
increase <- 210      # based on 10 mins extra walking per day.
walk.metmins <- 1500 # we assume the heat reference met-mins is 1500.

# initial matrix of relative risk, capped at 0.7
rr.matrix.init     <- 1 - (1 - 0.89) * (m.c.dists/walk.metmins)
rr.matrix.init[rr.matrix.init <0.7]     <- 0.7

# new matrix of relativ risk, capped at 0.7
m.c.dists.new      <- m.c.dists + increase
rr.matrix.new      <- 1 - (1 - 0.89) * (country.matrix.new/walk.metmins)
rr.matrix.new[rr.matrix.new <0.7]     <- 0.7

# plot new populations
plot(rr.matrix.init[,10],type = "l")
lines(rr.matrix.new[,10],col="blue")

# using single pop mortality
mort.rate <- 550 / 100000

# multiply this by mortality rate to estimate risks for each.
risk.init <- rr.matrix.init * mort.rate
risk.new  <- rr.matrix.new * mort.rate

# estimate the number of deaths averted per 100,000 per year.
da.hundk  <- (risk.init - risk.new) * 100000

df$saved  <-  colMeans(da.hundk)

#===
# PLOT IN GGPLOT THE INCREASE.
#===
map.vsly <- left_join(map.world, df, by = c('region' = 'country')) 

plot1 <- (ggplot(data= map.vsly, 
                 aes(x = long, y = lat, group = group)) +
            
            geom_polygon(aes(fill = saved),
                         colour="aliceblue",
                         lty=4) +
            
            scale_fill_continuous(name = "Deaths Averted") +
            
            labs(title = "Deaths averted by walking intervention", 
                 subtitle = "Per 100,000 people, assuming additional 10mins per day per person.", 
                 caption = "Sources: DRF from Aram et al. 2015, VSL from HEAT") +
            
            ditch_the_axes +
            
            theme(legend.position = c(0.1,0.2),
                  legend.background = element_rect(fill = "aliceblue"),
                  #legend.title = element_blank(),
                  panel.background = element_rect(fill = "aliceblue"),
                  legend.key.width = unit(0.5,"cm"))
)

pdf("figures/livessaved.pdf")
print(plot1)     # Plot 1 --> in the first page of PDF
dev.off() 


#===
# ARCHIVE
#===

# plot the old density and new density
plot(density(init.pop))
lines(density(new.pop), col = "red")

lm.rr <- lm(formula = RR ~ METminswk + METminssq ,data = temp)

# plot linear model results
fake <- data.frame(METminswk = seq(1,1800,100),
                   METminssq = seq(1,1800,100)^2)
fake$rr <- predict(object = lm.rr,
                   newdata = fake)

plot(temp$METminswk,temp$RR)
lines(x=fake$METminswk,y = fake$rr )



plot(y = country.matrix[,"UK"], x=1:100,type= "l")
lines(y = country.matrix[,"Ukraine"], x=1:100,col="red")


plot(density(country.matrix[,"UK"]))
lines(density(country.matrix[,"Ukraine"]),col="red")
lines(density(country.matrix[,"Vietnam"]),col="blue")
lines(density(country.matrix[,"Niger"]),col="green")

Niger

