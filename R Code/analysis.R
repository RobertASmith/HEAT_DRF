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
library(ggplot2)  # GGplot Package
library(tidyverse)
library(dplyr)
library(Rcpp)      # so can read from excel.

df <- read.csv(file = "data/guthold.csv")
m.c.dists <- read.csv("data/distributions.csv",row.names = 1)

# In the example below I use a daily 10 minute increase in walking wordwide.
# This gives a tidy example from which to estimate the benefits.

#===
# CREATE RELATIVE RISKS FOR EACH COUNTRY
#===

# get dose response relationship for mortality, from published paper by Aram et al. 2015

temp <- data.frame(RR = c(1,0.8,0.69,0.63,0.61,0.61),
                   METhwk = c(0,3.75,11.25,18.75,31.25,57.5))%>%
          mutate(METminswk = METhwk*60,
                 METminssq = METminswk^2)

# assign these met mins to the percentiles of PA for each country.
rr.metmins <- m.c.dists

for(c in colnames(m.c.dists)){

  rr.metmins[,c] <- approx(x = temp$METminswk,
                     y=temp$RR,
                     method = "linear",
                     xout = m.c.dists[,c],
                     rule = 2)$y
}

# increase the MET-mins of the population:
increase <- 210

rr.metmins.new <- rr.metmins

for(c in colnames(m.c.dists)){
  
  rr.metmins.new[,c] <- approx(x = temp$METminswk,
                           y=temp$RR,
                           method = "linear",
                           xout = m.c.dists[,c] + increase,
                           rule = 2)$y
}

#===
# plots showing this
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
#2. Estimate reductions in deaths per year.
#3. Value according the VSL methodology used by HEAT.




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
                 caption = "Sources: Various") +
            
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

