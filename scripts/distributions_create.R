# ====== #
# Author:   Robert Smith
# Contact:  rasmith3@sheffield.ac.uk
# Project: HEAT Dose Response Function
# Description: This script uses the method described in Hafner et al. 2019 to create distributions of physical activity.
#              It relies on data from HSE 2017 to create a generic distribution which is combined with information on prevalence of 
#               insufficient physical activity to create distributions for each country. .  
# ====== #


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
library(dplyr)
library(Rcpp)      # so can read from excel.
library("reactable")
library("gplots")

#===
# MATCHING DATA TO MAP DATA
#===

df <- read.csv(file = "data/guthold.csv",row.names = 1)

centroids <- getMap(resolution="low")        # map data.
centroids <- as.data.frame(gCentroid(centroids, byid=TRUE)) # generate centroids data frame
centroids$country <- rownames(centroids)

map.world <- map_data('world')
#map.world$region %>% unique
anti_join(df, map.world, by = c('country' = 'region'))

df <- df %>% mutate(country = recode(country,  
                                     'United Kingdom' = 'UK',
                                     "Russian Federation" = "Russia",
                                     "United States of America" = "USA",
                                     "United Republic of Tanzania" = "Tanzania",
                                     "Venezuela (Bolivarian Republic of)" = "Venezuela",
                                     "Republic of Moldova" = "Moldova",
                                     "Viet Nam" = "Vietnam",
                                     "Republic of Korea" = "South Korea",
                                     #"Congo" = "Democratic Republic of the Congo",
                                     "Cote d'Ivoire"= "Ivory Coast",
                                     "Iran (Islamic Republic of)" = "Iran",
                                     "Trinidad and Tobago" = "Trinidad")) %>% 
              
              filter(country != "Democratic Republic of the Congo")


map.vsly <- left_join(map.world, df, by = c('region' = 'country')) 


#===
# Creatimg chloropleth map of the world
#===

ggsave(filename = "output/IPAP.pdf",
         plot = (ggplot(data= map.vsly, 
                 aes(x = long, y = lat, group = group)) +
            
            geom_polygon(aes(fill = 1-both),
                         colour="aliceblue",
                         lty=4) +
            
            scale_fill_continuous(name = "% inactive") +
            
            labs(title = "Country prevalence of insufficient physical activity", 
                 subtitle = "Age standardised to the WHO Standard Population, 2016.", 
                 caption = "Source: Guthold R et al. (2018) Appendix 5") +
            
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
    ))


#===
# Creating generic distribution, from HSE
#===

mets <- c(0,4,8) 

# read in data, select the necessary variables and filter the dataset to those who answered questions, create/calc metmins variable.
hsedata <- read.dta("C:/Users/Robert/Google Drive/Model/Data/Raw/HSE15/hse2015ai.dta", convert.factors=FALSE)  %>%  # read in data
  dplyr::select(Sex,ag16g10,TotmWalD,MPAmWk,VPAmWk,MVPAmWk,wt_int) %>%             # select PA data
  dplyr::filter(MVPAmWk >= 0,                                                      # filter dataset to those who answered questions.
                TotmWalD >= 0,                                                     #  again, those answering questions.
                ag16g10 >= 0 & ag16g10 < 7) %>%                                    # limit to those aged 16-74
  dplyr::mutate(id = 1:nrow(.),                                                    # create ID variables.
                Sex = Sex-1,
                metmins = TotmWalD*mets[1] + MPAmWk*mets[2] + VPAmWk*mets[3], # calculate metmins using: mins * mets for each activity 
                cons_metmins = ifelse(metmins>10000, 10000, metmins)) # constrain to 10,000

#ggplot(hsedata,aes(metmins,fill = "red")) +
#geom_density(adjust = 1.5) +
#  xlim(0,10000) + 
#  theme_classic()+
#  labs(title = "Generic Physical Activity Distribution",
#       subtitle = "",
#       caption = "Data from the Health Survey for England 2015",
#       x = "Weekly MET-mins",
#       y = "Density") +
#  guides(fill=FALSE)


# Create percentiles for English (Generic) Distribution
gen.perc <- hsedata %>% 
  summarise(list(enframe(quantile(cons_metmins, probs= c(seq(0.01,1,0.01)))))) %>%  # create quintiles for each group
  unnest %>% mutate(name = seq(0.01,1,0.01)) 

#ggplot(gen.perc)+
#  geom_line(aes(x = name*100, y = value))+
#  labs(title = "Physical Activity by Percentile of Distribution",
#       caption = "Source: HSE 2015",
#       x = "Percentile of population",
#       y = "Weekly MET-mins")+
#  theme_classic()



#===
# Create physical activity distribution every country.
#===

country.matrix <- matrix(data = NA,
                         ncol = length(df$country),
                         nrow = 100, 
                         dimnames = list(c(1:100),df$country) )
country.matrix[1,] <- 0

for(c in 1:ncol(country.matrix)){ # country loop
  
  country <- paste(df$country[c])
  for(x in 2:100){ 
    # percentile loop
    country.matrix[x,country] <- 0.01 * df$both[df$country == country]/df$both[df$country == "UK"] + country.matrix[x-1,country]
  } # percentile loop
    } # country loop

#====
# USE GENERAL DISTRIBUTION TO ASSIGN MET-MINS
#====

# assign these met mins to the percentiles of PA for each country.
metmins <-  country.matrix  ; metmins[,] <- NA    # same dimensions as m.c.dists but clear data to NA
#plot(metmins[,c])
for(c in colnames(country.matrix)){
  # for each country, estimate met-mins given percentiles of PA relative to general distribution.
  metmins[,c] <- approx(x = country.matrix[,c],
                        y = gen.perc$value,
                        method = "linear",
                        xout = as.numeric(gen.perc$name),
                        rule = 2)$y
  #lines(metmins[,c])
}


write.csv(x = metmins,file = "data/distributions.csv")
write.csv(x = gen.perc, file = "data/general_dist.csv")

# TESTING THIS TO ENSURE CORRECT

#plot physical activity percentiles
metmins_long <- melt(metmins,
                     varnames = c("percentile","country"),
                     value.name = "metmins")

ggsave(plot = 
         
         ggplot(metmins_long %>% filter(country %in% c("UK",
                                                       "France",
                                                       "Portugal",
                                                       "Ukraine",
                                                       "Germany")))+
        geom_line(aes(x = percentile, y = metmins,col = country))+
        theme_classic()+
        labs(title = "Physical Activity in 5 HEAT countries",
             subtitle = "Using method developed in Hafner et al. (2019)",
             caption = "Sources: HSE 2015, Guthold et al. 2018",
             x = "Percentile",
             y = "Weekly MET-mins") +
        guides(fill=FALSE),
       
       filename = "output/countrydistributions.pdf",
       
       width = 5, height = 7)


