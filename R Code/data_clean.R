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


#  Guthold R, Stevens GA, Riley LM, Bull FC. Worldwide trends in insufficient physical activity from 2001 to 2016: a 
#  pooled analysis of 358 population-based surveys with 1·9 million participants. Lancet Glob Health 2018

#===
# EXTRACTING TABLE FROM PDF.
#===

pdf_file <- file.path("data/country_prevs.pdf")

text <- pdf_text(pdf_file)

tab <- str_split(text, "\n")[29:32]

#--- go through tabs
temp1 <- tab[[1]][6:53] %>% 
              strsplit( "\\s{2,}")%>% 
              unlist() 

temp1 <- matrix(data = temp1, 
                nrow=length(temp1)/4,
                ncol=4,
                byrow = TRUE)

for(c in 2:4){# column loop
temp2 <- temp1[,c] %>% strsplit("% ") %>% unlist() 
temp1[,c] <- temp2[seq(1,length(temp2),2)] 
}

# change to dataframe.
df <- as.data.frame(temp1,stringsAsFactors=FALSE)


#--- page 2
temp1 <- tab[[2]][4:54] %>% 
              strsplit( "\\s{2,}")%>% 
              unlist() %>%
              matrix(nrow=204/4,ncol=4,byrow = TRUE)

for(c in 2:4){# column loop
  temp2 <- temp1[,c] %>% strsplit("% ") %>% unlist() 
  temp1[,c] <- temp2[seq(1,length(temp2),2)] 
}

# change to dataframe.
df <- rbind(df,as.data.frame(temp1,stringsAsFactors=FALSE))

#--- page 3

temp1 <- tab[[3]][4:54] %>% 
            strsplit( "\\s{2,}")%>% 
            unlist() %>%
            matrix(nrow=204/4,ncol=4,byrow = TRUE)

for(c in 2:4){# column loop
  temp2 <- temp1[,c] %>% strsplit("% ") %>% unlist() 
  temp1[,c] <- temp2[seq(1,length(temp2),2)] 
}

# change to dataframe.
df <- rbind(df,as.data.frame(temp1,stringsAsFactors=FALSE))

#--- page 4

temp1 <- tab[[4]][4:21] %>% 
  strsplit( "\\s{2,}")%>% 
  unlist() %>%
  matrix(nrow=72/4,ncol=4,byrow = TRUE)

for(c in 2:4){# column loop
  temp2 <- temp1[,c] %>% strsplit("% ") %>% unlist() 
  temp1[,c] <- temp2[seq(1,length(temp2),2)] 
}

# change to dataframe.
df <- rbind(df,as.data.frame(temp1,stringsAsFactors=FALSE))
colnames(df) <- c("country","both","males","females")
df$country <- sub("^\\s+", "", df$country)
class(df$both) <- "numeric"

#===
# MATCHING DATA TO MAP DATA
#===

centroids <- getMap(resolution="low")        # map data.
centroids <- as.data.frame(gCentroid(centroids, byid=TRUE)) # generate centroids data frame
centroids$country <- rownames(centroids)

map.world <- map_data('world')
map.world$region %>% unique
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
                                     "Congo" = "Democratic Republic of the Congo",
                                     "Cote d'Ivoire"= "Ivory Coast",
                                     "Iran (Islamic Republic of)" = "Iran",
                                     "Trinidad and Tobago" = "Trinidad"))


map.vsly <- left_join(map.world, df, by = c('region' = 'country')) 


#===
# CREATING A COLOURCODED MAP
#===

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

plot1 <- (ggplot(data= map.vsly, 
       aes(x = long, y = lat, group = group)) +
            
            geom_polygon(aes(fill = 1-both),
                         colour="aliceblue",
                         lty=4) +
         
            scale_fill_continuous(name = "% inactive") +
         
            labs(title = "Country prevalence of insufficient physical activity", 
              subtitle = "Age standardised to the WHO Standard Population, 2016.", 
              caption = "Source: Guthold R et al. (2018) Appendix 5") +

            ditch_the_axes +
            
            theme(legend.position = c(0.1,0.2),
                  legend.background = element_rect(fill = "aliceblue"),
                  #legend.title = element_blank(),
                  panel.background = element_rect(fill = "aliceblue"),
                  legend.key.width = unit(0.5,"cm"))
)

pdf("figures/ggplot.pdf")
print(plot1)     # Plot 1 --> in the first page of PDF
dev.off() 

#===
# LOAD HSE DISTRIBUTIONS OF PHYSICAL ACTIIVTY
#===

mets <- c(0,4.1,8.7) 

# read in data, setlect the necessary variables and filter the dataset to those who answered questions, create/calc metmins variable.
hsedata <- read.dta("C:/Users/Robert/Google Drive/Model/Data/Raw/HSE15/hse2015ai.dta", convert.factors=FALSE)  %>%  # read in data
  dplyr::select(Sex,ag16g10,TotmWalD,MPAmWk,VPAmWk,MVPAmWk,wt_int) %>%             # select PA data
  dplyr::filter(MVPAmWk >= 0,                                                      # filter dataset to those who answered questions.
                TotmWalD >= 0,                                                     #  again, those answering questions.
                ag16g10 >= 0 & ag16g10 < 7) %>%                                    # limit to those aged 16-74
  dplyr::mutate(id = 1:nrow(.),                                                    # create ID variables.
                Sex = Sex-1,
                metmins = TotmWalD*mets[1] + MPAmWk*mets[2] + VPAmWk*mets[3], # calculate metmins using: mins * mets for each activity 
                cons_metmins = ifelse(metmins>10000, 10000, metmins)) # constrain to 10,000

# create percentiles
gen.perc <- hsedata %>% 
                summarise(list(enframe(quantile(cons_metmins, probs= c(seq(0.01,1,0.01)))))) %>%  # create quintiles for each group
                unnest 

#===
# DISTRIBUTION PHYSICAL ACTIVITY EVERY COUNTRY.
#===

country.matrix <- matrix(data = NA,
                         ncol = length(df$country),
                         nrow = 100, 
                         dimnames = list(c(1:100),df$country) )
country.matrix[1,] <- 0

for(c in 1:ncol(country.matrix)){ # country loop

  country <- df$country[c]

for(x in 2:100){ # percentile loop
  country.matrix[x,country] <- (gen.perc$value[x] - gen.perc$value[x-1]) * (100-df$both[df$country == country][1])/(100-df$both[df$country == "UK"]) + country.matrix[x-1,country]
} # percentile loop
}

#===
# CREATE RELATIVE RISKS FOR EACH COUNTRY
#===
increase <- 210  # based on 10 mins extra walking per day.
walk.metmins <- 1500 # we assume the heat reference met-mins is 1500.

country.matrix.new <- country.matrix + increase
rr.matrix.init     <- 1 - (1 - 0.89) * (country.matrix/walk.metmins)
rr.matrix.init[rr.matrix.init <0.7]     <- 0.7
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




plot(y = country.matrix[,"UK"], x=1:100,type= "l")
lines(y = country.matrix[,"Ukraine"], x=1:100,col="red")


plot(density(country.matrix[,"UK"]))
lines(density(country.matrix[,"Ukraine"]),col="red")
lines(density(country.matrix[,"Vietnam"]),col="blue")
lines(density(country.matrix[,"Niger"]),col="green")

Niger


