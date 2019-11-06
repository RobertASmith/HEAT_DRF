#===
# EXTRACTING TABLE FROM PDF.
#===
rm(list=ls())
library("tidyverse")
library("stringr")
library("pdftools")

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

#== ANALYSIS

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
                                     "Congo" = "Democratic Republic of the Congo"))


map.vsly <- left_join(map.world, df, by = c('region' = 'country')) 


#== CREATING THE PLOT

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
            
            geom_polygon(aes(fill = both),
                         colour="aliceblue",
                         lty=4) +
            
            #scale_fill_gradientn(colours = gray.colors(n=20,start = 0.9,end = 0.5), 
            #                     breaks = seq(0,200,10),
            #                     aes(VSLY),
            #                     na.value = NA) +
            #
            #coord_fixed(xlim = c(-20, 70),  
            #            ylim = c(30, 70), 
            #            ratio = 1.3) +
            #
            ditch_the_axes +
            
            theme(legend.position = c(0.1,0.1),
                  legend.background = element_rect(fill = "aliceblue"),
                  legend.title = element_blank(),
                  panel.background = element_rect(fill = "aliceblue"),
                  legend.key.width = unit(0.5,"cm"))
)

pdf("figures/ggplot.pdf")
print(plot1)     # Plot 1 --> in the first page of PDF
dev.off() 


