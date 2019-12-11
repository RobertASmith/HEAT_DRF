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

df <- df %>% mutate(country = recode(country,  "United Kingdom" = 'UK',
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

write.csv(df,file = "data/guthold.csv")

