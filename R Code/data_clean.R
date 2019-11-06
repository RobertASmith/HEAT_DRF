install.packages("tabulizer")
library(tabulizer)
library(dplyr)

rm(list=ls())

location <- "https://www.thelancet.com/cms/10.1016/S2214-109X(18)30357-7/attachment/0a5ff816-54d2-4ef6-ac70-1de1881f69ae/mmc1.pdf"

out <- extract_tables(location)


# new attempt

library("tidyverse")
library("stringr")
library("pdftools")

pdf_file <- file.path("data/country_prevs.pdf")

text <- pdf_text(pdf_file)

tab <- str_split(text, "\n")[29:32]

#--- page 1
temp1 <- tab[[1]][6:53] %>% 
              strsplit( "\\s{2,}")%>% 
              unlist() %>%
              matrix(nrow=192/4,ncol=4,byrow = TRUE)

# column 2
temp2 <- temp1[,2] %>% strsplit("% ") %>% unlist() 
temp1[,2] <- temp2[seq(1,96,2)] 

# column 3
temp2 <- temp1[,3] %>% strsplit("% ") %>% unlist() 
temp1[,3] <- temp2[seq(1,96,2)] 

# column 4
temp2 <- temp1[,4] %>% strsplit("% ") %>% unlist() 
temp1[,4] <- temp2[seq(1,96,2)] 

# change to dataframe.
df <- as.data.frame(temp1)

#--- page 2
temp1 <- tab[[2]][4:54] %>% 
              strsplit( "\\s{2,}")%>% 
              unlist() %>%
              matrix(nrow=204/4,ncol=4,byrow = TRUE)

# column 2
temp2 <- temp1[,2] %>% strsplit("% ") %>% unlist() 
temp1[,2] <- temp2[seq(1,102,2)] 

# column 3
temp2 <- temp1[,3] %>% strsplit("% ") %>% unlist() 
temp1[,3] <- temp2[seq(1,102,2)] 

# column 4
temp2 <- temp1[,4] %>% strsplit("% ") %>% unlist() 
temp1[,4] <- temp2[seq(1,102,2)] 

# change to dataframe.
df <- rbind(df,as.data.frame(temp1))

#--- page 3

temp1 <- tab[[3]][4:54] %>% 
            strsplit( "\\s{2,}")%>% 
            unlist() %>%
            matrix(nrow=204/4,ncol=4,byrow = TRUE)

# column 2
temp2 <- temp1[,2] %>% strsplit("% ") %>% unlist() 
temp1[,2] <- temp2[seq(1,102,2)] 

# column 3
temp2 <- temp1[,3] %>% strsplit("% ") %>% unlist() 
temp1[,3] <- temp2[seq(1,102,2)] 

# column 4
temp2 <- temp1[,4] %>% strsplit("% ") %>% unlist() 
temp1[,4] <- temp2[seq(1,102,2)] 

# change to dataframe.
df <- rbind(df,as.data.frame(temp1))

#--- page 4

temp1 <- tab[[4]][4:21] %>% 
  strsplit( "\\s{2,}")%>% 
  unlist() %>%
  matrix(nrow=72/4,ncol=4,byrow = TRUE)

# column 2
temp2 <- temp1[,2] %>% strsplit("% ") %>% unlist() 
temp1[,2] <- temp2[seq(1,36,2)] 

# column 3
temp2 <- temp1[,3] %>% strsplit("% ") %>% unlist() 
temp1[,3] <- temp2[seq(1,36,2)] 

# column 4
temp2 <- temp1[,4] %>% strsplit("% ") %>% unlist() 
temp1[,4] <- temp2[seq(1,36,2)] 

# change to dataframe.
df <- rbind(df,as.data.frame(temp1))
colnames(df) <- c("country","both","males","females")


#== ANALYSIS


