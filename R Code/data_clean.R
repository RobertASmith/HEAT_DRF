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
df <- as.data.frame(temp1)


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
df <- rbind(df,as.data.frame(temp1))

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
df <- rbind(df,as.data.frame(temp1))

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
df <- rbind(df,as.data.frame(temp1))
colnames(df) <- c("country","both","males","females")


#== ANALYSIS


