# SETUP

rm(list=ls())

#=== 
# PACKAGE INSTALL
#===
# install.packages("kableExtra")


#===
# READ FROM LIBRARIES
#===

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
library(kableExtra)
library(dplyr)
library(rgeos)
library(rworldmap)
library(flextable)
library(viridis)
library(rlang)     #ensures can read from excel
library(readxl)    # Ensure can read from excel
library(foreign)   # Foreign Package ensures that read.dta works.
library(Rcpp)      # so can read from excel.
