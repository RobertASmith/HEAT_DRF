# LOAD AND CLEAN

# Guthold et al. 2014 data on IPAP
df <- read.csv(file = "data/guthold.csv",
               stringsAsFactors = FALSE,
               row.names = 1)%>% 
  select(country,both) %>% 
  rename(IPAP = both)

# PA distributions for each country
metmins <- read.csv("data/distributions.csv",
                    row.names = 1,
                    check.names=FALSE, 
                    stringsAsFactors = F)

# general distribution (UK)
gen.perc  <- read.csv("data/general_dist.csv",
                      row.names = 1,
                      check.names=FALSE) %>%
  mutate(name = seq(0.01,1,0.01)) %>% 
  rename(pcnt = name)

# mortality rates
heat.mort <- read.csv("data/mortality_rates.txt",
                      header = TRUE,
                      stringsAsFactors = F)[,c(1,2,12,13)] %>%
  filter(age_group == "20-74") %>% 
  spread(key = age_group,value = value_heatdata) %>% 
  rename(country = country_name_heat,
         ISO_Code = iso3,
         #age2044  = '20-44',
         #age2064  = "20-64",
         mortrisk  = '20-74') %>%
  #age4564  = '45-64',
  #age4574  = '45-74')
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
vsl <- read.csv(file = "data/vsl_heat.csv",
                stringsAsFactors = F,
                col.names = c('ISO_Code','Country2','VSL','ISO_heat','modified')) %>% 
  select (ISO_Code,VSL) # read in value of statistical life estimates

# add in relative risks from xxxx et al. 

# Merge these datasets
merged <- merge(x = heat.mort,y = vsl)        # Merge the mortality rates and VSL measures
merged <- merge(x= merged, y = df)            # Merge the df too.

# constrain the metmins data to HEAT countries
metmins <- metmins[,intersect(merged$country,colnames(metmins))]

# remove unnecessary data
rm(df,gen.perc,heat.mort,vsl)                 # remove unnecessary data now merged.
