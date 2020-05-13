results.table3 <- read.csv(file = "./output/t375/results/S3.csv") %>%  # double dot to go back one file
  select(country,DA_drf,DA_lin,NMB_drf,NMB_lin)

library(flextable)
library(magrittr)

#### PLOT 1

plot1 <- results.table3 %>% 
  #round(digits = 2) %>%
  regulartable() %>% 
  autofit() %>% 
  width(j=~country,width=2) %>% 
  width(j=~DA_drf,width=1)%>%
  width(j=~DA_lin,width=1)%>%
  width(j=~NMB_drf,width=1)%>%
  width(j=~NMB_lin,width=1) %>% 
  set_header_labels(country = "Country", 
                    DA_drf = "non-linear", 
                    DA_lin = "linear",
                    NMB_drf = "non-linear",
                    NMB_lin = "linear") %>% 
  add_header_row(values = c("", "Annual Deaths Averted per 100,000", 
                            "Annual Deaths Averted per 100,000", 
                            "Net Monetary Benefit in 2016 USD", 
                            "Net Monetary Benefit in 2016 USD"),
                 top = TRUE )%>%  
  merge_at(i = 1, j = 2:3, part = "header")%>%  
  merge_at(i = 1, j = 4:5, part = "header")%>%
  
  # add cool bars
  compose(j = "DA_lin", 
          value = as_paragraph(minibar(DA_lin),
                               " ",
                               as_chunk(DA_lin, formater = function(x) sprintf("%.01f", x)))) %>% 
  compose(j = "DA_drf", 
          value = as_paragraph(minibar(DA_drf), 
                               " ",
                               as_chunk(DA_drf,formater = function(x) sprintf("%.01f", x) )))%>% 
  compose(j = "NMB_lin", 
          value = as_paragraph(minibar(NMB_lin),
                               " ",
                               as_chunk(NMB_lin,formater = function(x) sprintf("%.01f", x) ))) %>%
  compose(j = "NMB_drf", 
          value = as_paragraph(minibar(NMB_drf),
                               " ",
                               as_chunk(NMB_drf,formater = function(x) sprintf("%.01f", x) ))) %>% 
  
  align(j = 2:5, align = "left") %>% 
  
  add_footer_lines("All data available on the author's GitHub page") 

flextable::save_as_html(x = plot1,
                        path = "C:/Users/Robert/Google Drive/Other Projects/HEAT2/HEAT_DRF/output/t375/results.html")

#### PLOT 2
