#===
# CREATE DOSE/LINEAR RELATIVE RISKS PLOT
#===
library(ggplot2)
library(reshape2)

# This script simply takes the sose response relationships from Kahlmeier et al. & Woodcock et al. & plots them.
#
# using Woodcock et al. 2011 (https://doi.org/10.1093/ije/dyq104).




f_keepworkenvironmentclear <- function(){
  
  b = 168 ; a = 0.89 ; p = 1:3000

# linear data
temp <- data.frame (mets = p,
                    lin=(1 - (1 - a) * (p/b))) 
temp$lin[temp$lin<0.7] <- 0.7

#non-linear data
t = 0.25 ; temp$t25 = a^(p/b*3)^t
t = 0.5  ; temp$t50 = a^(p/b*3)^t
t = 0.75  ; temp$t75 = a^(p/b*3)^t
t = 0.375  ; temp$t375 = a^(p/b*3)^t

temp <- melt(data = temp,id.vars = "mets",measure.vars = colnames(temp)[2:6],variable.name = "type",value.name = "rr")


ggsave(filename = "Risk Functions.pdf",
       path = "output",
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot= ggplot(data = temp)+
                geom_line(aes(x=mets,y=rr,col = type))+
  
              theme_classic()+
              
              labs(title = "Relative risk using linear & non-linear dose response relationship",
                   y = 'Relative Risk of Mortality',
                   x = 'Weekly MET-mins',
                   caption = "Sources: Kahlmeier et al., 2017 ; Woodcock et al., 2011") +
              
              scale_fill_manual(aesthetics = "col",values = c("black", "red", "purple","blue","green"))+
              
              theme(legend.position = c(0.2,0.2),
                    legend.title = element_blank(),
                    legend.key.width = unit(0.5,"cm"),
                    legend.background = element_blank())
)
}
f_keepworkenvironmentclear()


  
  
  
  
