#===
# CREATE DOSE/LINEAR RELATIVE RISKS PLOT
#===
library(ggplot2)
library(reshape2)

# This script simply takes the sose response relationships from Kahlmeier et al. & Woodcock et al. & plots them.
#
# using Woodcock et al. 2011 (https://doi.org/10.1093/ije/dyq104).




#' Creates a plot for the publication.
#'
#' @param b reference walking 
#' @param a relative risk at reference walking
#' @param p numeric values for mins walking
#'
#' @return nothing, saves file to folder.
f_keepworkenvironmentclear <- function(b = 168,
                                       a = 0.89,
                                       p = 1:3000,
                                       folder = "output",
                                       file_name = "Risk Functions.pdf"){
  
# linear data
temp <- data.frame(mets = p*3,
                   lin=(1 - (1 - a) * (p/b))) 
temp$lin[temp$lin<0.7] <- 0.7

#non-linear data
t = 0.25 ; temp$t25 = a^(p/b*3)^t
t = 0.5  ; temp$t50 = a^(p/b*3)^t
t = 0.75  ; temp$t75 = a^(p/b*3)^t
t = 0.375  ; temp$t375 = a^(p/b*3)^t

# melt data for ggplot2
temp <- melt(data = temp,
             id.vars = "mets",
             measure.vars = colnames(temp)[2:6],
             variable.name = "type",
             value.name = "rr")

# create ggplot
plot1 <- ggplot(data = temp)+
  geom_line(aes(x=mets,
                y=rr,
                col = type),
            size = 1.25,
            alpha = 0.75)+
  
  theme_classic()+
  
  labs(title = "Relative Risk of Mortality using linear & non-linear dose response relationships",
       subtitle = "Power transformations donated in brackets",
       caption = "Sources: Kahlmeier et al., 2017 ; Woodcock et al., 2011") +
  
  scale_color_manual("Dose Response Function",
                     aesthetics = "col",
                     labels = c("lin" = "Linear (HEAT)", 
                                "t25" = "Non-linear (t = 0.25)", 
                                "t50" = "Non-linear (t = 0.5)", 
                                "t75" = "Non-linear (t = 0.75)", 
                                "t375" = "Non-linear (t = 0.375)"),
                     values = c("lin" = "black", 
                                "t25" = "red", 
                                "t50" = "purple", 
                                "t75" = "blue", 
                                "t375" = "green"))+
  
  scale_x_continuous(expand = c(0,0),
                     name = 'Weekly MET-mins',
                     limits = c(0,6000),breaks = seq(0, 6000, by = 1000))+
  
  scale_y_continuous(expand = c(0,0),
                     name = 'Relative Risk',
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2))+
  
  theme(legend.position = c(0.2,0.2),
        legend.key.width = unit(0.5,"cm"),
        legend.background = element_blank())

# save data...
ggsave(filename = file_name,
       path = folder,
       device = "pdf",
       width = 10, height = 6,
       plot= plot1)

return(plot1)
}


f_keepworkenvironmentclear()


  
  
  
  
