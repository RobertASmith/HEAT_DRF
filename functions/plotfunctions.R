# PLOT FUNCTIONS

#=== 
# DEATHS AVERTED PLOT
#===

f.deathsavertedplot <- function(x = "S1_DA_lin", y = "S1_DA_drf" , col = "IPAP", title = " "){
  plot <- ggplot(data = results.table,
       aes_string(x=x,y=y,col = col))+
  theme_classic()+
  geom_point()+
  geom_abline(slope = 1)+
  annotate(geom="text", x=40, y=40, 
           label="Equality", color="black")+
  labs(title = title,
       subtitle = "Annual deaths averted per 100,000: Non-linear relationship vs linear relationship",
       caption = "IPAP: Insufficient Physical Activity Prevalence;  Data Sources: WHO (2018), GBD (2017), OECD(2015)", 
       x = "Linear relationship ", 
       y = "Non-linear relationship")+
  #xlim(0, 150) + ylim(0,150)+
  geom_label_repel(data = as.data.frame(results.table), 
                   label = results.table$country, 
                   size = 2,
                   fill = NA,
                   label.size = NA,
                   alpha = 0.6)#,
#nudge_x = 2, nudge_y = -5,
#direction = "y",
#segment.color = "blue",colour = "blue")+
#theme(legend.position = c(0.9, 0.2))+

return(plot)
}


#===
# WORLD MAP PLOTS
#===

f.maprelative <- function(relative = "S1_NMB_relative",title = "",limits = c(-100,100)){
  
map.world <- map_data('world')

map <- left_join(map.world, results.table, by = c('region' = 'country')) 

# plot 1 net monetary benefit of dose response function
plot <- (ggplot(data= map, 
                 aes(x = long, y = lat, group = group)) +
            
            geom_polygon(aes_string(fill = relative),
                         colour="aliceblue",
                         lty=4) +
            
            #scale_fill_continuous(name = "$nmb/pp",low = "white", high = "blue") +
            scale_fill_viridis(discrete=FALSE,name = "% Dif",limits = limits) +
            
            labs(title = paste(title), 
                 subtitle = "% difference in NMB between non-linear relationship & linear relationship, Europe (2016)", 
                 caption = "Sources: WHO (2019); OECD (2012)") +
            
            coord_fixed(xlim = c(-20, 70),  
                        ylim = c(32, 70), 
                        ratio = 1.3) +
            
            theme(legend.position = c(0.1,0.2),
                  legend.background = element_blank(),
                  #legend.title = element_blank(),
                  panel.background = element_rect(fill = "aliceblue"),
                  legend.key.width = unit(0.5,"cm"),
                  axis.text = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank())            )

return(plot)

}


f.mapdoseresponse <- function(doseresponse = "S1_NMB_drf",title = "", limits = c(0,1000)){
  

map.world <- map_data('world')

map <- left_join(map.world, results.table, by = c('region' = 'country')) 



# plot 2 net monetary benefit of linear risk function
plot <- (ggplot(data= map, 
                 aes(x = long, y = lat, group = group)) +
            
            geom_polygon(aes_string(fill = doseresponse),
                         colour="aliceblue",
                         lty=4) +
            
            #scale_fill_continuous(name = "$nmb/pp",low = "white", high = "blue") +
            scale_fill_viridis(discrete=FALSE,name = "USD (2016)",limits = limits) +
            labs(title = title, 
                 subtitle = "Annual Monetary Benefit per capita, Europe (2016)", 
                 caption = "Sources: WHO (2019); OECD (2012)") +
            
            coord_fixed(xlim = c(-20, 70),  
                        ylim = c(32, 70), 
                        ratio = 1.3) +
            
            theme(legend.position = c(0.1,0.2),
                  legend.background = element_blank(),
                  #legend.title = element_blank(),
                  panel.background = element_rect(fill = "aliceblue"),
                  legend.key.width = unit(0.5,"cm"),
                  axis.text = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank())
)

return(plot)

}


#===
# PLOT SPECIFIC COUNTRY PA DISTRIBUTION
#===


#hist(metmins[,"Slovenia"])
