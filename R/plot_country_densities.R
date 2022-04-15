rm(list = ls())

source("R/load_and_clean.R")

# creating tidy country distributions:
df_dist <- read.csv("data/distributions.csv")
names(df_dist)[1] <- "perc"

# melt to long format
df_long <- melt(data = df_dist, 
     id.vars = "perc",
     value.name = "metmins",
     variable.name = "country")

# create fancy ggplot
df_plot <- df_long[df_long$country %in% c("UK", "Slovenia", "Spain", "France", "Latvia", "Poland"),]

density_plot <- ggplot(data = df_plot,
       aes(x = metmins,
           col = country,
           fill = country))+
  theme_classic()+
  geom_density(orientation = "x", 
               alpha = 0.25,
               size = 1.1)+
  scale_x_continuous(name = "MET-mins per week",
                     limits = c(0,10000), 
                     expand = c(0,0))+
  scale_y_continuous(name = "Density", 
                     expand = c(0,0))+
  scale_fill_brewer(type = "qual",
                    "Country", 
                    direction = -1,
                    aesthetics = c("fill", "col"))+
  labs(title = "Density plot of population physical activity distribution estimates",
          subtitle = "Sample of 6 countries included in the analysis",
          caption = "Based on method from Hafner et al.")+
  theme(legend.position = c(0.8,0.8))
  

ggsave(plot = density_plot,
       path = "output",
       filename = "country_density_plot.pdf",
       device = "pdf",
       width = 12,
       height = 8)


HEATcountries <- names(df_dist)[names(df_dist) %in% intersect(merged$country,colnames(metmins))]

kable_table1 <- kable(x = df_dist[,HEATcountries[1:floor(length(HEATcountries)/2)]],
      align = "c",
      row.names = 1:100,
      caption = "Percentiles of Weekly MET-mins by country",
      digits = 0)

kable_table2 <- kable(x = df_dist[,HEATcountries[ceiling(length(HEATcountries)/2):length(HEATcountries)]],
                      align = "c",
                      row.names = 1:100,
                      caption = "Percentiles of Weekly MET-mins by country",
                      digits = 0)

save_kable(x = kable_table1, 
           file = "output/country_dist_tables1.pdf")

save_kable(x = kable_table2, 
           file = "output/country_dist_tables2.pdf")


