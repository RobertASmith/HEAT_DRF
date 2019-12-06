#===
# ARCHIVE
#===


# increase in MET-mins.
increase <- 210      # based on 10 mins extra walking per day.
walk.metmins <- 1500 # we assume the heat reference met-mins is 1500.

# initial matrix of relative risk, capped at 0.7
rr.matrix.init     <- 1 - (1 - 0.89) * (m.c.dists/walk.metmins)
rr.matrix.init[rr.matrix.init <0.7]     <- 0.7

# new matrix of relativ risk, capped at 0.7
m.c.dists.new      <- m.c.dists + increase
rr.matrix.new      <- 1 - (1 - 0.89) * (country.matrix.new/walk.metmins)
rr.matrix.new[rr.matrix.new <0.7]     <- 0.7

# plot new populations
plot(rr.matrix.init[,10],type = "l")
lines(rr.matrix.new[,10],col="blue")

# using single pop mortality
mort.rate <- 550 / 100000

# multiply this by mortality rate to estimate risks for each.
risk.init <- rr.matrix.init * mort.rate
risk.new  <- rr.matrix.new * mort.rate

# estimate the number of deaths averted per 100,000 per year.
da.hundk  <- (risk.init - risk.new) * 100000

df$saved  <-  colMeans(da.hundk)

#===
# PLOT IN GGPLOT THE INCREASE.
#===
map.vsly <- left_join(map.world, df, by = c('region' = 'country')) 

plot1 <- (ggplot(data= map.vsly, 
                 aes(x = long, y = lat, group = group)) +
            
            geom_polygon(aes(fill = saved),
                         colour="aliceblue",
                         lty=4) +
            
            scale_fill_continuous(name = "Deaths Averted") +
            
            labs(title = "Deaths averted by walking intervention", 
                 subtitle = "Per 100,000 people, assuming additional 10mins per day per person.", 
                 caption = "Sources: DRF from Aram et al. 2015, VSL from HEAT") +
            
            ditch_the_axes +
            
            theme(legend.position = c(0.1,0.2),
                  legend.background = element_rect(fill = "aliceblue"),
                  #legend.title = element_blank(),
                  panel.background = element_rect(fill = "aliceblue"),
                  legend.key.width = unit(0.5,"cm"))
)

pdf("figures/livessaved.pdf")
print(plot1)     # Plot 1 --> in the first page of PDF
dev.off() 


#===
# ARCHIVE
#===

# plot the old density and new density
plot(density(init.pop))
lines(density(new.pop), col = "red")

lm.rr <- lm(formula = RR ~ METminswk + METminssq ,data = temp)

# plot linear model results
fake <- data.frame(METminswk = seq(1,1800,100),
                   METminssq = seq(1,1800,100)^2)
fake$rr <- predict(object = lm.rr,
                   newdata = fake)

plot(temp$METminswk,temp$RR)
lines(x=fake$METminswk,y = fake$rr )



plot(y = country.matrix[,"UK"], x=1:100,type= "l")
lines(y = country.matrix[,"Ukraine"], x=1:100,col="red")


plot(density(country.matrix[,"UK"]))
lines(density(country.matrix[,"Ukraine"]),col="red")
lines(density(country.matrix[,"Vietnam"]),col="blue")
lines(density(country.matrix[,"Niger"]),col="green")

Niger

