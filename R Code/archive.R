#===
# ARCHIVE
#===
#rr.metmins <- metmins  ; rr.metmins[,] <- NA    # initialise to same rows etc again.
#
#for(c in colnames(metmins)){
## the approx function estimates where within the dose response function each percentile is, assigns appropriate relative risk. 
#  rr.metmins[,c] <- approx(x = lit.rr$A.METmwk,
#                     y=lit.rr$A.rr,
#                     method = "linear",
#                     xout = metmins[,c],
#                     rule = 2)$y
#}


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

#                     METminssq = METminswk^2)
#temp <- data.frame(RR = c(1,0.8,0.69,0.63,0.61,0.61),
#                   METhwk = c(0,3.75,11.25,18.75,31.25,57.5))%>%
#              mutate(METminswk = METhwk*60,
#                     METminssq = METminswk^2)
#
#kelly.rr.walk <- data.frame(METhwk = c(0,8,22.5,50),
#                            rr.walk = c(1,0.91,0.88,0.80)) %>% 
#                        mutate(metminswk = METhwk*60)
#kelly.rr.cycle <- data.frame(METhwk = c(0,11.5,32,65),
#                             rr.walk = c(1,0.83,0.76,0.70)) %>% 
#                        mutate(K.METmwk = METhwk*60)

#plot(x = 1:50, 
#     y = predict(loess(formula = RR~METhwk,data = temp),
#                           newdata = data.frame(METhwk=1:50))
#     )


#===
#PLOT THIS CHANGE before and after
#===
#
# pdf("figures/RRBeforeAfter.pdf")
#
# plot(rr.metmins$Australia,
#      type = "l",
#      xlab = "Percentile",
#      ylab = "Relative Risk",
#      main = "Relative Risks of Mortality Before & After \n 10min increase in walking, Australia",
#      lty = 1)
# legend(x = 70,y = 1,
#        legend = c("Before","After"),
#        lty = c(1,2),box.lwd = NA,cex=0.7)
# 
# lines(rr.metmins.new$Australia,lty = 2)
#
# dev.off() 


#===
# ESTIMATE EFFECT OF INCREASING MET-MINS BY X AMOUNT ON RELATIVE RISKS.
#===

metmins.new <- metmins  # create a new set of met-mins

metmins.new[metmins<600] <- 600 # Change metmins of those lower than 600 mets to 600.

rr.metmins.new <- rr.metmins  ; rr.metmins.new[,] <- NA   # initialise matrix

for(c in colnames(rr.metmins.new)){
  # use the old distributions plus 210 to estimat the new relative risk functions.  
  rr.metmins.new[,c] <- approx(x = lit.rr$A.METmwk,
                               y = lit.rr$A.rr,
                               method = "linear",
                               xout = metmins.new[,c],
                               rule = 2)$y
}

# heat methodology - reduction in risk is very small
increase.walk <- sum(metmins.new - metmins)/ (ncol(metmins)*nrow(metmins)) / 3
rr.S2.ldrf <- 1 - (1 - 0.89) * (increase.walk/168)  # scenario 2 is everyone meeting targets


#===
# ESTIMATE MORTALITY REDUCTION
#===

# 1. for a list of countries.
# countries <- intersect(merged$country,colnames(metmins))   # identify countries which can analyse
merged$drf <- NA                              # create column for dose response estimate net benefit
merged$lin <- NA                              # create column for linear response estimate net benefit


# 2. Estimate reductions in deaths per year.
for(x in 1:length(countries)){
  temp.country <- countries[x]
  risk.change <- rr.metmins[,temp.country] - rr.metmins.new[,temp.country]
  merged$drf[x] <- mean(risk.change * merged$mortrisk[merged$country==temp.country])
  merged$lin[x] <- (1-rr.S2.ldrf)*merged$mortrisk[merged$country==temp.country]
}

#===
# MONETARY BENEFIT estimate monetary benefit for nmb and lin responses
#===

merged$nmb_drf  <- merged$drf * merged$VSL
merged$nmb_lin  <- merged$lin * merged$VSL
merged$relative <- (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin

#===
# STORE RESULTS
#===

results.table$S2_DA_drf  = merged$drf
results.table$S2_DA_lin  = merged$lin
results.table$S2_NMB_drf = merged$drf * merged$VSL
results.table$S2_NMB_lin = merged$lin * merged$VSL
results.table$S2_NMB_relative = (merged$nmb_drf-merged$nmb_lin)/merged$nmb_lin

