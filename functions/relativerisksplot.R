#===
# CREATE DOSE/LINEAR RELATIVE RISKS PLOT
#===

# New dose response relationship, using Woodcock et al. 2011 (https://doi.org/10.1093/ije/dyq104).
# Have therefore replaced other studies by Kelly et al., 2014 and Aram et al. 2015.
#
# Method basically turns linear relationship plotted here:
x <-  1:3000        # range of met-mins on x axis
y <- (1 - (1 - 0.89) * (x/168))  ;  y[y<0.7] <- 0.7   # y axis simply linear relationship

plot(x = x,
     y = y,
     type = "l", 
     xlab = 'Weekly MET-mins', 
     ylim = c(0.5,1),
     ylab = "Relative Risk",
     main = "Relative risk using linear & non-linear \n dose response relationship"
)

legend('topright', 
       cex = 0.7,
       legend = c("Linear","0.25","0.375","0.5","0.75"),
       lty = c(1,2,3,4,5), 
       col = c("black","black","blue","black","black"),
       title = "Power Transformation",
       bty = "n")

# Into a curve using equation: RR = a^(p/b)^t     # From Oliver Mytton PhD Thesis page 140
# where a = reference RR, b = reference metmins, t = log transformation, p = physical activity levels (met-mins/wk).

# set parameters as from HEAT manual, 168mins at 3METS, 0.89 RR.
b = 168*3 ; a = 0.89 ; p = x

# 0.25 power transformation
t = 0.25
lines(x = p, y = a^(p/b)^t, lty = 2)

# 0.5 power transformation
t = 0.5
lines(x = p, y = a^(p/b)^t, lty = 4)

# 0.75 power transformation
t = 0.75
lines(x = p, y = a^(p/b)^t, lty = 5)

# 0.375 power transformation (as per Woodcock et al.)
t = 0.375 
lines(x = p, y = a^(p/b)^t, lty = 3, col = "blue")

# I use the value 0.375 throughout as done by Woodcock et al. 2010 (https://doi.org/10.1093/ije/dyq104).
# Then I vary the analysis using the others as sensitivity analysis.
