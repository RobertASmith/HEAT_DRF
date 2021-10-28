# ============
# This code creates the plots for t = 0.375 (the main analysis), this could be easily adapted by 
# reading in the data files from different levels of t.
# ============

#==================== PLOTS SCENARIO 1 =============
# 10 minutes additional walking per person.

#LOAD SCENARIO 1 DATA
results.table1 <- read.csv(file = "output/t375/results/S1.csv")

# RELATIVE RESULTS
ggsave(filename = "S1_RelativeResults.pdf",
       path = "output/t375/figures",
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f_deathsavertedplot(
         data = results.table1,
         x = "DA_lin", 
         y = "DA_drf" , 
         col = "IPAP",
         title = "Scenario 1: Every person walks an additional 10 mins daily"))

# MAP RELATIVE
ggsave(filename = "S1_MapRelative.pdf",
       path = "output/t375/figures",
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f_maprelative(
          data = results.table1,
          relative = "NMB_relative",
          title = "Scenario 1: Every person walks an additional 10 mins daily",
          limits = c(min(results.table1$NMB_relative),max(results.table1$NMB_relative))))

# DOSE RESPONSE
ggsave(filename = "S1_DoseResponse.pdf",
       path = "output/t375/figures",
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f_mapdoseresponse(
         data = results.table1,
         doseresponse = "NMB_drf",
         title = "Scenario 1: Every person walks an additional 10 mins daily",
         limits = c(0,max(results.table1$NMB_drf))))


#==================== PLOTS SCENARIO 2 =============
# everyone meets WHO guidelines of 600 MET-mins/week

#LOAD SCENARIO 2 DATA
results.table2 <- read.csv(file = "output/t375/results/S2.csv")


# RELATIVE RESULTS
ggsave(filename = "S2_RelativeResults.pdf",
       path = "output/t375/figures",
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f_deathsavertedplot(
         data = results.table2,
         x = "DA_lin", 
         y = "DA_drf" , 
         col = "IPAP",
         title = "Scenario 2: Every person meets WHO Guidelines"))

# MAP RELATIVE
ggsave(filename = "S2_MapRelative.pdf",
       path = "output/t375/figures",
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f_maprelative(
         data = results.table2,
         relative = "NMB_relative",
         title = "Scenario 2: Every person meets WHO Guidelines",
         limits = c(min(results.table2$NMB_relative),max(results.table2$NMB_relative))))

# DOSE RESPONSE
ggsave(filename = "S2_DoseResponse.pdf",
       path = "output/t375/figures",
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f_mapdoseresponse(
         data = results.table2,
         doseresponse = "NMB_drf",
         title = "Scenario 2: Every person meets WHO Guidelines",
         limits = c(0,max(results.table2$NMB_drf))))

#==================== PLOTS SCENARIO 3 =============
# 10% rise in physical activity for everyone

#LOAD SCENARIO 3 DATA
results.table3 <- read.csv(file = "output/t375/results/S3.csv")


# RELATIVE RESULTS
ggsave(filename = "S3_RelativeResults.pdf",
       path = "output/t375/figures",
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f_deathsavertedplot(
         data = results.table3,
         x = "DA_lin", 
         y = "DA_drf" , 
         col = "IPAP",
         title = "Scenario 3: 10% Increase in PA for every person"))

# MAP RELATIVE
ggsave(filename = "S3_MapRelative.pdf",
       path = "output/t375/figures",
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f_maprelative(
         data = results.table3,
         relative = "NMB_relative",
         title = "Scenario 3: 10% Increase in PA for every person",
         limits = c(min(results.table3$NMB_relative),max(results.table3$NMB_relative))))

# DOSE RESPONSE
ggsave(filename = "S3_DoseResponse.pdf",
       path = "output/t375/figures",
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f_mapdoseresponse(
         data = results.table3,
         doseresponse = "NMB_drf",
         title = "Scenario 3: 10% Increase in PA for every person",
         c(0,max(results.table3$NMB_drf))))

#=================== COMBINED PLOT =======================

ggsave(filename = "RelativeAll.pdf",
       path = "output/t375/figures",
       device = "pdf",
       width = 7, height = 12,
       plot = grid.arrange(
         
         f_deathsavertedplot(
           data = results.table1,
           x = "DA_lin", 
           y = "DA_drf" , 
           col = "IPAP",
           title = "Scenario 1: Additional 10 mins daily walking"),
         
         f_deathsavertedplot(
           data = results.table2,
           x = "DA_lin", 
           y = "DA_drf" , 
           col = "IPAP",
           title = "Scenario 2: Every person meets WHO Guidelines"),
         

         f_deathsavertedplot(
           data = results.table3,
           x = "DA_lin", 
           y = "DA_drf" , 
           col = "IPAP",
           title = "Scenario 3: 10% Increase in PA for every person"),
         
         nrow=3,newpage = T))