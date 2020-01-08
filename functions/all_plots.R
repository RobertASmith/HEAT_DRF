# ALL PLOTS

#==================== PLOTS SCENARIO 1 =============
# 10 minutes additional walking per person.

# RELATIVE RESULTS
ggsave(filename = "S1_RelativeResults.pdf",
       path = paste(path),
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f.deathsavertedplot(
         x = "S1_DA_lin", 
         y = "S1_DA_drf" , 
         col = "IPAP",
         title = "Scenario 1: Additional 10 mins daily walking"))

# MAP RELATIVE
ggsave(filename = "S1_MapRelative.pdf",
       path = paste(path),
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f.maprelative(
          relative = "S1_NMB_relative",
          title = "Scenario 1: Additional 10 mins daily walking",
          limits = c(-100,100)))

# DOSE RESPONSE
ggsave(filename = "S1_DoseResponse.pdf",
       path = paste(path),
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f.mapdoseresponse(
         doseresponse = "S1_NMB_drf",
         title = "Scenario 1: Additional 10 mins daily walking",
         limits = c(0,800)))


#==================== PLOTS SCENARIO 2 =============
# everyone meets WHO guidelines of 600 MET-mins/week

# RELATIVE RESULTS
ggsave(filename = "S2_RelativeResults.pdf",
       path = paste(path),
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f.deathsavertedplot(
         x = "S2_DA_lin", 
         y = "S2_DA_drf" , 
         col = "IPAP",
         title = "Scenario 2: Every person meets WHO Guidelines"))

# MAP RELATIVE
ggsave(filename = "S2_MapRelative.pdf",
       path = paste(path),
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f.maprelative(
         relative = "S2_NMB_relative",
         title = "Scenario 2: Every person meets WHO Guidelines",
         limits = c(-100,100)))

# DOSE RESPONSE
ggsave(filename = "S2_DoseResponse.pdf",
       path = paste(path),
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f.mapdoseresponse(
         doseresponse = "S2_NMB_drf",
         title = "Scenario 2: Every person meets WHO Guidelines",
         limits = c(0,1000)))

#==================== PLOTS SCENARIO 3 =============
# 10% rise in physical activity for everyone

# RELATIVE RESULTS
ggsave(filename = "S3_RelativeResults.pdf",
       path = paste(path),
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f.deathsavertedplot(
         x = "S3_DA_lin", 
         y = "S3_DA_drf" , 
         col = "IPAP",
         title = "Scenario 3: 10% Increase in PA for every person"))

# MAP RELATIVE
ggsave(filename = "S3_MapRelative.pdf",
       path = paste(path),
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f.maprelative(
         relative = "S3_NMB_relative",
         title = "Scenario 3: 10% Increase in PA for every person",
         limits = c(-100,100)))

# DOSE RESPONSE
ggsave(filename = "S3_DoseResponse.pdf",
       path = paste(path),
       device = "pdf",
       width = 3.93*2, height = 4.99*2,
       plot = f.mapdoseresponse(
         doseresponse = "S3_NMB_drf",
         title = "Scenario 3: 10% Increase in PA for every person",
         limits = c(0,1000)))

#=================== COMBINED PLOT =======================

ggsave(filename = "RelativeAll.pdf",
       path = paste(path),
       device = "pdf",
       width = 7, height = 12,
       plot = grid.arrange(
         
         f.deathsavertedplot(
           x = "S1_DA_lin", 
           y = "S1_DA_drf" , 
           col = "IPAP",
           title = "Scenario 1: Additional 10 mins daily walking"),
         
         f.deathsavertedplot(
           x = "S2_DA_lin", 
           y = "S2_DA_drf" , 
           col = "IPAP",
           title = "Scenario 2: Every person meets WHO Guidelines"),
         
         f.deathsavertedplot(
           x = "S3_DA_lin", 
           y = "S3_DA_drf" , 
           col = "IPAP",
           title = "Scenario 3: 10% Increase in PA for every person"),
         
         nrow=3,newpage = T))