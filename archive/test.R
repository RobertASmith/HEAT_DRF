library(grid)    
library(gridExtra)  

graphcols <- 1:22
graphcols <- 23:44

df <- metmins %>% 
        round(digits = 2) %>% 
        mutate(Percentile = 1:100) %>% 
        select(c("Percentile",graphcols)) 

dim(df)  
maxrow = 34
npages = ceiling(nrow(df)/maxrow)      

# set the theme
tt <- ttheme_default(base_size = 4)#,core = list(fg_params =list(fontsize = 4)))

pdf("test.pdf", height = 11, width = 8.5,paper = "a4r")  
idx = seq(1, maxrow)  
grid.table(df[idx,],rows = NULL,theme = tt)  
for(i in 2:npages){
  grid.newpage();
  if(i*maxrow <= nrow(df)){
    idx = seq(1+((i-1)*maxrow), i * maxrow)
  }
  else{
    idx = seq(1+((i-1)*maxrow), nrow(df))
  }
  grid.table(df[idx, ],rows = NULL,theme = tt)
}
dev.off()



# knit R method

exampleDF <- data.frame(a = 1:5, b = letters[1:5])
knitr::kable(exampleDF)
