# RESULTS TABLES FUNCTION

f.results.table <- function(table = results.table,path){
  s1.results.table <- table %>% 
    select(ISO_Code, country, S1_DA_drf, S1_DA_lin, S1_NMB_drf, S1_NMB_lin)
  s2.results.table <- table %>% 
    select(ISO_Code, country, S2_DA_drf, S2_DA_lin, S2_NMB_drf, S2_NMB_lin)
  s3.results.table <- table %>% 
    select(ISO_Code, country, S3_DA_drf, S3_DA_lin, S3_NMB_drf, S3_NMB_lin)
  
  colnames(s1.results.table) <- c('ISO3 Code', 'Country', 'DRF', 'Lin','DRF','Lin')
  colnames(s2.results.table) <- c('ISO3 Code', 'Country', 'DRF', 'Lin','DRF','Lin')
  colnames(s3.results.table) <- c('ISO3 Code', 'Country', 'DRF', 'Lin','DRF','Lin')
  
  # excel tables, for publication if latex not accepted.
  write.csv(x =  s1.results.table,file = paste(path,sep = "/","s1_results.csv"))
  write.csv(x =  s2.results.table,file = paste(path,sep = "/","s2_results.csv"))
  write.csv(x =  s3.results.table,file = paste(path,sep = "/","s3_results.csv"))
  
}
