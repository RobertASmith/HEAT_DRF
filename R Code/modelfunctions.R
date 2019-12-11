# produces a matrix of relative risks from a matrix of met-mins.

f.getRR <- function(mets = metmins){
  rr.metmins <- mets  ; rr.metmins[,] <- NA    # initialise to same rows etc again.
  
  for(c in colnames(mets)){
    # the approx function estimates where within the dose response function each percentile is, assigns appropriate relative risk. 
    rr.metmins[,c] <- approx(x = lit.rr$A.METmwk,
                             y=lit.rr$A.rr,
                             method = "linear",
                             xout = mets[,c],
                             rule = 2)$y
  }
  return(rr.metmins)
}


# new function
# initialise data-frame for results
f.getresults <- function(risk.change.lin = rr.S1.ldrf, df = merged, v.countries = countries){

# 2. Estimate reductions in deaths per year.
for(x in 1:length(v.countries)){
  risk.change <- rr.metmins[,v.countries[x]] - rr.metmins.new[,v.countries[x]]
  df$drf[x] <- mean(risk.change * df$mortrisk[df$country==v.countries[x]])
  df$lin[x] <- (1-risk.change.lin)* df$mortrisk[df$country==v.countries[x]]
}

  #===
  # MONETARY BENEFIT estimate monetary benefit for nmb and lin responses
  #===
  
  df$nmb_drf  <- df$drf * df$VSL
  df$nmb_lin  <- df$lin * df$VSL
  df$relative <- (df$nmb_drf - df$nmb_lin)/df$nmb_lin
  
  temp <- list(drf = df$drf,
               lin = df$lin,
               nmb_drf = df$nmb_drf,
               nmb_lin = df$nmb_lin,
               relative = df$relative)
  return(temp)
  
}


