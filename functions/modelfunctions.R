# DEPRECATED #


#produces a matrix of relative risks from a matrix of met-mins.

f.getRR <- function(mets = metmins, x = lit.rr$A.METmwk, y = lit.rr$A.rr){
  
  rr.metmins <- mets  ; rr.metmins[,] <- NA    # initialise to same rows etc again.
  
  for(c in colnames(mets)){
    # the approx function estimates where within the dose response function each percentile is, assigns appropriate relative risk. 
    rr.metmins[,c] <- approx(x = x,
                             y= y,
                             method = "linear",
                             xout = mets[,c],
                             rule = 2)$y
  }
  return(rr.metmins)
}


# new function
# initialise data-frame for results
#f.getresults <- function(risk.change.lin = rr.S1.ldrf, df = merged, v.countries = countries){
#
##===
## Reductions in deaths per year.
##===
#
#  
#}


