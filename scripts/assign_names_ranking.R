#######################################################################
####    Assign variance specification and distribution names      ####
#######################################################################

assign_speci_dist_name <- function(speci_dist){
  
  # Does number of dist have 1 or 2 digits?
  start_dist_name <- ifelse(substr(speci_dist, 6, 6) == '_', 7, 8)
  
  speci <- substr(speci_dist, 1, start_dist_name-2)
  dist <- toupper(substr(speci_dist, start_dist_name, nchar(speci_dist)))
  
  if(speci == 'spec1'){
    spec_name <- 'ARCH'
  } else if(speci == 'spec2'){
    spec_name <- 'GARCH'
  } else if(speci == 'spec3'){
    spec_name <- 'EGARCH'
  } else if(speci == 'spec4'){
    spec_name <- 'GJR-GARCH'
  } else if(speci == 'spec5'){
    spec_name <- 'TGARCH'
  } else if(speci == 'spec6'){
    spec_name <- 'AVGARCH'
  } else if(speci == 'spec7'){
    spec_name <- 'APARCH'
  } else if(speci == 'spec8'){
    spec_name <- 'NGARCH'
  } else if(speci == 'spec9'){
    spec_name <- 'NAGARCH'
  } else if(speci == 'spec10'){
    spec_name <- 'FGARCH'
  } else if(speci == 'spec11'){
    spec_name <- 'CGARCH'
  } else {
    spec_name <- 'Error'
  }
  
  entry_name <- paste0(spec_name, ' ', dist)
  
  return(entry_name)
}


best.models.with.names <- best.models

for(col in names(best.models)){
  if(col %in% indices){
    
    for(i in 1:length(best.models[[col]])){
      
      best.models.with.names[[col]][i] <- assign_speci_dist_name(best.models[[col]][i])
      
    }
    
  }
}

rm(assign_speci_dist_name, col, i)

#colnames(best.models.with.names) <- c('DAX', 'FZ Loss DAX', 'p DAX', 'WIG', 'FZ Loss WIG', 'p WIG', 'BTC', 'FZ Loss BTC', 'p BTC', 'GLD', 'FZ Loss GLD', 'p GLD')
colnames(best.models.with.names) <- c('DAX', 'FZ Loss', 'p value', 'WIG', 'FZ Loss', 'p value', 'Bitcoin', 'FZ Loss', 'p value', 'Gold', 'FZ Loss', 'p value')





