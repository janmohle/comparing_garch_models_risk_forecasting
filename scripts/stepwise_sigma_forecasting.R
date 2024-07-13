#################################################################################################################
#### Predicting one step ahead VaR and ES across different variance specifications and distribution assumptions #
#################################################################################################################

# Explanation:
#  Script calculates forecasts for all data sets across all variance specifications and distribution assumptions
#  Takes multiple hours to run!!!!


# To-do:
#  Rename sigma to VaR_ES

# Subsets data for faster stepwise variance forecasting if length_data is specified
if(exists('length_data')){
  DAX <- DAX[1:length_data,]
  WIG <- WIG[1:length_data,]
  BTC <- BTC[1:length_data,]
  GOLD <- GOLD[1:length_data,]
}


# Vector of all index names
indices <- c('DAX',
             'WIG',
             'BTC',
             'GOLD')

# Subsets indices for faster stepwise variance forecasting if start_index and end_index are specified
if(exists('start_index') & exists('end_index')){
  indices <- indices[start_index:end_index]
}

# List of all variance specifications (ADD)
var.spec.list <- list(spec1 = list(model = 'sGARCH',
                                   garchOrder = c(1,1)),
                      spec2 = list(model = 'eGARCH',
                                   garchOrder = c(1,1)),
                      spec3 = list(model = 'gjrGARCH',
                                   garchOrder = c(1, 1)))

# ARMA(1,1) as mean model for all estimations (to capture as much dependencies of consecutive observations as possible while still keeping model parcimonious)
armamean <- list(armaOrder = c(1,1),
                 include.mean = TRUE)


# List of all distribution assumptions
dist.spec.list <-  list(distr1 = 'norm',
                        distr2 = 'std',
                        distr3 = 'ged',
                        distr4 = 'snorm',
                        distr5 = 'sstd',
                        distr6 = 'sged',
                        distr7 = 'ghyp',
                        distr8 = 'nig',
                        distr9 = 'ghst',
                        distr10 = 'jsu')


# Specifying estimation window width
width = 500


# Loop through indices

for(index in indices){
  
  # Extracting returns and dates
  index_name <- index
  data <- get(index)
  data_returns <- data$Return
  data_dates <- data$Date
  
  # Storing return data as zoo object
  returns <- zoo(data_returns, data_dates)
  
  # Creating empty list for forecasted VaR and ES values
  forecasted.VaR.list <- list()
  forecasted.ES.list <- list()
  
  # Creating counter for variance specification in current loop over garch specifications
  spec_i <- 0
  
  # Loop over variance specifications
  for (var.spec in var.spec.list) {
    
    spec_i <- spec_i + 1
    
    # Loop over distribution
    for (dist.spec in dist.spec.list) {
      
      # Start time of loop
      start <- Sys.time()
      
      cat('Current iteration: ',' Index: ',index_name, ' Spec: ', spec_i, ' Dist: ', dist.spec, '\n')
      rolling.VaR.ES <- rollapply(returns,
                                  width = width,
                                  FUN = function(x) predict_VaR_ES_1_ahead(data = x,
                                                                           var.spec = var.spec,
                                                                           mean.spec = armamean,
                                                                           dist.spec = dist.spec,
                                                                           tolerance_lvl = 0.05,
                                                                           spec_i = spec_i,
                                                                           dist = dist.spec),
                                 align = 'right')
      
      
      
      # Returning object after rollapply is difficult to handle, because predict_VaR_ES_1_ahead returns list in each iteration
      # Dates and values for VaR and ES must be extracted and stored in a more handy way
      
      # Extract dates
      dates <- index(rolling.VaR.ES)
      
      # Creating data frame for easier extraction of values
      rolling.VaR.ES.df <- as.data.frame(rolling.VaR.ES)
      
      # Extracting VaR and ES  values with loop
      rolling.VaR.values <- vector()
      for(obs in rolling.VaR.ES.df[['VaR']]){
        VaR.of.obs <- as.vector(obs)
        rolling.VaR.values <- append(rolling.VaR.values, VaR.of.obs)
      }
      
      rolling.ES.values <- vector()
      for(obs in rolling.VaR.ES.df[['ES']]){
        ES.of.obs <- as.vector(obs)
        rolling.ES.values <- append(rolling.ES.values, ES.of.obs)
      }
      
      # Creating clean zoo object for VaR and ES
      rolling.VaR.zoo <- zoo(rolling.VaR.values, dates)
      
      rolling.ES.zoo <- zoo(rolling.ES.values, dates)
      
      
      # Creating list with results
      entryname <- paste0('spec', spec_i, dist.spec)
      forecasted.VaR.list[[entryname]] <- rolling.VaR.zoo
      forecasted.ES.list[[entryname]] <- rolling.ES.zoo
      
      
      # End time of loop
      end <- Sys.time()
      
      # Print time loop needed to run
      time <- as.numeric(end - start, units = 'mins')
      cat('Iteration needed ', time, ' minutes to run\n')
    }
  }
  # Linear interpolation of NAs
  for(i in 1:length(forecasted.VaR.list)){
    forecasted.VaR.list[[i]] <- na.approx(forecasted.VaR.list[[i]])
    forecasted.ES.list[[i]] <- na.approx(forecasted.ES.list[[i]])
    
    # Assign forecasted.VaR.list and forecasted.ES.list to individual list for each index
    listname.VaR <- paste0(index_name, '.forecasted.VaR')
    assign(listname.VaR, forecasted.VaR.list)
    
    listname.ES <- paste0(index_name, '.forecasted.ES')
    assign(listname.ES, forecasted.ES.list)
  }
}

if(save_results == TRUE){
#save result in RData file
save(DAX.forecasted.VaR,
     file = 'output/DAX_forecasted_VaR.RData')

save(WIG.forecasted.VaR,
     file = 'output/WIG_forecasted_VaR.RData')

save(BTC.forecasted.VaR,
     file = 'output/BTC_forecasted_VaR.RData')

save(GOLD.forecasted.VaR,
     file = 'output/GOLD_forecasted_VaR.RData')

save(DAX.forecasted.ES,
     file = 'output/DAX_forecasted_ES.RData')

save(WIG.forecasted.ES,
     file = 'output/WIG_forecasted_ES.RData')

save(BTC.forecasted.ES,
     file = 'output/BTC_forecasted_ES.RData')

save(GOLD.forecasted.ES,
     file = 'output/GOLD_forecasted_ES.RData')

# Remove lists from current session
rm(list = c('DAX.forecasted.VaR',
            'WIG.forecasted.VaR',
            'BTC.forecasted.VaR',
            'GOLD.forecasted.VaR',
            'DAX.forecasted.ES',
            'WIG.forecasted.ES',
            'BTC.forecasted.ES',
            'GOLD.forecasted.ES'))
}