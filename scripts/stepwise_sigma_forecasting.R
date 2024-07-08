#################################################################################################################
#### Predicting one step ahead VaR and ES across different variance specifications and distribution assumptions #
#################################################################################################################

# Explanation:
#  Script calculates forecasts for all data sets across all variance specifications and distribution assumptions
#  Takes multiple hours to run!!!!


# To-do:
#  Rename sigma to VaR_ES
#  Include counter for non-converging calculations
#  Exclude lbfgs from solver in function
#  Delete second solver trial from function


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
  
  index_name <- index
  data <- get(index)
  data_returns <- data$Return
  data_dates <- data$Date
  
  # Storing return data as zoo object
  
  returns <- zoo(data_returns, data_dates)
  
  # Invoking empty list for forecasted VaR and ES values
  forecasted.VaR.ES.list <- list()
  
  # Invoking counter for variance specification in current loop over garch specifications
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
                                                                          dist.spec = dist.spec,
                                                                          tolerance_lvl = 0.05,
                                                                          spec_i = spec_i,
                                                                          dist = dist.spec),
                                 align = 'right')
      
      # Creating list with results
      entryname <- paste0('spec', spec_i, dist.spec)
      forecasted.VaR.ES.list[[entryname]] <- rolling.VaR.ES
      
      
      # End time of loop
      end <- Sys.time()
      
      # Print time loop needed to run
      time <- as.numeric(end - start, units = 'mins')
      cat('Iteration needed ', time, ' minutes to run\n')
    }
  }
  # Linear interpolation of NAs
  for(i in 1:length(forecasted.VaR.ES.list)){
    forecasted.VaR.ES.list[[i]] <- na.approx(forecasted.VaR.ES.list[[i]])
    
    # Assign fforecasted.VaR.ES.list to individual list for each index
    listname <- paste0(index_name, '.forecasted.VaR.ES')
    assign(listname, forecasted.VaR.ES.list)
  }
}


#safe result in RData file
save(DAX.forecasted.VaR.ES,
     file = 'output/DAX_forecasted_VaR_ES.RData')

save(WIG.forecasted.VaR.ES,
     file = 'output/WIG_forecasted_VaR_ES.RData')

save(BTC.forecasted.VaR.ES,
     file = 'output/BTC_forecasted_VaR_ES.RData')

save(GOLD.forecasted.VaR.ES,
     file = 'output/GOLD_forecasted_VaR_ES.RData')

# Remove lists from current session
rm(list = c('DAX.forecasted.VaR.ES',
            'WIG.forecasted.VaR.ES',
            'BTC.forecasted.VaR.ES',
            'GOLD.forecasted.VaR.ES'))



