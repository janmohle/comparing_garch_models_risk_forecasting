#################################################################################################################
#### Predicting one step ahead VaR and ES across different variance specifications and distribution assumptions #
#################################################################################################################

# Explanation:
#  Script calculates forecasts for all data sets across all variance specifications and distribution assumptions and stores it in csv file in output folder with price and return data of index
#  Takes multiple hours to run with full data set!!!!


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
                        #distr8 = 'nig', # nig leads to problems (a lot of NaN in spec 3 for GOLD -> Should I exclude it)
                        #distr9 = 'ghst',  # optimazation takes often too long with this distribution -> Sould I exclude it?
                        distr10 = 'jsu') # jsu leads to some problems (NaN in sigma in Spec 3 for GOLD -> Should I include it or not?)


# Specifying estimation window width
width = 500

execution_of_VaR_ES_forecasting <- function(){
  
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
        
        # Console message
        cat('Current iteration: ',' Index: ',index_name, ' Spec: ', spec_i, ' Dist: ', dist.spec, '\n')
        
        # Rolling window VaR and ES forecasting
        rolling.VaR.ES <- rollapply(returns,
                                    width = width,
                                    FUN = function(x) predict_VaR_ES_1_ahead(data = x,
                                                                             var.spec = var.spec,
                                                                             mean.spec = armamean,
                                                                             dist.spec = dist.spec,
                                                                             tolerance_lvl = 0.05,
                                                                             index_name = index_name,
                                                                             spec_i = spec_i,
                                                                             dist = dist.spec),
                                    align = 'right')
        # !!!!!!!!Test VaR and ES of last observation (delete in final version)!!!!!!!!!
        test_VaR_ES <- rolling.VaR.ES
        
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
        entryname <- paste0('VaR_spec', spec_i, dist.spec)
        forecasted.VaR.list[[entryname]] <- rolling.VaR.zoo
        
        entryname <- paste0('ES_spec', spec_i, dist.spec)
        forecasted.ES.list[[entryname]] <- rolling.ES.zoo
        
        # End time of loop
        end <- Sys.time()
        
        # Print time iteration needed to run
        time <- as.numeric(end - start, units = 'mins')
        cat('Iteration needed ', time, ' minutes to run\n')
      }
    }
    
    # Linear interpolation of NAs
    for(i in 1:length(forecasted.VaR.list)){
      forecasted.VaR.list[[i]] <- na.approx(forecasted.VaR.list[[i]])
    }
    
    for(i in 1:length(forecasted.ES.list)){
      forecasted.ES.list[[i]] <- na.approx(forecasted.ES.list[[i]])
    }
    
    # Leading forecasted.VaR.list and forecasted.ES.list
    for(i in 1:length(forecasted.VaR.list)){
      forecasted.VaR.list[[i]] <- stats::lag(x = forecasted.VaR.list[[i]],
                                             k = -1)
    }
    
    for(i in 1:length(forecasted.ES.list)){
      forecasted.ES.list[[i]] <- stats::lag(x = forecasted.ES.list[[i]],
                                            k = -1)
    }
    
    # Creating data frame for VaR and ES
    forecasted.VaR.data.frame <- as.data.frame(forecasted.VaR.list)
    forecasted.VaR.data.frame[['Date']] <- as.Date(rownames(forecasted.VaR.data.frame))
    
    forecasted.ES.data.frame <- as.data.frame(forecasted.ES.list)
    forecasted.ES.data.frame[['Date']] <- as.Date(rownames(forecasted.ES.data.frame))
    
    # Joining VaR and ES data frame to return data frame
    data <- data %>%
      left_join(forecasted.VaR.data.frame, by = 'Date') %>%
      left_join(forecasted.ES.data.frame, by = 'Date')
    
    # Assign exceedence flag: 0 -> no exceedence, 1 -> exceedence
    for(i in 4:ncol(data)){
      colname_test <- colnames(data[i])
      colname_result <- paste0('Exceeded_', colname_test)
      
      data[[colname_result]] <- ifelse(data$Return < data[[colname_test]], 1, 0)
    }
    
    # Saving results in csv file with corresponding name
    write.csv(x = data,
              file = paste0('output/', index_name, '_with_forecasted_VaR_ES.csv'),
              row.names = FALSE)
  }
  return(test_VaR_ES) # !!!!!!!!!!!!! Exclude in final version !!!!!!!!!!!!
}





