#################################################################################################################
#### Predicting one step ahead VaR and ES across different variance specifications and distribution assumptions #
#################################################################################################################

# Explanation:
#  Function calculates forecasts for all data sets across all variance specifications and distribution assumptions and stores it in csv file in output folder joined with price and return data
#  Takes multiple hours to run without sub-setting!!!!

# Sub-setting for faster testing

# Subsets data for faster step wise forecasting if data_include is specified
if(exists('data_include')){
  DAX <- DAX[data_include,]
  WIG <- WIG[data_include,]
  BTC <- BTC[data_include,]
  GOLD <- GOLD[data_include,]
}

# Subsets indices for faster step wise forecasting if index_include specified
if(exists('index_include')){
  indices <- indices[index_include]
}

# Subsets variance specifications for faster step wise forecasting if varspec_include specified
if(exists('varspec_include')){
  var.spec.list <- var.spec.list[varspec_include]
}

# Subsets distribution assumptions for faster step wise forecasting if dist_include specified
if(exists('dist_include')){
  dist.spec.list <- dist.spec.list[dist_include]
}


# Definition of function
execution_of_VaR_ES_forecasting_function <- function(){
  
  
  # Loop through indices
  for(index in indices){
    
    # Extracting returns and dates
    index_name <- index
    data <- get(index)
    data_returns <- data$Return
    data_dates <- data$Date
    
    # Storing return data as zoo object
    returns <- zoo(data_returns, data_dates)
    
    # Omitting NAs (first entry in returns is NA)
    returns <- na.omit(returns)
    
    # Creating empty list for forecasted VaR and ES values
    forecasted.VaR.list <- list()
    forecasted.ES.list <- list()
    
    # Loop over variance specifications
    for (var.spec in names(var.spec.list)) {
      
      # Extracting number of specification
      spec_i <- substr(var.spec, 5, nchar(var.spec))
      
      # Specification list entry
      var.spec <- var.spec.list[[var.spec]]
      
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
                                                                             tolerance_lvl = tolerance_lvl,
                                                                             index_name = index_name,
                                                                             spec_i = spec_i),
                                    align = 'right',
                                    coredata = FALSE)
        # !!!TESTING!!! VaR and ES of last observation (delete in final version)!!!!!!!!!
        #test_VaR_ES <- rolling.VaR.ES
        
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
    for(col_name in names(forecasted.VaR.list)){
      forecasted.VaR.list[[col_name]] <- na.approx(forecasted.VaR.list[[col_name]],
                                                   na.rm = FALSE)
    }
    
    for(col_name in names(forecasted.ES.list)){
      forecasted.ES.list[[col_name]] <- na.approx(forecasted.ES.list[[col_name]],
                                                  na.rm = FALSE)
    }
    
    # Leading forecasted.VaR.list and forecasted.ES.list
    for(col_name in names(forecasted.VaR.list)){
      forecasted.VaR.list[[col_name]] <- stats::lag(x = forecasted.VaR.list[[col_name]],
                                                    k = -1)
    }
    
    for(col_name in names(forecasted.ES.list)){
      forecasted.ES.list[[col_name]] <- stats::lag(x = forecasted.ES.list[[col_name]],
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
    
    # Assign exceedence flag for VaR: 0 -> no exceedence, 1 -> exceedence
    for(colname_test in names(data)){
      is_col_VaR <- substr(colname_test, 1, 3)
      if(is_col_VaR == 'VaR'){
        colname_result <- paste0('Exceeded_', colname_test)
        data[[colname_result]] <- ifelse(data$Return < data[[colname_test]], 1, 0)
      }
    }
    
    
    # Saving results in csv file with corresponding name
    write.csv(x = data,
              file = paste0('output/', index_name, '_with_forecasted_VaR_ES.csv'),
              row.names = FALSE)
  }
  #return(test_VaR_ES) # !!!TESTING!!! remove in final version
}


################################################################################################
### Execution of VaR ES forecasting if execution_of_VaR_ES_forecasting == TRUE, else loading ###
################################################################################################

if(execution_of_VaR_ES_forecasting){
  # Empty vectors for NA information (NA in VaR and ES)
  NA_fit <<- vector()
  NA_forecast <<- vector()
  NA_mu <<- vector()
  NA_sigma <<- vector()
  NA_q_tolerance_lvl <<- vector()
  
  # Empty vectors for NA information (NA in  # Empty vectors for NA information (NA in VaR and ES)ES)
  NA_integrated_value <<- vector()
  
  # Execution of VaR ES forecasting
  execution_of_VaR_ES_forecasting_function()
  
  # Loading forecasted VaR and ES
  for(index in indices){
    index_name <- index
    data <- read.csv(paste0('output/', index_name, '_with_forecasted_VaR_ES.csv'))
    
    # Converting Date column to date value
    data <- data %>%
      mutate(Date = as.Date(Date))
    
    assign(index_name, data)
    rm(index_name, data)
  }
  
  # Removing unnessecary onbjects
  rm(index, index_name, data)
  
  # Storing NA information in list and deleting individual vectors
  NA_information <- list(fit = NA_fit,
                         forecast = NA_forecast,
                         mu = NA_mu,
                         sigma = NA_sigma,
                         q_tolerance_lvl = NA_q_tolerance_lvl,
                         integrated_value = NA_integrated_value)
  rm(NA_fit, NA_forecast, NA_mu, NA_sigma, NA_q_tolerance_lvl, NA_integrated_value)
  
  # Assign proper date to integer value
  for(NA_info in names(NA_information)){
    NA_information[[NA_info]] <- as.Date('1970-01-01') + NA_information[[NA_info]]
  }
  
  # Saving NA information list
  saveRDS(NA_information, file='output/NA_information.RData')
  
} else{
  
  # Loading forecasted VaR and ES
  for(index in indices){
    index_name <- index
    data <- read.csv(paste0('output/', index_name, '_with_forecasted_VaR_ES.csv'))
    
    # Converting Date column to date value
    data <- data %>%
      mutate(Date = as.Date(Date))
    
    assign(index_name, data)
    rm(index_name, data)
  }
  
  # Loading NA information list
  NA_information <- readRDS('output/NA_information.RData')
}





