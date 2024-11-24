#################################################################################################################
#### Predicting one step ahead VaR and ES across different variance specifications and distribution assumptions #
#################################################################################################################

# Explanation:
#  Function calculates forecasts for all data sets across all variance specifications and distribution assumptions and stores it in csv file in output folder joined with price and return data
#  Takes multiple hours to run without sub-setting!!!!


# Assign output folder name
if(simulation){
  output_folder <- 'simulated_output/'
} else {
  output_folder <- 'output/'
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
    
    # Creating empty list for forecasted VaR, ES, mu, sigma, skew, shape and dist
    forecasted.VaR.list <- list()
    forecasted.ES.list <- list()
    forecasted.mu.list <- list()
    forecasted.sigma.list <- list()
    forecasted.skew.list <- list()
    forecasted.shape.list <- list()
    forecasted.dist.list <- list()
    
    # Loop over variance specifications
    for (var_spec in names(var.spec.list)) {
      
      # Extracting number of specification
      spec_i <- substr(var_spec, 5, nchar(var_spec))
      
      # Specification list entry
      var.spec <- var.spec.list[[var_spec]]
      
      # Loop over distribution
      for (dist_spec in names(dist.spec.list)) {
        
        # Extracting distribution
        dist.spec <- dist.spec.list[[dist_spec]]
        
        # Start time of loop
        start <- Sys.time()
        
        # Console message
        if(dist_spec == 'empirical'){
          cat('Current iteration: ','Index:',index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist_spec, '\n')
        } else {
          cat('Current iteration: ','Index:',index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec, '\n')
        }
        
        
        # Rolling window VaR and ES forecasting
        rolling.VaR.ES <- rollapply(returns,
                                    width = window_width,
                                    FUN = function(x) predict_VaR_ES_1_ahead(data = x,
                                                                             var.spec = var.spec,
                                                                             mean.spec = mean.spec,
                                                                             dist.spec = dist.spec,
                                                                             tolerance_lvl = tolerance_lvl,
                                                                             index_name = index_name,
                                                                             spec_i = spec_i,
                                                                             dist_spec = dist_spec),
                                    align = 'right',
                                    coredata = FALSE)
        
        # Clear coef_prev_fit and num_window_shift for next run
        if(exists('coef_prev_fit')){
          rm(coef_prev_fit, envir = .GlobalEnv)
        }
        if(exists('num_window_shift')){
          rm(num_window_shift, envir = .GlobalEnv)
        }
        
        
        # !!!TESTING!!! VaR and ES of last observation (delete in final version)!!!!!!!!!
        #test_VaR_ES <- rolling.VaR.ES
        
        # Returning object after rollapply is difficult to handle, because predict_VaR_ES_1_ahead returns list in each iteration
        # Dates and values must be extracted and stored in a more handy way
        
        # Extract dates
        dates <- index(rolling.VaR.ES)
        
        # Creating data frame for easier extraction of values
        rolling.VaR.ES.df <- as.data.frame(rolling.VaR.ES)
        
        # Extracting VaR, ES, mu, sigma, skew, shape, dist values with loop and converting it to zoo object
        for(col in names(rolling.VaR.ES.df)){
          values <- vector()
          for(obs in rolling.VaR.ES.df[[col]]){
            value_of_obs <- as.vector(obs)
            values <- append(values, value_of_obs)
          }
          values_zoo <- zoo(values, dates)
          
          # Creating list with results
          forecasted_col_list_name <- paste0('forecasted.', col, '.list')
          forecasted_col_list <- get(forecasted_col_list_name)
          
          if(dist_spec == 'empirical'){
            entryname <- paste0(col, '_spec', spec_i,'_', dist_spec)
          } else {
            entryname <- paste0(col, '_spec', spec_i,'_', dist.spec)
          }
          forecasted_col_list[[entryname]] <- values_zoo
          
          assign(forecasted_col_list_name, forecasted_col_list)
        }
        
        # End time of loop
        end <- Sys.time()
        
        # Print time iteration needed to run
        time <- as.numeric(end - start, units = 'mins')
        cat('Iteration took', time, 'minutes to run\n')
      }
    }
    
    
    
    
    #!!!EXCLUDE?!!!
    # Linear interpolation of NAs 
    #for(col_name in names(forecasted.VaR.list)){
    # forecasted.VaR.list[[col_name]] <- na.approx(forecasted.VaR.list[[col_name]],
    #                                               na.rm = FALSE)
    #}
    
    #for(col_name in names(forecasted.ES.list)){
    # forecasted.ES.list[[col_name]] <- na.approx(forecasted.ES.list[[col_name]],
    #                                            na.rm = FALSE)
    #}
    #!!!!!!!!!
    
    # Leading lists with forecasted measurements
    for(measure in names(rolling.VaR.ES.df)){
      forecasted_measure_list_name <- paste0('forecasted.', measure, '.list')
      forecasted_measure_list <- get(forecasted_measure_list_name)
      
      for(col_name in names(forecasted_measure_list)){
        forecasted_measure_list[[col_name]] <- stats::lag(x = forecasted_measure_list[[col_name]],
                                                          k = -1)
      }
      assign(forecasted_measure_list_name, forecasted_measure_list)
    }
    
    # Creating data frame for VaR, ES, mu, sigma, skew, shape and dist
    for(measure in names(rolling.VaR.ES.df)){
      forecasted_measure_list_name <- paste0('forecasted.', measure, '.list')
      forecasted_measure_list <- get(forecasted_measure_list_name)
      
      forecasted_measure_data_frame <- as.data.frame(forecasted_measure_list)
      forecasted_measure_data_frame[['Date']] <- as.Date(rownames(forecasted_measure_data_frame))
      
      forecasted_measure_data_frame_name <- paste0('forecasted.', measure, '.data.frame')
      assign(forecasted_measure_data_frame_name, forecasted_measure_data_frame)
    }
    
    # Joining VaR, ES, mu, sigma, skew, shape and dist data frame to return data frame
    data <- data %>%
      left_join(forecasted.VaR.data.frame, by = 'Date') %>%
      left_join(forecasted.ES.data.frame, by = 'Date') %>%
      left_join(forecasted.mu.data.frame, by = 'Date') %>%
      left_join(forecasted.sigma.data.frame, by = 'Date') %>%
      left_join(forecasted.skew.data.frame, by = 'Date') %>%
      left_join(forecasted.shape.data.frame, by = 'Date') %>%
      left_join(forecasted.dist.data.frame, by = 'Date')
    
    # Assign exceedence flag for VaR: 0 -> no exceedence, 1 -> exceedence
    for(colname_test in names(data)){
      is_col_VaR <- substr(colname_test, 1, 3)
      if(is_col_VaR == 'VaR'){
        colname_result <- paste0('Exceeded_', colname_test)
        data[[colname_result]] <- ifelse(data$Return <= data[[colname_test]], 1, 0)
      }
    }
    
    # Computing cumulative exceedences for ES backtesting (values could vary slightly because of distribution parameter estimation errors due to approximation in numerical optimization!)
    dist.spec.names.vec <- unlist(dist.spec.list)
    var.spec.names.vec <- names(var.spec.list)
    
    # Loop over rows of data
    for(i in 1:nrow(data)){
      
      # Loop over variance specifications
      for(var in var.spec.names.vec){
        
        # Loop over distribution assumptions
        for(dist_name in names(dist.spec.names.vec)){
          
          if(dist_name == 'empirical'){
            dist <- dist_name
          } else {
            dist <- dist.spec.names.vec[[dist_name]]
          }
          
          # Current combination of var and dist
          var_dist <- paste0(var, '_', dist)
          
          # Assign name of currently needed column
          dist_var_dist <- paste0('dist_', var_dist)
          mu_var_dist <- paste0('mu_', var_dist)
          sigma_var_dist <- paste0('sigma_', var_dist)
          skew_var_dist <- paste0('skew_', var_dist)
          shape_var_dist <- paste0('shape_', var_dist)
          
          # Test that data is not NA, if so, assign NAs to u and CumVio
          Exceeded_VaR_var_dist <- paste0('Exceeded_VaR_', var_dist)
          
          if(is.na(data[[Exceeded_VaR_var_dist]][i])){
            
            u <- NA
            CumVio <- NA
            
          } else {
            
            # Compute value of cdf of standardized empirical innovation
            if(dist == 'empirical'){
              
              # Extracting current empirical distribution from list with all empirical distributions
              speci_empirical <- paste0('spec', spec_i, '_empirical')
              empirical_dist <- other.quantities[[index_name]][[speci_empirical]][['empirical_dist']][[(i - window_width - 1)]]
              
              # Calculating current empirical standardized innovation using prediction of mu and sigma with actual return
              return_i <- data[['Return']][i]
              mu_i <- data[[mu_var_dist]][i]
              sigma_i <- data[[sigma_var_dist]][i]
              innovation_i <- (return_i - mu_i) / sigma_i
              
              # Calculate value of cdf of innovation on empirical distribution
              u <- mean(empirical_dist <= innovation_i)
              
            } else {
              # Calculate value of cdf of innovation on theoretical distribution
              u <- pdist(distribution = data[[dist_var_dist]][i],
                         q = data[['Return']][i],
                         mu = data[[mu_var_dist]][i],
                         sigma = data[[sigma_var_dist]][i],
                         skew = data[[skew_var_dist]][i],
                         shape = data[[shape_var_dist]][i])
            }
            
            # Calculate cumulative violations
            if(u <= tolerance_lvl){
              
              CumVio <- 1 / tolerance_lvl * (tolerance_lvl - u)
              
            } else {
              
              CumVio <- 0
              
            }
          }
          
          # Assign u and CumVio to current column with correct name
          col_name_u <- paste0('u_', var_dist)
          data[[col_name_u]][i] <- u
          
          col_name_CumVio <- paste0('CumVio_', var_dist)
          data[[col_name_CumVio]][i] <- CumVio
        }
      }
    }
    
    # Saving results in csv file with corresponding name
    write.csv(x = data,
              file = paste0(output_folder, index_name, '_with_forecasted_VaR_ES.csv'),
              row.names = FALSE)
  }
  #return(test_VaR_ES) # !!!TESTING!!! remove in final version
}


################################################################################################
### Execution of VaR ES forecasting if execution_of_VaR_ES_forecasting == TRUE, else loading ###
################################################################################################

if(execution_of_VaR_ES_forecasting){
  
  # Execution of VaR ES forecasting
  execution_of_VaR_ES_forecasting_function()
  
  # Loading forecasted VaR and ES
  for(index_name in indices){
    data <- read.csv(paste0(output_folder, index_name, '_with_forecasted_VaR_ES.csv'))
    
    # Converting Date column to date value
    data <- data %>%
      mutate(Date = as.Date(Date))
    
    # Assign data frame to name and remove temporary data
    assign(index_name, data)
    rm(data)
    
    # Assign proper date to integer values in NA.information
    for(NA_info in names(NA.information[[index_name]])){
      NA.information[[index_name]][[NA_info]] <- as.Date('1970-01-01') + NA.information[[index_name]][[NA_info]]
    }
    rm(NA_info)
  }
  rm(index_name)
  
  # Save NA information list
  saveRDS(NA.information, file=paste0(output_folder, 'NA_information.RData'))
  
  # Save other.quantities list
  saveRDS(other.quantities, file=paste0(output_folder, 'other_quantities.RData'))
  
} else{
  
  # Loading forecasted VaR and ES
  for(index_name in indices){
    data <- read.csv(paste0(output_folder, index_name, '_with_forecasted_VaR_ES.csv'))
    
    # Converting Date column to date value
    data <- data %>%
      mutate(Date = as.Date(Date))
    
    # Assign data frame to name and remove temporary data
    assign(index_name, data)
    rm(data)
  }
  
  # Loading NA information list (this is list from last execution, so only NA information of data which was processed there is represented!)
  NA.information <- readRDS(paste0(output_folder, 'NA_information.RData'))
  
  # Loading other.quantities list (this is list from last execution, so only other quantities of data which was processed there are represented!)
  other.quantities <- readRDS(paste0(output_folder, 'other_quantities.RData'))
}

rm(output_folder)



