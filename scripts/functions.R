#########################################################################
### Returns most important statistics for financial time series data ####
#########################################################################

ts_main_statistics <- function(data, lags_Ljung_Box_test = 15, lags_ArchTest = 12, nu = 5) {
  # Required libraries
  library(FinTS)
  library(moments)
  library(tseries)
  library(rugarch)
  
  #omit na
  data <- na.omit(data)
  
  # Initialize results list
  results <- list()
  
  # Calculate mean
  results$mean <- mean(data)
  
  # Calculate standard deviation
  results$sd <- sd(data)
  
  # Calculate quantiles (0, 0.25, 0.5, 0.75, 1)
  results$quantiles <- quantile(data)
  
  # Calculate skewness
  results$skewness <- skewness(data)
  
  # Calculate kurtosis
  results$kurtosis <- kurtosis(data)
  
  # Perform Ljung-Box test for serial correlation
  results$Ljung_Box_test <- stats::Box.test(data, lag = lags_Ljung_Box_test, type = 'Ljung-Box')
  
  # Perform ARCH test for conditional heteroskedasticity
  results$ArchTest <- FinTS::ArchTest(data, lags = lags_ArchTest)
  
  # Standerdize data
  data_standerdized <- (data - results$mean) / results$sd
  
  # Perform Jaque Bera test for normality
  results$JB_test <- tseries::jarque.bera.test(data_standerdized)
  
  # density plot compared with normal and t distribution
  density_data <- data.frame(x = data_standerdized)
  results$density <- ggplot(density_data,
                            aes(x = x)) +
    geom_density() +
    geom_function(fun = function(x) ddist(y = x),
                  aes(color = "Standard Normal")) +
    geom_function(fun = function(x) ddist(y = x,
                                          distribution = 'std',
                                          shape = nu),
                  aes(color = "t-Distribution")) +
    scale_color_manual(values = c("darkgreen", "purple")) +
    labs(color = "Distributions",
         title = 'Density Comparison') +
    theme(plot.title = element_text(hjust = 0.5))
  
  # QQ Plot (normal)
  quantiles <- data.frame(q_emp = data_standerdized,
                          p = 1 / length(data_standerdized))
  quantiles <- quantiles %>%
    arrange(q_emp) %>%
    mutate(p = cumsum(p)) %>%
    slice(1:(n() - 1)) %>%
    mutate(q_theo = qnorm(p))
  
  results$qqplot <- ggplot(quantiles,
                           aes(x = q_theo,
                               y = q_emp)) +
    geom_point() +
    geom_function(fun = function(x) x,
                  col = 'red') +
    labs(title = 'QQ Plot',
         x = 'Theoretical Quantile',
         y = 'Empirical Quantile') +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Significance threshold for ACFs and PACFs
  significance_threshold <- qnorm(0.975) / sqrt(length(data))
  
  # ACF for data
  acf_values <- acf(data, plot = F)
  acf_values <- data.frame(lag = acf_values$lag[2:20],
                           acf = acf_values$acf[2:20])
  results$acf <- ggplot(acf_values,
                        aes(x = lag,
                            y = acf)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(- significance_threshold, significance_threshold),
               linetype = "dashed",
               color = "red") +
    labs(x = "Lag",
         y = "ACF",
         title = "ACF Plot") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # PACF for data
  pacf_values <- pacf(data, plot = F)
  pacf_values <- data.frame(lag = pacf_values$lag[1:20],
                            pacf = pacf_values$acf[1:20])
  results$pacf <- ggplot(pacf_values,
                         aes(x = lag,
                             y = pacf)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(- significance_threshold, significance_threshold),
               linetype = "dashed",
               color = "red") +
    labs(x = "Lag",
         y = "PACF",
         title = "PACF Plot") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # ACF for squared data
  acf_values <- acf(data^2, plot = F)
  acf_values <- data.frame(lag = acf_values$lag[2:20],
                           acf = acf_values$acf[2:20])
  results$acf_squared <- ggplot(acf_values,
                                aes(x = lag,
                                    y = acf)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(- significance_threshold, significance_threshold),
               linetype = "dashed",
               color = "red") +
    labs(x = "Lag",
         y = "ACF",
         title = "ACF Plot Squared") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # PACF for squared data
  pacf_values <- pacf(data^2, plot = F)
  pacf_values <- data.frame(lag = pacf_values$lag[1:20],
                            pacf = pacf_values$acf[1:20])
  results$pacf_squared <- ggplot(pacf_values,
                         aes(x = lag,
                             y = pacf)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(- significance_threshold, significance_threshold),
               linetype = "dashed",
               color = "red") +
    labs(x = "Lag",
         y = "PACF",
         title = "PACF Plot Squared") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # CCF for data and squared data
  ccf_values <- ccf(data^2, data, plot = F)
  lower <- ceiling(length(ccf_values$lag) / 2) 
  upper <- lower + 20
  ccf_values <- data.frame(lag = ccf_values$lag[lower:upper],
                           ccf = ccf_values$acf[lower:upper])
  results$ccf <- ggplot(ccf_values,
                        aes(x = lag,
                            y = ccf)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(- significance_threshold, significance_threshold),
               linetype = "dashed",
               color = "red") +
    labs(x = "Lag",
         y = "CCF",
         title = "CCF Plot Leverage Effect") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(results)
}


############################################################################################
### Returns one-step-ahead VaR and ES forecast  (convert input data to zoo object first) ###
############################################################################################
predict_VaR_ES_1_ahead <- function(data,
                                   var.spec,
                                   mean.spec,
                                   dist.spec,
                                   tolerance_lvl,
                                   index_name,
                                   spec_i){
  
  # Test if data is zoo object and stop function if not
  if(!is.zoo(data)){
    data_class <- class(data)
    error_message <- paste0('Input is ',data_class, ' but needs to be zoo object!')
    stop(error_message)
  }
  
  # Function that returns NA for VaR and ES if both can't be calculated and writes message to console
  return_na_VaR_ES <- function(data){
    index.NA <- tail(index(data), 1)
    VaR <- zoo(NA, index.NA)
    ES <- zoo(NA, index.NA)
    VaR_and_ES <- list(VaR = VaR,
                       ES = ES)
    
    # Print date which can't be calculated and will later be interpolated
    date <- index(VaR_and_ES$VaR)
    message <- paste0('Preceeding date of date where VaR and ES get interpolated: ', date, '\n\n')
    cat(message)
    
    # Return VaR and ES
    return(VaR_and_ES)
  }
  
  # Specifying GARCH model
  spec <- ugarchspec(variance.model = var.spec,
                     mean.model = mean.spec,
                     distribution.model = dist.spec)
  
  # Fitting GARCH model (hybrid automatically tries different solver in order: solnp, nlminb, gosolnp, nloptr):
  # Return NA if fitting doesn't work
  fit <- tryCatch(
    {
      ugarchfit(spec = spec,
                data = data,
                solver = 'hybrid')
    },
    error = function(e){
      cat('\nSolvers did not work for one observation (Index: ', index_name, ' Spec: ', spec_i, ', Dist: ', dist.spec,'):', e$message, '\nNA gets returned for VaR and ES\n\n')
      
      return(NA)
    }
  )
    
  # If fit is NA than NAs get returned for VaR and ES forecast
  if(is.na(fit)) {
    
    # Vector with NA information (global scope)
    new_entry_NA_fit <- tail(index(data), 1)
    names(new_entry_NA_fit) <- paste0(index_name, '_spec', spec_i, dist.spec)
    NA_fit <<- c(NA_fit, new_entry_NA_fit)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES(data = data))
  }
  
  # Extracting coefficients from fit
  coef_fit <- coef(fit)
    
  # Extracting skewness and shape parameter (if not prevalent, NA gets assigned)
  skew <- ifelse('skew' %in% names(coef_fit), coef_fit['skew'], NA)
  shape <- ifelse('shape' %in% names(coef_fit), coef_fit['shape'], NA)

  # One-step-ahead forecast
  # Return NA if forecasting doesn't work
  forecast <- tryCatch(
    {
      ugarchforecast(fitORspec = fit,
                     n.ahead = 1)
    },
    error = function(e){
      cat('\nForecasting did not work for one observation (Index: ', index_name, ' Spec: ', spec_i, ', Dist: ', dist.spec,'):', e$message, '\nNA gets returned for VaR and ES\n\n')
      
      return(NA)
    }
  )
   
  # If forecast is NA than NAs get returned for VaR and ES forecast   
  if(is.na(forecast)){
    
    # Vector with NA information (global scope)
    new_entry_NA_forecast <- tail(index(data), 1)
    names(new_entry_NA_forecast) <- paste0(index_name, '_spec', spec_i, dist.spec)
    NA_forecast <<- c(NA_forecast, new_entry_NA_forecast)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES(data = data))
  }
  
  # Extraction of predicted mean and standard deviation
  mu <- fitted(forecast)
  sigma <- sigma(forecast)
  
  # Writing message if mu can't be calculated and return NA for VaR and ES
  if(is.nan(mu)){
    # Console message
    cat('\nMu cannot be calculated for one observation (Index: ', index_name, ' Spec: ', spec_i, ', Dist: ', dist.spec,')\nNA gets returned for VaR and ES\n\n')
    
    # Vector with NA information (global scope)
    new_entry_NA_mu <- tail(index(data), 1)
    names(new_entry_NA_mu) <- paste0(index_name, '_spec', spec_i, dist.spec)
    NA_mu <<- c(NA_mu, new_entry_NA_mu)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES(data = data))
  }
      
  # Writing message if sigma can't be calculated and return NA for VaR and ES
  if(is.nan(sigma)){
    # Console message
    cat('\nSigma cannot be calculated for one observation (Index: ', index_name, ' Spec: ', spec_i, ', Dist: ', dist.spec,')\nNA gets returned for VaR and ES\n\n')
    
    # Vector with NA information (global scope)
    new_entry_NA_sigma <- tail(index(data), 1)
    names(new_entry_NA_sigma) <- paste0(index_name, '_spec', spec_i, dist.spec)
    NA_sigma <<- c(NA_sigma, new_entry_NA_sigma)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES(data = data))
  }
      
  # Function returns pth quantile of current distribution
  pth_quantile <- function(p) {
    q <- qdist(distribution = dist.spec,
               p = p,
               skew = skew,
               shape = shape)
    return(q)
  }
  
  # Calculation of quantile of tolerance level
  # Return NA if calculation doesn't work
  q_tolerance_lvl <- tryCatch(
    {
      pth_quantile(p = tolerance_lvl)
    }, error = function(e){
      cat('\nQuantile of tolerance level cannot be calculated (Index: ', index_name, ' Spec: ', spec_i, ', Dist: ', dist.spec,'):', e$message, '\nNA gets returned for VaR and ES\n\n')
      
      return(NA)
    }
  )

  # If quantile of tolerance level is NA than NAs get returned for VaR and ES forecast  
  if(is.na(q_tolerance_lvl)){
    
    # Vector with NA information (global scope)
    new_entry_NA_q_tolerance_lvl <- tail(index(data), 1)
    names(new_entry_NA_q_tolerance_lvl) <- paste0(index_name, '_spec', spec_i, dist.spec)
    NA_q_tolerance_lvl <<- c(NA_q_tolerance_lvl, new_entry_NA_q_tolerance_lvl)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES(data = data))
  }
  
  # Calculation of one-step-ahead VaR forecast
  VaR <- mu + sigma * q_tolerance_lvl
  
  # Integrate over inverse cdf: from 0 until tolerance level
  # Return NULL if integration doesn't work
  integrated_value <- tryCatch(
    {
      integrated <- integrate(pth_quantile,
                              lower = 0,
                              upper = tolerance_lvl)
      
      # Extract value of integration
      integrated$value
      
    }, error = function(e) {
      cat('\nIntegration did not work for one observation (Index: ', index_name, ' Spec: ', spec_i, ', Dist: ', dist.spec,'):', e$message, '\nNA gets returned for ES\n\n')
      
      return(NULL)
    }
  )
  
  # If integrated value is NULL than NA gets returned for ES forecast in combination with forecasted VaR
  if(is.null(integrated_value)){
    
    # Vector with NA information (global scope)
    new_entry_NA_integrated_value <- tail(index(data), 1)
    names(new_entry_NA_integrated_value) <- paste0(index_name, '_spec', spec_i, dist.spec)
    NA_integrated_value <<- c(NA_integrated_value, new_entry_NA_integrated_value)
    
    # Creating list with VaR and ES
    index.NA <- tail(index(data), 1)
    ES <- zoo(NA, index.NA)
    VaR_and_ES <- list(VaR = VaR,
                       ES = ES)
    
    # Print date which can't be calculated and will later be interpolated
    date <- index(VaR_and_ES$ES)
    message <- paste0('Preceeding date of date where ES gets interpolated: ', date, '\n\n')
    cat(message)
    
    # Return VaR and ES
    return(VaR_and_ES)
  }
      
  # Calculation of ES
  ES <- mu + sigma / tolerance_lvl * integrated_value
  
  # Combine VaR and ES in one list
  VaR_and_ES <- list(VaR = VaR,
                     ES = ES)
  
  #Return list with VaR and ES forecasts
  return(VaR_and_ES)
}


############################################################################################
###  Kupiec test: Unonditional Coverage VaR (if column name starts with Exceeded)        ###
############################################################################################
Kupiec_test <- function(data,
                        tolerance_lvl){
  # Initialize empty result list
  results <- list()
  
  for(column in names(data)){
    is_name_Exceeded <- substr(column, 1, 8)
    
    # Test if column is column where test can be deployed
    if(is_name_Exceeded == 'Exceeded'){
      column_data <- na.omit(data[[column]])
      
      # Calculating number of non-exceedences, exceedences and proportion of exceedences
      n1 <- sum(column_data)
      n0 <- length(column_data) - n1
      prop_exceeded <- n1 / (n1 + n0)
      
      # Likelihood Ratio Test
      L_ur <- prop_exceeded ^ n1 * (1 - prop_exceeded) ^ n0
      L_r <- tolerance_lvl ^ n1 * (1 - tolerance_lvl) ^ n0
      LR <- -2 * log(L_r / L_ur)
      p_value <- pchisq(q = LR,
                        df = 1,
                        lower.tail = FALSE)
      
      # Storing test results in list
      result <- list(p_value = p_value,
                     LR = LR)
      entry_name <- paste0('Kupiec_', substr(column, 10, nchar(column)))
      results[[entry_name]] <- result
    }
  }
  return(results)
}


############################################################################################
###  Christofferson 1 test: Independence of (if column name starts with Exceeded)        ###
############################################################################################
Christofferson1_test <- function(data){
  
  # Initialize empty result list
  results <- list()
  for(column in names(data)){
    
    is_name_Exceeded <- substr(column, 1, 8)
    
    # Test if column is column where test can be deployed
    
    if(is_name_Exceeded == 'Exceeded'){
      
      # n_ij starts with 0 in for each column
      n00 <- 0
      n11 <- 0
      n01 <- 0
      n10 <- 0
      
      # Omit NAs
      column_data <- na.omit(data[[column]])
      
      # Assign n_ij (n_ij = 1 with i in {0,1} and j in {0,1})
      for(i in 2:length(column_data)){
        if(column_data[i] == 0 & column_data[i - 1] == 0){
          n00 <- n00 + 1
        }
        else if(column_data[i] == 1 & column_data[i - 1] == 1){
          n11 <- n11 + 1
        }
        else if(column_data[i] == 0 & column_data[i - 1] == 1){
          n01 <- n01 + 1
        }
        else if(column_data[i] == 1 & column_data[i - 1] == 0){
          n10 <- n10 + 1
        }
        else {
          stop('Error: Values out of domain {0, 1}')
        }
      }
      
      # Assigning proportion of exceedences if previous observation exceeded / didnt exceed
      prop_exceeded <- (n01 + n11) / (n00 + n11 + n01 + n10)
      prop_exceeded0 <- n01 / (n00 + n01)
      prop_exceeded1 <- n11 / (n10 + n11)
      
      # Likelihood Ratio Test
      L_r <- prop_exceeded ^ (n01 + n11) * (1 - prop_exceeded) ^ (n00 + n10)
      L_ur <- prop_exceeded0 ^ n01 * (1 - prop_exceeded0) ^ n00 * prop_exceeded1 ^ n11 * (1 - prop_exceeded1) ^ n10
      LR <- -2 * log(L_r / L_ur)
      p_value <- pchisq(q = LR,
                        df = 1,
                        lower.tail = FALSE)
      
      # Storing test result in list
      result <- list(p_value = p_value,
                     LR = LR)
      entry_name <- paste0('Chr1_', substr(column, 10, nchar(column)))
      results[[entry_name]] <- result
    }
  }
  return(results)
}

############################################################################################
### Christofferson 2 test: Conditional Coverage VaR (if column name starts with Exceeded) ##
############################################################################################
Christofferson2_test <- function(data,
                                 tolerance_lvl){
  # Initialize empty result list
  results <- list()
  
  for(column in names(data)){
    is_name_Exceeded <- substr(column, 1, 8)
    
    # Test if column is column where test can be deployed
    if(is_name_Exceeded == 'Exceeded'){
      column_data <- na.omit(data[[column]])
      
      # n_ij starts with 0 in for each column
      n00 <- 0
      n11 <- 0
      n01 <- 0
      n10 <- 0
      
      # Assign n_ij (n_ij = 1 with i in {0,1} and j in {0,1})
      for(i in 2:length(column_data)){
        if(column_data[i] == 0 & column_data[i - 1] == 0){
          n00 <- n00 + 1
        }
        else if(column_data[i] == 1 & column_data[i - 1] == 1){
          n11 <- n11 + 1
        }
        else if(column_data[i] == 0 & column_data[i - 1] == 1){
          n01 <- n01 + 1
        }
        else if(column_data[i] == 1 & column_data[i - 1] == 0){
          n10 <- n10 + 1
        }
        else {
          stop('Error: Values out of domain {0, 1}')
        }
      }
      
      # Assigning proportion of exceedences if previous observation exceeded / didnt exceed
      prop_exceeded0 <- n01 / (n00 + n01)
      prop_exceeded1 <- n11 / (n10 + n11)
      
      # Likelihood Ratio Test
      L_r <- tolerance_lvl ^ (n01 + n11) * (1 - tolerance_lvl) ^ (n00 + n10)
      L_ur <- prop_exceeded0 ^ n01 * (1 - prop_exceeded0) ^ n00 * prop_exceeded1 ^ n11 * (1 - prop_exceeded1) ^ n10
      LR <- -2 * log(L_r / L_ur)
      
      # Calcutating p value (LR ~ X^2(2))
      p_value <- pchisq(q = LR,
                        df = 2,
                        lower.tail = FALSE)
      
      # Storing test result in list
      result <- list(p_value = p_value,
                     LR = LR)
      entry_name <- paste0('Chr2_', substr(column, 10, nchar(column)))
      results[[entry_name]] <- result
    }
  }
  return(results)
}
