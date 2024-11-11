#########################################################################
### Returns list of price and return plots for all defined indices   ####
#########################################################################

price_return_plots_func <- function(index){
  
  # Assign data
  data <- get(index)
  
  # Initiate empty list for plots for each index
  index.price.return.plots <- list()
  
  # Price plot
  index.price.return.plots[['Price']] <- ggplot(data,
                                                aes(x = Date,
                                                    y = Price)) +
    geom_line(na.rm = TRUE) +
    labs(title = paste0(index)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Return plot
  index.price.return.plots[['Return']] <- ggplot(data,
                                                 aes(x = Date,
                                                     y = Return)) +
    geom_line(na.rm = TRUE) +
    labs(title = paste0(index)) +
    theme(plot.title = element_text(hjust = 0.5))

  return(index.price.return.plots)
}


#########################################################################
### Returns most important statistics for financial time series data ####
#########################################################################

ts_main_statistics <- function(index, lags_Ljung_Box_test = 15, lags_ArchTest = 12, nu = 5) {
  # Required libraries
  library(FinTS)
  library(moments)
  library(tseries)
  library(rugarch)
  
  # Assign data
  data <- get(index)
  
  # Retrieve Returns
  data <- data[['Return']]
  
  #Omit nas
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
  
  # Density plot compared with normal and t distribution
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
         title = paste0('Density Comparison: ', index)) +
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
    labs(title = paste0('QQ Plot: ', index),
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
         title = paste0("ACF Plot: ", index)) +
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
         title = paste0("PACF Plot: ", index)) +
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
         title = paste0("ACF Plot Squared: ", index)) +
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
         title = paste0("PACF Plot Squared: ", index)) +
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
         title = paste0("CCF Plot Leverage Effect: ", index)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(results)
}


############################################################################################
### Returns one-step-ahead VaR and ES forecast  (firstly convert input data to zoo object) ###
############################################################################################
predict_VaR_ES_1_ahead <- function(data,
                                   var.spec,
                                   mean.spec,
                                   dist.spec,
                                   tolerance_lvl,
                                   index_name,
                                   spec_i,
                                   dist_spec){
  
  # Test if data is zoo object and stop function if not
  if(!is.zoo(data)){
    data_class <- class(data)
    error_message <- paste0('Input is ',data_class, ' but needs to be zoo object!')
    stop(error_message)
  }
  
  # Store index of last obs
  index_last_obs <- tail(index(data), 1)
  
  # Function that returns NA for VaR and ES if both can't be calculated and writes message to console
  return_na_VaR_ES <- function(){
    VaR <- zoo(NA, index_last_obs)
    ES <- zoo(NA, index_last_obs)
    mu <- zoo(NA, index_last_obs)
    sigma <- zoo(NA, index_last_obs)
    skew <- zoo(NA, index_last_obs)
    shape <- zoo(NA, index_last_obs)
    dist.spec <- zoo(dist.spec, index_last_obs)
    
    VaR_and_ES <- list(VaR = VaR,
                       ES = ES,
                       mu = mu,
                       sigma = sigma,
                       skew = skew,
                       shape = shape,
                       dist = dist.spec)
    
    # Print date which can't be calculated
    date <- index(VaR_and_ES$VaR)
    message <- paste0('Preceeding date of date where VaR and ES cannot be calculated: ', date, '\n\n')
    cat(message)
    
    # Return VaR and ES
    return(VaR_and_ES)
  }
  
  # Specifying GARCH model
  spec <- ugarchspec(variance.model = var.spec,
                     mean.model = mean.spec,
                     distribution.model = dist.spec)
  
  # Fitting GARCH model
  # Return NA if fitting doesn't work
  fit <- tryCatch(
    {
      ugarchfit(spec = spec,
                data = data,
                solver = 'hybrid',
                solver.control = list(n.restarts = 5,                                     # Number of restarts for the global solver (gosolnp)
                                      
                                      # Global solver settings (gosolnp)
                                      global.solver.control = list(maxit = 50000,         # Maximum number of iterations
                                                                   xtol_rel = 1e-6,       # Tolerance for parameter changes
                                                                   ftol_rel = 1e-8        # Tolerance for function value changes (likelihood function)
                                                                   ),
                                      
                                      # Local solver settings (nloptr)
                                      local.solver = "nloptr",
                                      local.solver.control = list(maxeval = 50000,       # Maximum number of iterations
                                                                  xtol_rel = 1e-6,       # Tolerance for parameter changes
                                                                  ftol_rel = 1e-8        # Tolerance for function value changes (likelihood function)
                                                                  )
                                      )
                )
      },
    error = function(e){
      cat('\nSolvers did not work for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,'):\nError message: ', e$message, '\nNAs get returned for VaR and ES\n')
      
      return(NA)
    }
  )
  
  
  # Information regarding convergence (Exclude later again or store information in different way)
  #fit_convergence <- fit@fit$convergence
  #print(fit_convergence)
  #rm(fit_convergence)
  
  
  
  # If fit is NA than NAs get returned for VaR and ES forecast
  if(suppressWarnings(is.na(fit))){
    
    # Vector with NA information (global scope)
    new_entry_NA_fit <- tail(index(data), 1)
    names(new_entry_NA_fit) <- paste0(index_name, '_spec', spec_i, dist.spec)
    NA_fit <<- c(NA_fit, new_entry_NA_fit)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES())
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
      cat('\nForecasting did not work for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,'):\nError message: ', e$message, '\nNAs get returned for VaR and ES\n')
      
      return(NA)
    }
  )
  
  # If forecast is NA than NAs get returned for VaR and ES forecast   
  if(suppressWarnings(is.na(forecast))){
  
    # Vector with NA information (global scope)
    new_entry_NA_forecast <- tail(index(data), 1)
    names(new_entry_NA_forecast) <- paste0(index_name, '_spec', spec_i, dist.spec)
    NA_forecast <<- c(NA_forecast, new_entry_NA_forecast)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES())
  }
  
  # Extraction of predicted mean and standard deviation
  mu <- fitted(forecast)
  sigma <- sigma(forecast)
  
  # Writing message if mu can't be calculated and return NA for VaR and ES
  if(is.nan(mu)){
    # Console message
    cat('\nMu cannot be calculated for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,')\nNAs get returned for VaR and ES\n')
    
    # Vector with NA information (global scope)
    new_entry_NA_mu <- tail(index(data), 1)
    names(new_entry_NA_mu) <- paste0(index_name, '_spec', spec_i, dist.spec)
    NA_mu <<- c(NA_mu, new_entry_NA_mu)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES())
  }
      
  # Writing message if sigma can't be calculated and return NA for VaR and ES
  if(is.nan(sigma)){
    # Console message
    cat('\nSigma cannot be calculated for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,')\nNAs get returned for VaR and ES\n')
    
    # Vector with NA information (global scope)
    new_entry_NA_sigma <- tail(index(data), 1)
    names(new_entry_NA_sigma) <- paste0(index_name, '_spec', spec_i, dist.spec)
    NA_sigma <<- c(NA_sigma, new_entry_NA_sigma)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES())
  }
      
  # Using empirical innovations for VaR and ES forecast if dist_spec = empirical
  if(dist_spec == 'empirical'){
 
    # Standardized residuals
    resid_std <- residuals(fit, standardize = TRUE)
    
    # Storing empirical distribution of standardized innovations in global list with dates as names
    if(!exists('resid_std_emp_dist')){
      resid_std_emp_dist <<- list()
    }
    resid_std_emp_dist[[as.character(index_last_obs)]] <<- as.vector(resid_std)
    
    # pth quantile of residuals
    q_resid_std <- quantile(resid_std,
                            probs = tolerance_lvl,
                            na.rm = TRUE,
                            names = FALSE,
                            type = 8) # Approximately median unbiased regardless of distribution
    
    # Calculating VaR
    VaR <- mu + sigma * q_resid_std

    # Extract standardized residuals which exceed q_resid_std
    resid_ES <- resid_std[resid_std <= q_resid_std]
    
    # Mean of exceeded q_resid_std
    mean_resid_ES <- mean(resid_ES)
    
    # Remove name from mean_resid_ES
    names(mean_resid_ES) <- NULL
    
    # Calculating ES
    ES <- mu + sigma * mean_resid_ES

    # Change skew and shape with NAs to zoo object and insert empirical as distribution specification
    skew <- zoo(NA, index_last_obs)
    shape <- zoo(NA, index_last_obs)
    dist.spec <- zoo('empirical', index_last_obs)
    
    # Combine VaR and ES in one list
    VaR_and_ES <- list(VaR = VaR,
                       ES = ES,
                       mu = mu,
                       sigma = sigma,
                       skew = skew,
                       shape = shape,
                       dist = dist.spec)
    
    #Return results
    return(VaR_and_ES)
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
      cat('\nQuantile of tolerance level cannot be calculated (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,'):\nError message: ', e$message, '\nNAs get returned for VaR and ES\n')
      
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
    return(return_na_VaR_ES())
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
      cat('\nIntegration did not work for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,'):\nError message', e$message, '\nNA gets returned for ES\n')
      
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
    ES <- zoo(NA, index_last_obs)
    skew <- zoo(skew, index_last_obs)
    shape <- zoo(shape, index_last_obs)
    dist.spec <- zoo(dist.spec, index_last_obs)
    VaR_and_ES <- list(VaR = VaR,
                       ES = ES,
                       mu = mu,
                       sigma = sigma,
                       skew = skew,
                       shape = shape,
                       dist = dist.spec)
    
    # Print date which can't be calculated
    date <- index(VaR_and_ES$ES)
    message <- paste0('Preceeding date of date where ES cannot be calculated: ', date, '\n\n')
    cat(message)
    
    # Return VaR and ES
    return(VaR_and_ES)
  }
      
  # Calculation of ES
  ES <- mu + sigma / tolerance_lvl * integrated_value
  
  # Change skew and shape to zoo object
  skew <- zoo(skew, index_last_obs)
  shape <- zoo(shape, index_last_obs)
  dist.spec <- zoo(dist.spec, index_last_obs)
  
  # Combine VaR and ES in one list
  VaR_and_ES <- list(VaR = VaR,
                     ES = ES,
                     mu = mu,
                     sigma = sigma,
                     skew = skew,
                     shape = shape,
                     dist = dist.spec)
  
  #Return list with VaR and ES forecasts
  return(VaR_and_ES)
}


############################################################################################
###  Kupiec test: Unonditional Coverage VaR (if column name starts with Exceeded)        ###
############################################################################################
VaR_Kupiec_backtest <- function(data,
                                tolerance_lvl){
  
  # Preparing result vectors
  LR_all <- vector()
  p_all <- vector()
  
  for(col in names(data)){
    
    # Test if column consists of exceedences
    is_name_Exceeded <- substr(col, 1, 8)
    if(is_name_Exceeded == 'Exceeded'){
      
      # Using only non-missing entries
      col_data <- na.omit(data[[col]])
      
      # Calculating number of non-exceedences, exceedences and proportion of exceedences
      n1 <- sum(col_data)
      n0 <- length(col_data) - n1
      prop_exceeded <- n1 / (n1 + n0)
      
      # Likelihood Ratio Test
      # Likelihood of unrestricted (L_ur) and restricted (L_r) model
      L_ur <- prop_exceeded ^ n1 * (1 - prop_exceeded) ^ n0
      L_r <- tolerance_lvl ^ n1 * (1 - tolerance_lvl) ^ n0
      
      # Test statistic
      LR <- -2 * log(L_r / L_ur)
      
      # p value (LR ~ X^2(1))
      p <- pchisq(q = LR,
                  df = 1,
                  lower.tail = FALSE)

      # Appending result vectors and add name
      entryname <- substr(col, 14, nchar(col))
      
      LR_all[entryname] <- LR
      p_all[entryname] <- p
    }
  }
  
  #Return results
  results <- list(LR = LR_all,
                  p = p_all)
  return(results)
}


############################################################################################
###  Christofferson 1 test: Independence of (if column name starts with Exceeded)        ###
############################################################################################
VaR_Christofferson1_backtest <- function(data){
  
  # Preparing result vectors
  LR_all <- vector()
  p_all <- vector()
  
  for(col in names(data)){
    
    # Test if column consists of exceedences
    is_name_Exceeded <- substr(col, 1, 8)
    if(is_name_Exceeded == 'Exceeded'){
      
      # n_ij starts with 0 in for each column
      n00 <- 0
      n11 <- 0
      n10 <- 0
      n01 <- 0
      
      # Using only non-missing entries
      col_data <- na.omit(data[[col]])
      
      # Assign n_ij (n_ij = 1 with i in {0,1} and j in {0,1})
      for(i in 2:length(col_data)){
        if(col_data[i - 1] == 0 & col_data[i] == 0){
          n00 <- n00 + 1
        }
        else if(col_data[i - 1] == 1 & col_data[i] == 1){
          n11 <- n11 + 1
        }
        else if(col_data[i - 1] == 1 & col_data[i] == 0){
          n10 <- n10 + 1
        }
        else if(col_data[i - 1] == 0 & col_data[i] == 1){
          n01 <- n01 + 1
        }
        else {
          stop('Error: Values out of domain {0, 1}')
        }
      }
      
      # Assigning proportion of exceedences if previous observation exceeded / didnt exceed
      prop_exceeded <- (n01 + n11) / (n00 + n11 + n10 + n01)
      prop_exceeded11 <- n11 / (n10 + n11)
      prop_exceeded01 <- n01 / (n00 + n01)
      
      # Likelihood Ratio Test
      # Likelihood of restricted (L_r) and unrestricted (L_ur) model
      L_r <- prop_exceeded ^ (n01 + n11) * (1 - prop_exceeded) ^ (n00 + n10)
      L_ur <- prop_exceeded01 ^ n01 * (1 - prop_exceeded01) ^ n00 * prop_exceeded11 ^ n11 * (1 - prop_exceeded11) ^ n10
      
      # Test statistic
      LR <- -2 * log(L_r / L_ur)
      
      # p value (LR ~ X^2(1))
      p <- pchisq(q = LR,
                  df = 1,
                  lower.tail = FALSE)
      
      # Appending result vectors and add name
      entryname <- substr(col, 14, nchar(col))
      
      LR_all[entryname] <- LR
      p_all[entryname] <- p
    }
  }

  #Return results
  results <- list(LR = LR_all,
                  p = p_all)
  return(results)
}

############################################################################################
### Christofferson 2 test: Conditional Coverage VaR (if column name starts with Exceeded) ##
############################################################################################
VaR_Christofferson2_backtest <- function(data,
                                         tolerance_lvl){
  # Preparing result vectors
  LR_all <- vector()
  p_all <- vector()
  
  for(col in names(data)){
    
    # Test if column consists of exceedences
    is_name_Exceeded <- substr(col, 1, 8)
    if(is_name_Exceeded == 'Exceeded'){
      
      # n_ij starts with 0 in for each column
      n00 <- 0
      n11 <- 0
      n10 <- 0
      n01 <- 0
      
      # Using only non-missing entries
      col_data <- na.omit(data[[col]])
      
      # Assign n_ij (n_ij = 1 with i in {0,1} and j in {0,1})
      for(i in 2:length(col_data)){
        if(col_data[i - 1] == 0 & col_data[i] == 0){
          n00 <- n00 + 1
        }
        else if(col_data[i - 1] == 1 & col_data[i] == 1){
          n11 <- n11 + 1
        }
        else if(col_data[i - 1] == 1 & col_data[i] == 0){
          n10 <- n10 + 1
        }
        else if(col_data[i - 1] == 0 & col_data[i] == 1){
          n01 <- n01 + 1
        }
        else {
          stop('Error: Values out of domain {0, 1}')
        }
      }
      
      # Assigning proportion of exceedences if previous observation exceeded / didnt exceed
      prop_exceeded01 <- n01 / (n00 + n01)
      prop_exceeded11 <- n11 / (n10 + n11)
      
      # Likelihood Ratio Test
      # Likelihood of restricted (L_r) and unrestricted (L_ur) model
      L_r <- tolerance_lvl ^ (n01 + n11) * (1 - tolerance_lvl) ^ (n00 + n10)
      L_ur <- prop_exceeded01 ^ n01 * (1 - prop_exceeded01) ^ n00 * prop_exceeded11 ^ n11 * (1 - prop_exceeded11) ^ n10
      
      # Test statistic
      LR <- -2 * log(L_r / L_ur)
      
      # p value (LR ~ X^2(2))
      p <- pchisq(q = LR,
                  df = 2,
                  lower.tail = FALSE)
      
      # Appending result vectors and add name
      entryname <- substr(col, 14, nchar(col))
      
      LR_all[entryname] <- LR
      p_all[entryname] <- p
    }
  }

  #Return results
  results <- list(LR = LR_all,
                  p = p_all)
  return(results)
}


############################################################################################
### Unconditional coverage test ES without adjustment for parameter estimation risk      ###
############################################################################################
ES_uc_backtest <- function(data,
                           tolerance_lvl){
  
  # Preparing result vectors
  U_all <- vector()
  p_all <- vector()
  
  for(col in names(data)){
    
    # Test if column consists of cumulative violations
    is_CumVio <- substr(col, 1, 6)
    if(is_CumVio == 'CumVio'){
      
      # Using only non-missing entries
      H_hut <- na.omit(data[[col]])
      
      # Mean of H_huts
      H_mean <- mean(H_hut)
      
      # Number of non-missing entries
      n <- length(H_hut)
      
      # Calculating test statistic
      U <- sqrt(n) * (H_mean - tolerance_lvl / 2) / sqrt(tolerance_lvl * (1 / 3 - tolerance_lvl / 4))
      
      # Calculating p value
      p <- 2 * pnorm(q = abs(U),
                     lower.tail = FALSE)
        
      # Appending result vectors and add name
      entryname <- substr(col, 8, nchar(col))
      
      U_all[entryname] <- U
      p_all[entryname] <- p
    }
  }
  
  # Return results
  result <- list(U = U_all,
                 p = p_all)
  return(result)
}

############################################################################################
### Conditional coverage test ES without adjustment for parameter estimation risk        ###
############################################################################################
ES_cc_backtest <- function(data,
                           tolerance_lvl,
                           lags){
  # Preparing result vectors
  C_all <- vector()
  p_all <- vector()
  
  for(col in names(data)){
    
    # Test if column consists of cumulative violations
    is_CumVio <- substr(col, 1, 6)
    if(is_CumVio == 'CumVio'){
      
      # Using only non-missing entries
      H_hut <- na.omit(data[[col]])
      
      # Number of entries
      n <- length(H_hut)
      
      # Variance of H_huts
      gamma_n0 <- 1 / n * sum((H_hut - tolerance_lvl / 2) * (H_hut - tolerance_lvl / 2))
      
      # Vector with covariance between H_hut and j-laged H_hut
      gamma_nj <- vector(length = lags)
      
      for(j in 1:lags){
        gamma_nj[j] <- 1 / (n - j) * sum((H_hut[(j+1):n] - tolerance_lvl / 2) * (H_hut[1:(n-j)] - tolerance_lvl / 2))
      }
      
      # Vector with correlations between H_hut and j-laged H_hut
      rho_nj <- gamma_nj / gamma_n0
      
      # Test statistic for Box-Pierce-Test
      C <- n * sum(rho_nj ^ 2)
      
      # p-value of test statistic
      df <- length(rho_nj)
      
      p <- pchisq(q = C,
                  df = df,
                  lower.tail = FALSE)
      
      # Appending result vectors and add name
      entryname <- substr(col, 8, nchar(col))
      
      C_all[entryname] <- C
      p_all[entryname] <- p
    }
  }
  
  #Return results
  results <- list(C = C_all,
                  p = p_all)
  return(results)
}
