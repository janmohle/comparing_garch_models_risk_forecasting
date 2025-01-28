##################################################################################
### Function returns list of price and return plots for all defined indices   ####
##################################################################################

price_return_plots_func <- function(index){
  
  # Assign data
  data <- get(index)
  
  # Initiate empty list for plots for each index
  index.price.return.plots <- list()
  
  # Price plot
  index.price.return.plots[['Price']] <- ggplot(data,
                                                aes(x = Date,
                                                    y = Price)) +
    geom_line(na.rm = TRUE, size = 0.5) +
    labs(title = paste0('Price ',index)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))
  
  # Return plot
  index.price.return.plots[['Return']] <- ggplot(data,
                                                 aes(x = Date,
                                                     y = Return)) +
    geom_line(na.rm = TRUE, size = 0.5) +
    labs(title = paste0('Daily Log-Returns ',index)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))

  return(index.price.return.plots)
}


#########################################################################
### Returns important statistics for financial time series data      ####
#########################################################################

ts_main_statistics <- function(index,
                               lags_Ljung_Box_test,
                               lags_ArchTest,
                               nu) {
  
  # Assign data
  data <- get(index)
  
  # Retrieve Returns
  data <- data[['Return']]
  
  # Omit NAs
  data <- na.omit(data)
  
  # Initialize result list
  results <- list()
  
  # Calculate mean
  results[['mean']] <- mean(data)
  
  # Calculate standard deviation
  results[['sd']] <- sd(data)
  
  # Calculate quantiles (0, 0.25, 0.5, 0.75, 1)
  results[['quantiles']] <- quantile(data)
  
  # Calculate skewness
  results[['skewness']] <- skewness(data)
  
  # Calculate kurtosis
  results[['kurtosis']] <- kurtosis(data)
  
  # Perform Ljung-Box test for serial correlation
  results[['Ljung_Box_test']] <- stats::Box.test(data,
                                                 lag = lags_Ljung_Box_test,
                                                 type = 'Ljung-Box')
  
  # Perform ARCH test for conditional heteroskedasticity
  results[['ArchTest']] <- FinTS::ArchTest(data,
                                           lags = lags_ArchTest)
  
  # Standardize data
  data_standardized <- (data - results[['mean']]) / results[['sd']]
  
  # Perform Jaque Bera test of normality
  results[['JB_test']] <- tseries::jarque.bera.test(data_standardized)
  
  # Density plot compared with normal and t distribution
  density_data <- data.frame(x = data_standardized)
  results[['density']] <- ggplot(density_data,
                            aes(x = x)) +
    geom_density(aes(color = 'Standardized Returns')) +
    geom_function(fun = function(x) ddist(y = x),
                  aes(color = 'Normal Distribution')) +
    geom_function(fun = function(x) ddist(y = x,
                                          distribution = 'std',
                                          shape = nu),
                  aes(color = paste0('t-Distribution with ', nu, ' df'))) +
    scale_color_manual(
      values = c('black', 'darkgreen', 'purple'),
      breaks = c('Standardized Returns', 'Normal Distribution', paste0('t-Distribution with ', nu, ' df'))
    )  +
    labs(color = 'Distributions',
         linetype = 'Distributions',
         title = paste0('Density Comparison: ', index),
         x = NULL,
         y = NULL) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.1, 0.5),
          legend.justification = 'left',
          panel.background = element_rect(fill = 'white'),
          legend.background = element_rect(fill = 'white', color = NA),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))
  
  # Lower tail compared with normal and t distribution
  results$lower_tail <- ggplot(density_data,
                               aes(x = x)) +
    geom_density(aes(color = 'Standardized Returns')) +
    geom_function(fun = function(x) ddist(y = x),
                  aes(color = 'Normal Distribution')) +
    geom_function(fun = function(x) ddist(y = x,
                                          distribution = 'std',
                                          shape = nu),
                  aes(color = paste0('t-Distribution with ', nu, ' df'))) +
    scale_color_manual(
      values = c('black', 'darkgreen', 'purple'),
      breaks = c('Standardized Returns', 'Normal Distribution', paste0('t-Distribution with ', nu, ' df'))
    )  +
    labs(color = 'Distributions',
         linetype = 'Distributions',
         title = paste0('Lower Tail Comparison: ', index),
         x = NULL,
         y = NULL) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.1, 0.5),
          legend.justification = 'left',
          panel.background = element_rect(fill = 'white'),
          legend.background = element_rect(fill = 'white', color = NA),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black')) +
    coord_cartesian(xlim = c(-4.5, -1),
                    ylim = c(0, 0.3))
  
  # QQ Plot (normal)
  quantiles <- data.frame(q_emp = data_standardized,
                          p = 1 / length(data_standardized))
  quantiles <- quantiles %>%
    arrange(q_emp) %>%
    mutate(p = cumsum(p)) %>%
    slice(1:(n() - 1)) %>%
    mutate(q_theo = qnorm(p))
  
  results[['qqplot']] <- ggplot(quantiles,
                                aes(x = q_theo,
                                    y = q_emp)) +
    geom_point() +
    geom_function(fun = function(x) x,
                  col = 'red') +
    labs(title = paste0('QQ Plot: ', index),
         x = 'Theoretical Quantile Normal Distribution',
         y = 'Empirical Quantile') +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))
  
  # Significance threshold for ACFs and PACFs
  significance_threshold <- qnorm(0.975) / sqrt(length(data))
  
  # ACF plot
  acf_values <- acf(data,
                    plot = FALSE)
  
  acf_values <- data.frame(lag = acf_values$lag[2:21],
                           acf = acf_values$acf[2:21])
  results[['acf']] <- ggplot(acf_values,
                             aes(x = lag,
                                 y = acf)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(-significance_threshold, significance_threshold),
               linetype = 'dashed',
               color = 'red') +
    labs(x = 'Lags',
         y = 'ACF',
         title = paste0('ACF Plot: ', index)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))
  
  # PACF plot
  pacf_values <- pacf(data,
                      plot = FALSE)
  pacf_values <- data.frame(lag = pacf_values$lag[1:20],
                            pacf = pacf_values$acf[1:20])
  results[['pacf']] <- ggplot(pacf_values,
                              aes(x = lag,
                                  y = pacf)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(-significance_threshold, significance_threshold),
               linetype = 'dashed',
               color = 'red') +
    labs(x = 'Lags',
         y = 'PACF',
         title = paste0('PACF Plot: ', index)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))
  
  # ACF for squared data plot
  acf_values <- acf(data^2,
                    plot = FALSE)
  acf_values <- data.frame(lag = acf_values$lag[2:21],
                           acf = acf_values$acf[2:21])
  results[['acf_squared']] <- ggplot(acf_values,
                                     aes(x = lag,
                                         y = acf)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(-significance_threshold, significance_threshold),
               linetype = 'dashed',
               color = 'red') +
    labs(x = 'Lags',
         y = 'ACF',
         title = paste0('ACF Plot Squared: ', index)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))
  
  # PACF for squared data plot
  pacf_values <- pacf(data^2,
                      plot = FALSE)
  pacf_values <- data.frame(lag = pacf_values$lag[1:20],
                            pacf = pacf_values$acf[1:20])
  results[['pacf_squared']] <- ggplot(pacf_values,
                                      aes(x = lag,
                                          y = pacf)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(-significance_threshold, significance_threshold),
               linetype = 'dashed',
               color = 'red') +
    labs(x = 'Lags',
         y = 'PACF',
         title = paste0('PACF Plot Squared: ', index)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))
  
  # CCF for data and squared data plot
  ccf_values <- ccf(data^2, data,
                    plot = FALSE)
  lower <- ceiling(length(ccf_values$lag) / 2) + 1
  upper <- lower + 19
  ccf_values <- data.frame(lag = ccf_values$lag[lower:upper],
                           ccf = ccf_values$acf[lower:upper])
  results[['ccf']] <- ggplot(ccf_values,
                             aes(x = lag,
                                 y = ccf)) +
    geom_bar(stat = 'identity') +
    geom_hline(yintercept = c(-significance_threshold, significance_threshold),
               linetype = 'dashed',
               color = 'red') +
    labs(x = 'Lags',
         y = 'CCF',
         title = paste0('CCF Plot Leverage Effect: ', index)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = 'white'),
          panel.grid = element_blank(),
          axis.line = element_line(color = 'black'))
  
  return(results)
}

#####################################################################################################
### Function returns one-step-ahead VaR and ES forecast  (input data has to be zoo object)        ###
#####################################################################################################
predict_VaR_ES_1_ahead <- function(data,
                                   var.spec,
                                   mean.spec,
                                   dist.spec,
                                   tolerance_lvl,
                                   index_name,
                                   spec_i,
                                   dist_spec,
                                   n_compl_opti,
                                   new_coef_est_counter){
  
  # Terminate function if data is not zoo object
  if(!is.zoo(data)){
    data_class <- class(data)
    error_message <- paste0('Input is ',data_class, ' but needs to be zoo object!')
    stop(error_message)
  }
  
  # Global list to store all quantities which are not in output of function but will later be needed
  if(!exists('other.quantities')){
    other.quantities <<- list()
  }
  
  # Global list to record NAs with their date an the part of the fuction they appeared in
  if(!exists('NA.information')){
    NA.information <<- list()
  }
  
  if(is.null(NA.information[[index_name]])){
    NA.information[[index_name]] <<- list(fit = vector(),
                                          forecast = vector(),
                                          mu = vector(),
                                          sigma = vector(),
                                          q_tolerance_lvl = vector(),
                                          integrated_value = vector(),
                                          grad_mu = vector(),
                                          grad_sigma = vector(),
                                          CumVio = vector())
  }
  
  # Store index of last obs
  index_last_obs <- tail(index(data), 1)
  
  # Store speci_dist
  speci_dist <- paste0('spec', spec_i, '_', dist_spec)
  
  # Function that returns NA for VaR and ES if both can't be calculated and writes message to console
  return_na_VaR_ES <- function(speci_dist){
    
    # Storing NAs in other.quantities in cases one part of the function fails
    other.quantities[[index_name]][[speci_dist]][['cov_matrix']][[as.character(index_last_obs)]] <<- NA
    other.quantities[[index_name]][[speci_dist]][['coef_est']][[as.character(index_last_obs)]] <<- NA
    other.quantities[[index_name]][[speci_dist]][['grad_mu']][[as.character(index_last_obs)]] <<- NA
    other.quantities[[index_name]][[speci_dist]][['grad_sigma']][[as.character(index_last_obs)]] <<- NA
    
    if(dist_spec == 'empirical'){
      other.quantities[[index_name]][[speci_dist]][['empirical_dist']][[as.character(index_last_obs)]] <<- NA
    }
    
    VaR <- zoo(NA, index_last_obs)
    ES <- zoo(NA, index_last_obs)
    mu <- zoo(NA, index_last_obs)
    sigma <- zoo(NA, index_last_obs)
    skew <- zoo(NA, index_last_obs)
    shape <- zoo(NA, index_last_obs)
    lambda <- zoo(NA, index_last_obs)
    dist.spec <- zoo(NA, index_last_obs)
    
    VaR_and_ES <- list(VaR = VaR,
                       ES = ES,
                       mu = mu,
                       sigma = sigma,
                       skew = skew,
                       shape = shape,
                       lambda = lambda,
                       dist = dist.spec)
    
    # Print date which can't be calculated
    date <- index(VaR_and_ES$VaR)
    message <- paste0('Preceeding date of date of which VaR and ES forecast cannot be calculated: ', date, '\n\n')
    cat(message)
    
    # Return VaR and ES
    return(VaR_and_ES)
  }
  
  # Counter counts number of runs of one model and is used to decide based on new_coef_est_counter whether parameters of previous run should be used as starting parameters for current run or not
  if(!exists('new_coef_est')){
    new_coef_est <<- 0
  } else {
    new_coef_est <<- new_coef_est + 1
  }

  # If no coefficient from previous run existent or new_coef_est has reached a multiple of new_coef_est_counter, an empty list is assigned as starting parameter
  if(!exists('coef_prev_fit') | new_coef_est %% new_coef_est_counter == 0){
    coef_prev_fit <<- list()
  }

  # Variable to control that every n_compl_opti'th window shift and if optimization of previous run of same model failed, the global optimizer in combination with the local optimizer is used. In other cases the hybrid one is used and only in cases where this fails, the global-local-combination is used
  if(!exists('num_window_shift')){
    num_window_shift <<- 0
    } else {
    num_window_shift <<- num_window_shift + 1
    }
  
  # Specifying GARCH model
  spec <- ugarchspec(variance.model = var.spec,
                     mean.model = mean.spec,
                     distribution.model = dist.spec,
                     start.pars = coef_prev_fit)
  
  # Fitting GARCH model
  # Return NA if fitting doesn't work
  if(num_window_shift %% n_compl_opti != 0 | new_coef_est_counter == 1){
    # More efficient and faster solver, since solnp is tried first with parameters of previous window as starting parameters (hybrid tries: solnp -> nlminb -> gosolnp -> nloptr)
    # solver.control specified for gosolnp and nloptr which are used if previous optimizer fail
    fit <- tryCatch(
      {
        ugarchfit(spec = spec,
                  data = data,
                  solver = 'hybrid',
                  solver.control = list(n.restarts = 15,                               # gosolnp
                                        parallel = TRUE,                               # gosolnp
                                        cores = floor(parallel::detectCores() * 0.75), # gosolnp
                                        rseed = 123,                                   # gosolnp
                                        maxeval = 50000,                               # nloptr
                                        print_level = 0,                               # nloptr
                                        solver = 10))                                  # nloptr
      }, error = function(e){
        return(NA)
      }
    )
  } else {
    
    # Ensure that fit is NA if hybrid solver is not supposed to be run -> condition for next solver!
    fit <- NA
  }
  
  if(suppressWarnings(is.na(fit))){
    # Optimization routine first searches with global solver for global optima (to not be stuck in local optima which is not global optima) and then uses a local solver for fine-tuning
    complex_ugarchfit <- function(spec,
                                  data,
                                  var.spec,
                                  mean.spec,
                                  dist.spec){
      
      # Global optimization
      global_fit <- ugarchfit(spec = spec,
                              data = data,
                              solver = 'gosolnp',
                              solver.control = list(n.restarts = 15,
                                                    parallel = TRUE,
                                                    cores = floor(parallel::detectCores() * 0.75),
                                                    rseed = 123)) # restarts introduce randomness -> rseed for reproducibility
      
      # Extract parameter of global_fit to use them as starting parameters for local fine-tuning
      coef_global_fit <- as.list(coef(global_fit))
      
      # Spec with coef_global_fit as starting parameter for optimization
      spec_with_global_fit <- ugarchspec(variance.model = var.spec,
                                         mean.model = mean.spec,
                                         distribution.model = dist.spec,
                                         start.pars = coef_global_fit)
      
      local_fine_tuned_fit <- ugarchfit(spec = spec_with_global_fit,
                                        data = data,
                                        solver = 'nloptr',
                                        solver.control = list(maxeval = 25000,
                                                              print_level = 0,
                                                              solver = 10))
      return(local_fine_tuned_fit)
    }
    
    # Specifying GARCH model without start.pars
    spec <- ugarchspec(variance.model = var.spec,
                       mean.model = mean.spec,
                       distribution.model = dist.spec)
    
    # Execute more complex optimization routine
    fit <- tryCatch(
      {
        complex_ugarchfit(spec = spec,
                          data = data,
                          var.spec = var.spec,
                          mean.spec = mean.spec,
                          dist.spec = dist.spec)
      }, error = function(e){
        cat('\nSolvers did not work for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,'):\nError message: ', e$message, '\nNAs get returned for VaR and ES\n')
        
        return(NA)
      }
    )
  }

  # If fit is NA than NAs get returned for VaR and ES forecast
  if(suppressWarnings(is.na(fit))){
    
    # Storing last date of data and reason for NA
    new_entry_NA_fit <- index_last_obs
    names(new_entry_NA_fit) <- speci_dist
    NA.information[[index_name]][['fit']] <<- c(NA.information[[index_name]][['fit']], new_entry_NA_fit)
    
    # Additionally, coef_prev_fit, num_window_shift and new_coef_est are removed to start next iteration with more complex solver if new_coef_est_counter is not 1
    rm(coef_prev_fit,
       num_window_shift,
       new_coef_est,
       envir = .GlobalEnv)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES(speci_dist))
  }
  
  # Extracting coefficients from fit
  coef_fit <- coef(fit)
  
  # Storing coefficients for next run within one model (window shift) as starting parameter vector to speed up optimization. This has no effect if new_coef_est_counter = 1!
  coef_prev_fit <<- as.list(coef_fit)
  
  # Extracting skewness, shape and lambda parameter (if not prevalent, NA gets assigned)
  skew <- ifelse('skew' %in% names(coef_fit),
                 coef_fit['skew'],
                 NA)
  shape <- ifelse('shape' %in% names(coef_fit),
                  coef_fit['shape'],
                  NA)
  lambda <- ifelse('ghlambda' %in% names(coef_fit),
                   coef_fit['ghlambda'],
                   NA)
  
  # Storing parameter estimates and covariance matrix of parameter estimates for every forecast
  other.quantities[[index_name]][[speci_dist]][['coef_est']][[as.character(index_last_obs)]] <<- coef_fit
  other.quantities[[index_name]][[speci_dist]][['cov_matrix']][[as.character(index_last_obs)]] <<- vcov(fit)
  
  # Calculation of gradient of mu
  # Function that returns forecasted mu depending on parameters
  mu_func <- function(par_vec){

    par_list <- as.list(par_vec)
    
    spec <- ugarchspec(variance.model = var.spec,
                       mean.model = mean.spec,
                       distribution.model = dist.spec,
                       fixed.pars = par_list)
    
    forecast <- ugarchforecast(fitORspec = spec,
                               data = data,
                               n.ahead = 1)
    mu <- as.double(fitted(forecast))
    
    return(mu)
  }
  
  # Calculate gradient of mu_func at coef_fit
  grad_mu <- tryCatch(
    {
      grad(func = mu_func,
           x = coef_fit,
           method = 'simple',
           method.args=list(eps=sqrt(.Machine$double.eps)))
    }, error = function(e){
      cat('\nGradient calculation of mu did not work for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,'):\nError message: ', e$message)
      
      # Storing last date of data and reason for NA
      new_entry_NA_fit <- index_last_obs
      names(new_entry_NA_fit) <- speci_dist
      NA.information[[index_name]][['grad_mu']] <<- c(NA.information[[index_name]][['grad_mu']], new_entry_NA_fit)
      
      return(NA)
      }
  )
    
  # Storing gradiant of mu in other.quantities
  other.quantities[[index_name]][[speci_dist]][['grad_mu']][[as.character(index_last_obs)]] <<- grad_mu
  
  # Calculation of gradient of sigma
  # Function that returns forecasted sigma depending on parameters
  sigma_func <- function(par_vec){
    
    par_list <- as.list(par_vec)
    
    spec <- ugarchspec(variance.model = var.spec,
                       mean.model = mean.spec,
                       distribution.model = dist.spec,
                       fixed.pars = par_list)
    
    forecast <- ugarchforecast(fitORspec = spec,
                               data = data,
                               n.ahead = 1)
    
    sigma <- as.double(sigma(forecast))
    
    return(sigma)
  }

  # Calculate gradient of sigma_func at coef_fit
  grad_sigma <- tryCatch(
    {
      grad(func = sigma_func,
           x = coef_fit,
           method = 'simple',
           method.args=list(eps=sqrt(.Machine$double.eps)))
    }, error = function(e) {
      cat('\nGradient calculation of sigma did not work for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,'):\nError message: ', e$message)
      
      # Storing last date of data and reason for NA
      new_entry_NA_fit <- index_last_obs
      names(new_entry_NA_fit) <- speci_dist
      NA.information[[index_name]][['grad_sigma']] <<- c(NA.information[[index_name]][['grad_sigma']], new_entry_NA_fit)
      
      return(NA)
    }
  )

  # Storing gradient of sigma in other.quantities
  other.quantities[[index_name]][[speci_dist]][['grad_sigma']][[as.character(index_last_obs)]] <<- grad_sigma
  
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
  
    # Storing last date of data and reason for NA
    new_entry_NA_forecast <- index_last_obs
    names(new_entry_NA_forecast) <- speci_dist
    NA.information[[index_name]][['forecast']] <<- c(NA.information[[index_name]][['forecast']], new_entry_NA_forecast)
    
    # Additionally, coef_prev_fit, num_window_shift and new_coef_est are removed to start next iteration with more complex solver if new_coef_est_counter is not 1
    rm(coef_prev_fit,
       num_window_shift,
       new_coef_est,
       envir = .GlobalEnv)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES(speci_dist))
  }
  
  # Extraction of predicted mean and standard deviation
  mu <- fitted(forecast)
  sigma <- sigma(forecast)
  
  # Writing message if mu can't be calculated and return NA for VaR and ES
  if(is.nan(mu)){
    
    # Console message
    cat('\nMu cannot be calculated for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,')\nNAs get returned for VaR and ES\n')
    
    # Storing last date of data and reason for NA
    new_entry_NA_mu <- index_last_obs
    names(new_entry_NA_mu) <- speci_dist
    NA.information[[index_name]][['mu']] <<- c(NA.information[[index_name]][['mu']], new_entry_NA_mu)
    
    # Additionally, coef_prev_fit, num_window_shift and new_coef_est are removed to start next iteration with more complex solver if new_coef_est_counter is not 1
    rm(coef_prev_fit,
       num_window_shift,
       new_coef_est,
       envir = .GlobalEnv)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES(speci_dist))
  }
      
  # Writing message if sigma can't be calculated and return NA for VaR and ES
  if(is.nan(sigma)){
    
    # Console message
    cat('\nSigma cannot be calculated for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,')\nNAs get returned for VaR and ES\n')
    
    # Storing last date of data and reason for NA
    new_entry_NA_sigma <- index_last_obs
    names(new_entry_NA_sigma) <- speci_dist
    NA.information[[index_name]][['sigma']] <<- c(NA.information[[index_name]][['sigma']], new_entry_NA_sigma)
    
    # Additionally, coef_prev_fit, num_window_shift and new_coef_est are removed to start next iteration with more complex solver if new_coef_est_counter is not 1
    rm(coef_prev_fit,
       num_window_shift,
       new_coef_est,
       envir = .GlobalEnv)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES(speci_dist))
  }
      
  # Using empirical innovations for VaR and ES forecast if dist_spec = empirical
  if(dist_spec == 'empirical'){
 
    # Standardized residuals
    resid_std <- residuals(fit,
                           standardize = TRUE)
    
    # Storing empirical distribution of standardized innovations in global list of other quantities with dates as entry names
    other.quantities[[index_name]][[speci_dist]][['empirical_dist']][[as.character(index_last_obs)]] <<- as.vector(resid_std)
    
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

    # Transform skew, shape and lambda with NAs to zoo object and insert empirical as distribution specification
    skew <- zoo(NA, index_last_obs)
    shape <- zoo(NA, index_last_obs)
    lambda <- zoo(NA, index_last_obs)
    dist.spec <- zoo('empirical', index_last_obs)
    
    # Combine VaR and ES in one list
    VaR_and_ES <- list(VaR = VaR,
                       ES = ES,
                       mu = mu,
                       sigma = sigma,
                       skew = skew,
                       shape = shape,
                       lambda = lambda,
                       dist = dist.spec)
    
    #Return results
    return(VaR_and_ES)
  }

  # Function returns pth quantile of current distribution
  pth_quantile <- function(p) {
    q <- qdist(distribution = dist.spec,
               p = p,
               skew = skew,
               shape = shape,
               lambda = lambda)
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
    
    # Storing last date of data and cause of NA
    new_entry_NA_q_tolerance_lvl <- index_last_obs
    names(new_entry_NA_q_tolerance_lvl) <- speci_dist
    NA.information[[index_name]][['q_tolerance_lvl']] <<- c(NA.information[[index_name]][['q_tolerance_lvl']], new_entry_NA_q_tolerance_lvl)
    
    # Returning NAs for VaR and ES
    return(return_na_VaR_ES(speci_dist))
  }
  
  # Calculation of one-step-ahead VaR forecast
  VaR <- mu + sigma * q_tolerance_lvl
  
  # Integrate over inverse cdf: from 0 to tolerance level
  # Return NULL if integration doesn't work
  integrated_value <- tryCatch(
    {
      integrated <- integrate(pth_quantile,
                              lower = 0,
                              upper = tolerance_lvl)
      
      # Extract value of integration
      integrated[['value']]
      
    }, error = function(e) {
      cat('\nIntegration did not work for one observation (Index:', index_name, '; Variance Specification Nr.:', spec_i, '; Distribution:', dist.spec,'):\nError message', e$message, '\nNA gets returned for ES\n')
      
      return(NULL)
    }
  )
  
  # If integrated value is NULL than NA gets returned for ES forecast together with forecasted VaR
  if(is.null(integrated_value)){
    
    # Storing last date of data and reason for NA
    new_entry_NA_integrated_value <- index_last_obs
    names(new_entry_NA_integrated_value) <- speci_dist
    NA.information[[index_name]][['integrated_value']] <<- c(NA.information[[index_name]][['integrated_value']], new_entry_NA_integrated_value)
    
    # Storing NAs in other.quantities in cases where one part of the function fails
    other.quantities[[index_name]][[speci_dist]][['cov_matrix']][[as.character(index_last_obs)]] <<- NA
    other.quantities[[index_name]][[speci_dist]][['grad_mu']][[as.character(index_last_obs)]] <<- NA
    other.quantities[[index_name]][[speci_dist]][['grad_sigma']][[as.character(index_last_obs)]] <<- NA

    # Creating list with VaR and ES
    ES <- zoo(NA, index_last_obs)
    skew <- zoo(skew, index_last_obs)
    shape <- zoo(shape, index_last_obs)
    lambda <- zoo(lambda, index_last_obs)
    dist.spec <- zoo(dist.spec, index_last_obs)
    VaR_and_ES <- list(VaR = VaR,
                       ES = ES,
                       mu = mu,
                       sigma = sigma,
                       skew = skew,
                       shape = shape,
                       lambda = lambda,
                       dist = dist.spec)
    
    # Print date which can't be calculated
    date <- index(VaR_and_ES[['ES']])
    message <- paste0('Preceeding date of date where ES cannot be calculated: ', date, '\n\n')
    cat(message)
    
    # Return VaR and ES
    return(VaR_and_ES)
  }
      
  # Calculation of ES
  ES <- mu + sigma / tolerance_lvl * integrated_value
  
  # Transform skew, shape and lambda to zoo object
  skew <- zoo(skew, index_last_obs)
  shape <- zoo(shape, index_last_obs)
  lambda <- zoo(lambda, index_last_obs)
  dist.spec <- zoo(dist.spec, index_last_obs)
  
  # Combine VaR and ES in one list
  VaR_and_ES <- list(VaR = VaR,
                     ES = ES,
                     mu = mu,
                     sigma = sigma,
                     skew = skew,
                     shape = shape,
                     lambda = lambda,
                     dist = dist.spec)
  
  #Return list with VaR and ES forecasts
  return(VaR_and_ES)
}


############################################################################################
###  Kupiec test: Unonditional Coverage VaR                                              ###
############################################################################################
VaR_Kupiec_backtest <- function(exceedences,
                                speci_dist,
                                tolerance_lvl){
  
  # Using only non-missing entries
  exceedences <- na.omit(exceedences)
  
  # Calculating number of non-exceedences, exceedences and proportion of exceedences
  n1 <- sum(exceedences)
  n0 <- length(exceedences) - n1
  prop_exceeded <- n1 / (n1 + n0)
  
  # Likelihood Ratio Test
  # Likelihood of unrestricted (L_ur) and restricted (L_r) model
  L_ur <- prop_exceeded ^ n1 * (1 - prop_exceeded) ^ n0
  L_r <- tolerance_lvl ^ n1 * (1 - tolerance_lvl) ^ n0
  
  # Test statistic and assign name speci_dist
  LR <- -2 * log(L_r / L_ur)
  names(LR) <- speci_dist
  
  # p value (LR ~ X^2(1)) and assign name speci_dist
  p <- pchisq(q = LR,
              df = 1,
              lower.tail = FALSE)
  names(p) <- speci_dist
  
  #Return results
  results <- list(LR = LR,
                  p = p)
  return(results)
}

############################################################################################
###  Christofferson 1 test: Independence of violations                                   ###
############################################################################################
VaR_Christofferson1_backtest <- function(exceedences,
                                         speci_dist){
  
  # n_ij starts with 0 in for each column
  n00 <- 0
  n11 <- 0
  n10 <- 0
  n01 <- 0
  
  # Using only non-missing entries
  exceedences <- na.omit(exceedences)
  
  # Assign n_ij (n_ij = 1 with i in {0,1} and j in {0,1})
  for(i in 2:length(exceedences)){
    if(exceedences[i - 1] == 0 & exceedences[i] == 0){
      n00 <- n00 + 1
    }
    else if(exceedences[i - 1] == 1 & exceedences[i] == 1){
      n11 <- n11 + 1
    }
    else if(exceedences[i - 1] == 1 & exceedences[i] == 0){
      n10 <- n10 + 1
    }
    else if(exceedences[i - 1] == 0 & exceedences[i] == 1){
      n01 <- n01 + 1
    }
    else {
      stop('Error: Values out of domain {0, 1}')
    }
  }
  
  # Assigning proportion of exceedences if previous observation exceeded / didn't exceed
  prop_exceeded <- (n01 + n11) / (n00 + n11 + n10 + n01)
  prop_exceeded11 <- n11 / (n10 + n11)
  prop_exceeded01 <- n01 / (n00 + n01)
  
  # Likelihood Ratio Test
  # Likelihood of restricted (L_r) and unrestricted (L_ur) model
  L_r <- prop_exceeded ^ (n01 + n11) * (1 - prop_exceeded) ^ (n00 + n10)
  L_ur <- prop_exceeded01 ^ n01 * (1 - prop_exceeded01) ^ n00 * prop_exceeded11 ^ n11 * (1 - prop_exceeded11) ^ n10
  
  # Test statistic and assign name speci_dist
  LR <- -2 * log(L_r / L_ur)
  names(LR) <- speci_dist
  
  # p value (LR ~ X^2(1)) and assign name speci_dist
  p <- pchisq(q = LR,
              df = 1,
              lower.tail = FALSE)
  names(p) <- speci_dist
  
  #Return results
  results <- list(LR = LR,
                  p = p)
  return(results)
}


############################################################################################
### Christofferson 2 test: Conditional Coverage VaR                                      ###
############################################################################################
VaR_Christofferson2_backtest <- function(exceedences,
                                         speci_dist,
                                         tolerance_lvl){
  
  # n_ij starts with 0 in for each column
  n00 <- 0
  n11 <- 0
  n10 <- 0
  n01 <- 0
  
  # Count number of entries which are not NAs
  # n_entries <- sum(exceedences[!is.na(exceedences)])
  
  # Using only non-missing entries
  exceedences <- na.omit(exceedences)
  
  # Assign n_ij (n_ij = 1 with i in {0,1} and j in {0,1})
  for(i in 2:length(exceedences)){
    if(exceedences[i - 1] == 0 & exceedences[i] == 0){
      n00 <- n00 + 1
    }
    else if(exceedences[i - 1] == 1 & exceedences[i] == 1){
      n11 <- n11 + 1
    }
    else if(exceedences[i - 1] == 1 & exceedences[i] == 0){
      n10 <- n10 + 1
    }
    else if(exceedences[i - 1] == 0 & exceedences[i] == 1){
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
  
  # Test statistic and assign name speci_dist
  LR <- -2 * log(L_r / L_ur)
  names(LR) <- speci_dist
  
  # p value (LR ~ X^2(2)) and assign name speci_dist
  p <- pchisq(q = LR,
              df = 2,
              lower.tail = FALSE)
  names(p) <- speci_dist
  
  #Return results
  results <- list(LR = LR,
                  p = p)
  return(results)
}


############################################################################################
### Unconditional coverage test ES                                                       ###
############################################################################################
ES_uc_backtest <- function(CumVio,
                           speci_dist,
                           tolerance_lvl,
                           robust,
                           index,
                           cov_matrices,
                           window_width,
                           grad_mu,
                           grad_sigma,
                           r,
                           mu,
                           sigma,
                           skew,
                           shape,
                           lambda,
                           dist){
  
  # Vector with positions which are not NA in CumVio
  not_na_Cum_Vio <- !is.na(CumVio)
  
  # Using only non-missing entries
  H_hut <- CumVio[not_na_Cum_Vio]
  
  # Mean of H_huts
  H_mean <- mean(H_hut)
  
  # Number of non-missing entries
  n <- length(H_hut)
  
  # If robust = TRUE, robust UC Backtest is calculated -> U ~ N(0, sigma) -> parameter estimation effect in test statistic included
  # If robust = FALSE: Assumption U ~ N(0,1)
  if(robust){
    
    # Calculating mean of covariance matrices W
    n_cov <- 0
    for(i in 1:(length(cov_matrices) - 1)){
      if(is.matrix(cov_matrices[[i]])){
        if(n_cov == 0){
          sum_cov <- cov_matrices[[i]]
          n_cov <- 1
        } else {
          sum_cov <- sum_cov + cov_matrices[[i]]
          n_cov <- n_cov + 1
        }
      }
    }
    W <- sum_cov / n_cov
    
    # Test that number of covariance matrices is correct
    if(n_cov != n){
      cat(paste0('\nES UC Backtest: Number of entries in ', index, ' CumVio_', speci_dist, ' (', n, ') are different from number of its covariance matrices (', n_cov, ')\n\n'))
    }
    
    # Test that number of gradients without NAs is correct
    n_grad <- 0
    for(i in 1:(length(grad_sigma) - 1)){
      if(!(NA %in% grad_sigma[[i]])){
        n_grad <- n_grad + 1
      }
    }
    
    if(n_grad != n){
      cat(paste0('\nES UC Backtest: Number of entries in ', index, ' CumVio_', speci_dist, ' (', n, ') are different from number of its gradients without NAs (', n_grad, ')\n\n'))
    }
    
    # Extracting values necessary to calculate R
    r <- r[not_na_Cum_Vio]
    mu <- mu[not_na_Cum_Vio]
    eps <- r - mu
    sigma <- sigma[not_na_Cum_Vio]
    skew <- skew[not_na_Cum_Vio]
    shape <- shape[not_na_Cum_Vio]
    lambda <- lambda[not_na_Cum_Vio]
    dist <- dist[not_na_Cum_Vio]
    
    # In case of empirical distribution, replace the empirical distribution with normal distribution to make robust test feasible (THINK ABOUT THIS, IF IT MAKES SENSE!!!!!!!!!)
    dist <- ifelse(dist == 'empirical', 'norm', dist)
    
    # Calculation of R_ES_hut
    for(i in 1:n){
      G_q_eps <- qdist(distribution = dist[i],
                       p = tolerance_lvl,
                       mu = 0,
                       sigma = sigma[i],
                       skew = skew[i],
                       shape = shape[i],
                       lambda = lambda[i])
      if(eps[i] <= G_q_eps){
        g_d_eps <- ddist(distribution = dist[i],
                         y = eps[i],
                         mu = 0,
                         sigma = sigma[i],
                         skew = skew[i],
                         shape = shape[i],
                         lambda = lambda[i])
        R_i <- g_d_eps * ((grad_mu[[i]] + (eps[i] * grad_sigma[[i]])) / sigma[i])
        
        if(!exists('R')){
          R <- R_i
        } else {
          R <- R + R_i
        }
      }
    }
    
    R_ES_hut <- R / (tolerance_lvl * n)
    
    rm(R)
    
    # Calculation of variance correction factor
    cor_fac <- (n / window_width) * (t(R_ES_hut) %*% W %*% R_ES_hut)
    cor_fac <- as.double(cor_fac)
  } else {
    # Correction factor is zero if assimption U ~ N(0,1)
    cor_fac <- 0
  }
  
  # Calculating test statistic and assign name speci_dist
  U <- (sqrt(n) * (H_mean - (tolerance_lvl / 2))) / sqrt((tolerance_lvl * ((1 / 3) - (tolerance_lvl / 4))) + cor_fac)
  names(U) <- speci_dist
  
  # Calculating p value and assign name speci_dist
  p <- 2 * pnorm(q = abs(U),
                 lower.tail = FALSE)
  names(p) <- speci_dist
  
  # Return results
  result <- list(U = U,
                 p = p)
  
  return(result)
}


############################################################################################
### Conditional coverage test ES                                                         ###
############################################################################################
ES_cc_backtest <- function(CumVio,
                           speci_dist,
                           tolerance_lvl,
                           lags,
                           robust,
                           index,
                           cov_matrices,
                           window_width,
                           grad_mu,
                           grad_sigma,
                           r,
                           mu,
                           sigma,
                           skew,
                           shape,
                           lambda,
                           dist){
  
  # Vector with positions which are not NA in CumVio
  not_na_Cum_Vio <- !is.na(CumVio)
  
  # Using only non-missing entries
  H_hut <- CumVio[not_na_Cum_Vio]
  
  # Number of entries
  n <- length(H_hut)
  
  # Variance of H_huts
  gamma_n0 <- 1 / n * sum((H_hut - (tolerance_lvl / 2)) * (H_hut - (tolerance_lvl / 2)))
  
  # Vector with covariance between H_hut and j-laged H_hut
  gamma_nj <- vector(length = lags)
  
  for(j in 1:lags){
    gamma_nj[j] <- 1 / (n - j) * sum((H_hut[(j+1):n] - (tolerance_lvl / 2)) * (H_hut[1:(n-j)] - (tolerance_lvl / 2)))
  }
  
  # Vector with correlations between H_hut and j-laged H_hut
  rho_nj <- as.vector(gamma_nj / gamma_n0)
  
  
  # Robust conditional coverage test
  
  # If robust = TRUE, robust CC Backtest is calculated -> parameter estimation effect in test statistic included -> corrected C ~ X^2(lags)
  # If robust = FALSE: Assumption uncorrected C ~ X^2(lags)
  if(robust){
    
    # Calculating mean of covariance matrices W
    n_cov <- 0
    for(i in 1:(length(cov_matrices) - 1)){
      if(is.matrix(cov_matrices[[i]])){
        if(n_cov == 0){
          sum_cov <- cov_matrices[[i]]
          n_cov <- 1
        } else {
          sum_cov <- sum_cov + cov_matrices[[i]]
          n_cov <- n_cov + 1
        }
      }
    }
    W <- sum_cov / n_cov
    
    # Test that number of covariance matrices is correct
    if(n_cov != n){
      cat(paste0('\nES CC Backtest: Number of entries in ', index, ' CumVio_', speci_dist, ' (', n,') are different from number of its covariance matrices (',n_cov,')\n\n'))
    }
    
    # Test that number of gradients without NAs is correct
    n_grad <- 0
    for(i in 1:(length(grad_sigma) - 1)){
      if(!(NA %in% grad_sigma[[i]])){
        n_grad <- n_grad + 1
      }
    }
    
    if(n_grad != n){
      cat(paste0('\nES CC Backtest: Number of entries in ', index, ' CumVio_', speci_dist, ' (', n,') are different from number of its gradients (', n_grad, ')\n\n'))
    }
    
    # Extracting values necessary to calculate R
    r <- r[not_na_Cum_Vio]
    mu <- mu[not_na_Cum_Vio]
    eps <- r - mu
    sigma <- sigma[not_na_Cum_Vio]
    skew <- skew[not_na_Cum_Vio]
    shape <- shape[not_na_Cum_Vio]
    lambda <- lambda[not_na_Cum_Vio]
    dist <- dist[not_na_Cum_Vio]
    
    # In case of empirical distribution, replace the empirical distribution with normal distribution to make robust test feasible (THINK ABOUT THIS, IF IT MAKES SENSE!!!!!!!!!)
    dist <- ifelse(dist == 'empirical', 'norm', dist)
    
    # List to store all R_huts
    R_j_hut <- list()
    
    # Loop of different lags j
    for(j in 1:lags){
      
      # Calculation of R_j_hut
      for(i in (1+j):n){
        G_q_eps <- qdist(distribution = dist[i],
                         p = tolerance_lvl,
                         mu = 0,
                         sigma = sigma[i],
                         skew = skew[i],
                         shape = shape[i],
                         lambda = lambda[i])
        if(eps[i] <= G_q_eps){
          g_d_eps <- ddist(distribution = dist[i],
                           y = eps[i],
                           mu = 0,
                           sigma = sigma[i],
                           skew = skew[i],
                           shape = shape[i],
                           lambda = lambda[i])
          R_j_i <- (H_hut[(i-j)] - (tolerance_lvl / 2)) * g_d_eps * ((grad_mu[[i]] + (eps[i] * grad_sigma[[i]])) / sigma[i])
          
          if(!exists('R_j')){
            R_j <- R_j_i
          } else {
            R_j <- R_j + R_j_i
          }
        }
      }
      
      R_j_hut[[j]] <- as.vector((1 / (tolerance_lvl * ((1 / 3) - (tolerance_lvl / 4)))) * (1 / (n - j)) * R_j)
      
      rm(R_j)
    }
    
    # Creating empty sigma_hut matrix
    sigma_hut_mat <- matrix(data = NA, ncol = lags, nrow = lags)
    
    # Calculating every element of sigma_hut matrix
    for(i in 1:lags){
      for(j in 1:lags){
        sigma_hut_mat[i,j] <- ifelse(i == j, 1, 0) + ((n / window_width) * (t(R_j_hut[[i]]) %*% W %*% R_j_hut[[j]]))
      }
    }
    
    # Inverting sigma_hut_mat
    sigma_hut_mat_inv <- solve(sigma_hut_mat)
    
  } else {
    # Sigma_hut matrix is identity matrix if no parameter estimation risk correction is applyed to variance of test statistic C
    sigma_hut_mat_inv <- diag(x = 1, nrow = lags)
  }
  
  # Test statistic for Box-Pierce-Test and assign name speci_dist
  C <- n * t(rho_nj) %*% sigma_hut_mat_inv %*% rho_nj
  names(C) <- speci_dist
  
  # p-value of test statistic and assign name speci_dist
  p <- pchisq(q = C,
              df = length(rho_nj),
              lower.tail = FALSE)
  names(C) <- speci_dist
  
  #Return results
  results <- list(C = C,
                  p = p)
  return(results)
}