#########################################################################
### Returns most important statistics for financial time series data ####
#########################################################################

ts.main.statistics <- function(data, lags_Ljung_Box_test = 15, lags_ArchTest = 12, nu = 5) {
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
                                   tolerance_lvl = 0.05,
                                   index_name,
                                   spec_i = 'NA',
                                   dist = 'NA'){
  # Omit NAs form data
  data <- na.omit(data)
  
  # Specifying garch model
  spec <- ugarchspec(variance.model = var.spec,
                     mean.model = mean.spec,
                     distribution.model = dist.spec)
  
  # Fitting garch model (hybrid automatically trys different solvers: solnp, nlminb, gosolnp, nloptr):
  fit <- tryCatch(
    {
      ugarchfit(spec = spec,
                data = data,
                solver = 'hybrid')
    },
    error = function(e){
      cat('\nSolvers did not work for one observation (Index: ', index_name, ' Spec: ', spec_i, ', Dist: ', dist,'):', e$message, '\n\n')
      
      return(NA)
    }
  )
    
  # Forecasting one-step-ahead VaR and ES (insert NA if no solver converges)
  if(is.na(fit)) {
    index.NA <- tail(index(data), 1)
    VaR <- zoo(NA, index.NA)
    ES <- zoo(NA, index.NA)
    VaR_and_ES <- list(VaR = VaR,
                       ES = ES)
  } else {
    # Coefficients
    coef_fit <- coef(fit)
    
    # Skewness and shape parameter
    skew <- coef_fit['skew']
    shape <- coef_fit['shape']
    
    # One-step-ahead forecast of model
    forecast <- ugarchforecast(fitORspec = fit,
                               n.ahead = 1)
    
    # Extraction of predicted mean and standard deviation
    mu <- fitted(forecast)
    sigma <- sigma(forecast)
    
    # Writing message if sigma cannot be calculated
    if(is.nan(sigma)){
      cat('\nSigma cannot be calculated for one observation (Index: ', index_name, ' Spec: ', spec_i, ', Dist: ', dist,')\n\n')
      sigma <- NA
    }
  
    # Function returns pth quantile of current distribution
    pth_quantile <- function(p) {
      q <- qdist(distribution = dist.spec,
                 p = p,
                 skew = skew,
                 shape = shape)
      return(q)
    }
      
    # Calculation of one-step-ahead VaR forecast
    VaR <- mu + sigma * pth_quantile(p = tolerance_lvl)
  
    # Integrate over inverse cdf: from 0 until tolerance level (handling errors gracefully)
    integrated <- tryCatch(
      {
        integrate(pth_quantile,
                  lower = 0,
                  upper = tolerance_lvl)
      }, error = function(e) {
         cat('\nIntegration did not work for one observation (Index: ', index_name, ' Spec: ', spec_i, ', Dist: ', dist,'):', e$message, '\n\n')
        
         return(NULL)
      }
    )
    
    # Test if integration worked, if not, assign NA
    if(is.null(integrated)){
      integrated_value <- NA
    } else {
      integrated_value <- integrated$value      
    }
    
    # Calculation of ES
    ES <- mu + sigma / tolerance_lvl * integrated_value
    
    # Combine VaR and ES in list
    VaR_and_ES <- list(VaR = VaR,
                       ES = ES)
  }
  #Return list with VaR and ES forecasts
  return(VaR_and_ES)
}


