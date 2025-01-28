#################################################################################
####           TO-DO-LIST AND NOTES                                          ####
#################################################################################

#DEFINITLY:
# Recheck all tests
# Try loading results with simulations
# Include check that Covariance matrix was calculated and added to other.quantities (it appears that sometimes the covariance matrix of estimation is not estimated)
# Compare results of my tests with test from public packages

#DONE:
# Add execution time to other.quantities[[index]][[paste0('spec_', spec_i, '_', dist_spec)]][['execution_time]] <- time in stepwise_VaR_ES_forecastin line 126
# Recheck whole process and delete parts which are not needed anymore

# All for ES Backtest:
# In gradient calculation of mu and sigma, should it really be the forecasted value or the fitted value??? -> probably forecasted value
# handle errors in gradient calculation -> improve parameters of grad(): consider first richardson and if this fails then simple
# Check that ES Backtests handle NAs in other.quantities correctly (historical distributions maybe different?)
# Check if backtest for historical distribution makes sense
# Include other ES Backtests from esback package

# NOTES:

# MAYBE:
# Include other ES Backtests -> definitly compare results I would receive with that
# Include loss function for comparison if backtest don't come to satisfactory result
# Store information of problems during backtest execution in separate object
# Handle convergence information


# CHALLANGES:

# IDEAS:

# RECHECK LOGIC RESULTS:
# - date assignments are correct in tables and other.quantities

# NOTE:
# Using parameter of previous shift as starting parameter reduces optimization time a lot to biased estimates but leads in some models -> not done in final version (but with setting n_compl_opti and new_coef_est_counter still possible)
# First return in input data has to be NA that the program works correctly!
# main processing time consumers are optimization, gradiant calculation and integration. Only optimization can really be adjusted. A simple way would be to adjust n_compl_opti, but as mentioned above this leads to errors
# paralleling would decrease computation time a lot, but conflicts with program logic as it is -> current program relies on sequential execution to work properly -> paralleling could be an improvement for later

#################################################################################
####           General set-up                                                ####
#################################################################################

# Open connection to write console messages into a txt file
sink('console_messages.txt', split = TRUE)

# Initial cleaning
rm(list = ls())
if (dev.cur() != 1) {
  dev.off()
}
cat('\14')

# Restore packages of renv
# If library not synchronized to lockfile, a dialog pops up and asks if all listed packages should be installed. Enter 'Y' in console. Packages are then installed with correct version.
renv::restore()

# Print start time
Sys.time()

# Loading required libraries
library(tidyverse)  # comprehensive package for data manipulation
library(zoo)        # handling of time series data
library(rugarch)    # comprehensive package for GARCH modeling and distributions
library(FinTS)      # used for Arch test
library(numDeriv)   # calculation of numerical derivatives
library(moments)    # moment calculation
library(tseries)    # used for JB test

# Definition of functions
source('scripts/functions.R')

#################################################################################
####           General parameter setting                                     ####
#################################################################################

# Number of auto-regressive terms for mean model
ar = 0

# Number of moving average terms for mean model
ma = 0

# Number of ARCH terms for variance model
arch = 1

# Number of GARCH terms for variance model
garch = 1

# Width of estimation window for rolling forecasting
window_width = 750

# Tolerance level for VaR and ES
tolerance_lvl = 0.05

#######################################################################################
### Sub setting parameters for faster calculations (mainly for development period   ###
#######################################################################################

# Number of forecasts (comment out if not needed)
#number_forecasts = 50

# Input data - has to be higher than parameter window_width (could also be set directly, but setting it with number_forecasts is recommended) (comment out if not needed)
#data_include = 1:(window_width+1+number_forecasts)

# Indices to include (comment out if not needed)
#index_include = c(1,2)

# Variance specifications (comment out if not needed)
varspec_include = c(1:11)

# Distribution assumptions (comment out if not needed)
dist_include = c(1:7,10,11)

# Should real data or simulated data be used? TRUE for simulated data (needs to be set)
# Script used for simulation can be found in scripts/simulate_data.R
simulation = FALSE

# Number of simulations (has to be set if simulation = TRUE) (comment out if if simulation = FALSE)
#number_simulations = 400

# Execution of VaR and ES forecast (has to be set)
# TRUE: stepwise VaR and ES forecast calculates all over again (takes multiple days to run with full data set!)
# FALSE: old results are loaded from csv files in output folder - not recommended to use with simulated data - index_include specifies which index data is loaded
execution_of_VaR_ES_forecasting = TRUE

# Parameter which sets number of window shifts after which complex_ugarchfit should be executed. Explanation can be found in function.R at position of complex_ugarchfit (has to be set)
# In cases where new_coef_est_counter = 1, this parameter has no effect!
n_compl_opti = 100

# Parameter which sets number of times after which fitting is executed without parameter of previous run as starting parameters. It is recommended to set it to 1, since it leads to faster executions but also introduces biases in some models (has to be set)
new_coef_est_counter = 1

# Execution of VaR and ES Backtests (has to be set)
execute_Backtest = FALSE

# Plot all calculated models (has to be set)
plot_all_calc_models = FALSE

#################################################################################
####           General model specification set-up                            ####
#################################################################################

if(simulation){
  
  # Vector of all simulated data
  indices <- vector()
  for(i in 1:number_simulations){
    indices <- c(indices, paste0('sim', i))
    }
  
  } else {
    
    # Vector of all index names
    indices <- c('DAX',
                 'WIG',
                 'BTC',
                 'GLD')
    }

# List of all possible variance specifications from rugarch (excluding mcsGARCH, since this is mainly for intra-day data)
var.spec.list <- list(spec1 = list(model = 'sGARCH',              # ARCH (set arch correctly)
                                   garchOrder = c(10, 0)),
                      spec2 = list(model = 'sGARCH',              # GARCH
                                   garchOrder = c(arch, garch)),
                      spec3 = list(model = 'eGARCH',              # EGARCH
                                   garchOrder = c(arch, garch)),
                      spec4 = list(model = 'gjrGARCH',            # GJR-GARCH
                                   garchOrder = c(arch, garch)),
                      spec5 = list(model = 'fGARCH',              # TGARCH
                                   garchOrder = c(arch, garch),
                                   submodel = 'TGARCH'),
                      spec6 = list(model = 'fGARCH',              # FGARCH
                                   garchOrder = c(arch, garch),
                                   submodel = 'ALLGARCH'),
                      spec7 = list(model = 'apARCH',              # APARCH
                                   garchOrder = c(arch, garch)),
                      spec8 = list(model = 'fGARCH',              # NGARCH
                                   garchOrder = c(arch, garch),
                                   submodel = 'NGARCH'),
                      spec9 = list(model = 'csGARCH',             # CSGARCH
                                   garchOrder = c(arch, garch)),
                      spec10 = list(model = 'fGARCH',             # AVGARCH
                                    garchOrder = c(arch, garch),
                                    submodel = 'AVGARCH'),
                      spec11 = list(model = 'fGARCH',             # NAGARCH
                                    garchOrder = c(arch, garch),
                                    submodel = 'NAGARCH'),
                      spec12 = list(model = 'iGARCH',             # IGARCH: Leave out! ES backtest always fails due to incompatible dimensions of R and W
                                    garchOrder = c(arch, garch)),
                      spec13 = list(model = 'realGARCH',          # realized GARCH: don't use: ugarchfit-->error: you must supply the realized volatility (realizedVol) for the realGARCH model
                                    garchOrder = c(arch, garch)),
                      spec14 = list(model = 'fiGARCH',            # FIGARCH: don't use: gradient calculations and forecasting does not work
                                    garchOrder = c(arch, garch)))


# Mean specification
mean.spec <- list(armaOrder = c(ar,ma),
                  include.mean = TRUE)

# List of all possible distribution assumptions in rugarch package
dist.spec.list <-  list(norm = 'norm',      #1
                        std = 'std',        #2
                        ged = 'ged',        #3
                        snorm = 'snorm',    #4
                        sstd = 'sstd',      #5
                        sged = 'sged',      #6
                        ghyp = 'ghyp',      #7   # nests sstd and nig
                        nig = 'nig',        #8   # dont include: nig leads to problems (a lot of NaN in spec 3 for GLD -> Should I exclude it)
                        ghst = 'ghst',      #9   # dont include: optimazation takes often too long with this distribution -> Sould I exclude it? --- df and skewness parameter interact in complicated way
                        jsu = 'jsu',        #10  # condider to include: jsu leads to some problems (NaN in sigma in Spec 3 for GLD -> Should I include it or not?)
                        empirical = 'norm') #11  # Normal distribution for QML estimation -> asymptotically consistent


#################################################################################
####  Data preparation and sub setting of data and specifications           ####
#################################################################################

source('scripts/preparing_data.R')

source('scripts/subset.R')


#################################################################################
####           Descriptive part                                              ####
#################################################################################

price.return.plots <- list()
main.statistics <- list()

for(index in indices){
  
  # Price and return plots
  price.return.plots[[index]] <- price_return_plots_func(index = index)
  
  # Main descriptive statistics
  main.statistics[[index]] <- ts_main_statistics(index = index,
                                                 lags_Ljung_Box_test = 10,
                                                 lags_ArchTest = 10,
                                                 nu = 5)
}
rm(index)

#Storing both lists in output folder
saveRDS(price.return.plots, file=paste0(ifelse(simulation, 'simulated_output/', 'output/'), 'price_return_plots.RData'))
saveRDS(main.statistics, file=paste0(ifelse(simulation, 'simulated_output/', 'output/'), 'main_statistics.RData'))

# DAX
  # -> significant autocorrelation (10 lags) (but looks very uninformative and weak)
  # -> significant ARCH effect (10 lags)
  # -> leverage effect
  # -> mean model could potentially be useful
  
# WIG
  # -> no significant autocorrelation (10 lags)
  # -> significant ARCH effect (10 lags)
  # -> leverage effect
  # NO MEAN MODEL NEEDED
  
# BTC
  # -> significant autocorrelation (10 lags) (potentially informative: first day - negative reaction; second day - positive reaction -- but very small coefficients)
  # -> significant ARCH effect (10 lags)
  # -> no clear leverage effect (only very slight)
  # -> mean model could potentially be useful
  
# GLD
  # -> no significant autocorrelation (10 lags)
  # -> significant ARCH effect (10 lags)
  # -> no or late and weak leverage effect
  # NO MEAN MODEL NEEDED

# GENERAL
# - Serial autocorrelation seems to be non existent or very weak -> mostly uninformative, although direction in BTC interesting
# - Volatility clustering always existent
# - leverage effect seems to be stronger in stock indices than in BTC and especially stronger than in GLD
# - Standardized returns are not normally distributed (excess kurtosis and negative skewness)
# - slightly positive mean and median of returns (DAX, WIG, GLD: 1e-4, BTC: 1e-3) -> slight positive drift

#################################################################################
####  Forecasting or loading forecasted data                                 ####
#################################################################################

source('scripts/stepwise_VaR_ES_forecasting.R')

#################################################################################
####           Backtesting of forecasts                                      ####
#################################################################################

source('scripts/backtests.R')

#################################################################################
####           Visual inspection of forecasts                                ####
#################################################################################

# Plot all calculated models if plot_all_calc_models = TRUE
if(plot_all_calc_models){
  
  VaR.ES.plot <- list()
  
  for(index in indices){
    
    index_data <- get(index)
    
    for(speci in names(var.spec.list)){
      for(dist in names(dist.spec.list)){
        
        data <- data.frame(Date = index_data[['Date']],
                           Return = index_data[['Return']],
                           Exceeded_VaR = index_data[[paste0('Exceeded_VaR_', speci, '_', dist)]],
                           VaR = index_data[[paste0('VaR_', speci, '_', dist)]],
                           ES = index_data[[paste0('ES_', speci, '_', dist)]])
        
        VaR.ES.plot[[index]][[paste0(speci, '_', dist)]] <- ggplot(data =  data[-1:-(window_width + 2),],
                                                                   mapping = aes(x = Date,
                                                                                 y = Return)) +
          geom_point(aes(colour = as.factor(Exceeded_VaR))) +
          geom_line(aes(y = VaR),
                    col = 'orange') +
          geom_line(aes(y = ES),
                    col = 'purple') +
          scale_color_manual(values = c("1" = "red", "0" = "black"),
                             name = "Exceeded VaR",
                             
                             labels = c("No", "Yes")) +
          labs(title = paste0('VaR and ES vs historical returns: ', speci, '_', dist)) +
          theme(plot.title = element_text(hjust = 0.5))
      }
    }
  }
  saveRDS(VaR.ES.plot, file=paste0(ifelse(simulation, 'simulated_output/', 'output/'), 'VaR_ES_plot.RData'))
}


plot_1 = FALSE


# Other plots
if(plot_1){
  for(index in indices){
    data <- get(index)
    plot <- ggplot(data[-1:-(window_width + 2),], aes(x = as.Date(Date),
                                                y = Return)) +
      geom_point() +
      geom_line(aes(y = VaR_spec2_norm),
                col = 'red') +
      geom_line(aes(y = VaR_spec2_ged),
                col = 'green') +
      geom_line(aes(y = VaR_spec2_sstd),
                col = 'purple') +
      ggtitle(index) +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab('Date')
    print(plot)
  }
}

plot_2 = FALSE
if(plot_2){
ggplot(data =  DAX[-1:-(window_width + 2),],
       mapping = aes(x = Date,
                     y = Return)) +
  geom_point(aes(colour = as.factor(Exceeded_VaR_spec8_norm))) +
  geom_line(aes(y = VaR_spec8_norm),
            col = 'orange') +
  geom_line(aes(y = ES_spec8_norm),
            col = 'purple') +
  scale_color_manual(values = c("1" = "red", "0" = "black"),
                     name = "Exceeded VaR",
                     labels = c("No", "Yes"))
}

# Print finish time
Sys.time()

# Close connection to txt file
sink()



