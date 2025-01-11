#################################################################################
####           TO-DO-LIST                                                    ####
#################################################################################

#DEFINITLY:

# Recheck whole process and delete parts which are not needed anymore

# Reconcider way how to test model with historical distribution
# In the gradient calculation of mu and sigma, should it really be the forecasted value or the fitted value??? -> probably yes

# continue insert NA into other.quantities done
# handle errors in gradient calculation -> improve paramters of grad(): consider first richardson and if this fails then simple

# implement corrected version of ES backtests (handle potential errors in gradient calculation first and think about how to deal with NA in other.quantities) (special case is empirical distribution)

# Check if backtest for empirical violations makes sense

# (Investigate error if Execution is TRUE for real data)

# NOTES:
# old2 are good data sets

# NOT NEEDED ANYMORE IF NO INTERPOLATION:
# Implement way to handle remaining NAs and give out more information where values got interpolated!!! -> vector with date as value and column name as entry name? different vectors for fitting error, integration error, etc.
# Join vectors with NA information with final table for lead and receive correct date where NAs are interpolated

# MAYBE:
# Create vector to store iteration time of each iteration
# Handle convergence information
# Include loss function for comparison if backtest don't come to satisfactory result
# Make simulation more dynamic in terms of specifications


# TEST OF WHOLE MODEL:
# Test multiple simulations and check if results are logical
# Test different parameters of program

# CHALLANGES:

# IDEAS:

# LATER:
# Recheck all needed packages


# Recently changed:


# NOTE:
# using combination of first global and then local optimiser leads to more accurat results but increases processing time a lot
# maybe taking previous parameter estimates as staring parameter reduces time a lot
# taking hybrid solver for all dist and only use other optimizer if hybrid fails
# First return in input data set has to be NA that the program works correctly

#################################################################################
####           General set-up                                                ####
#################################################################################

# When using code for the first time, use renv::restore() to install all packages according to the renv.lock file!
# A dialog will pop up and ask if all listed packages should be installed. Enter 'Y' in console. Packages are then installed with correct version.

# Initial cleaning
rm(list = ls())
if (dev.cur() != 1) {
  dev.off()
}
cat('\14')

# Loading required libraries
library(tidyverse)
library(zoo)
library(rugarch)
library(FinTS)
library(numDeriv)
library(moments)
library(tseries)

# Function definition
source('scripts/functions.R')


#################################################################################
####           General parameter assignment                                  ####
#################################################################################

# Number of autoregressiv terms for mean
ar = 0

# Number of moving average terms for mean
ma = 0

# Number of ARCH terms for variance
arch = 1

# Number of GARCH terms for variance
garch = 1

# Width of estimation window for rolling forecasting
window_width = 750

# Tolerance level for VaR and ES
tolerance_lvl = 0.05

################################################################################
### Sub setting parameters for faster calculations in development period     ###
################################################################################

# Number of forecasts
number_forecasts = 2000

# Input data - has to be higher than parameter window_width (comment out if not needed) (can be set directly or using parameter number_forecasts)
#data_include = 1:(window_width+1+number_forecasts)

# Indices (comment out if not needed)
# Be careful when simulation = TRUE because indices are also subset then
#index_include = c(1)

# Variance specifications (comment out if not needed)
varspec_include = c(1)

# Distribution assumptions (comment out if not needed)
dist_include = c(1)

# Should real data or simulated data be used? TRUE for simulated data
simulation = TRUE

# Number of simulations (specifiy if simulation = TRUE)
number_simulations = 3

# Execution of VaR and ES forecast
# TRUE: stepwise VaR and ES forecast calculates all over again (takes multiple hours to run)
# FALSE: old results are being loaded from csv files in output - not recommended to use with simulated data - index_include specifies which index data is loaded
execution_of_VaR_ES_forecasting = TRUE

# Execution of VaR and ES Backtests
execute_Backtest = TRUE

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


# List of all variance specifications (ADD)
var.spec.list <- list(spec1 = list(model = 'sGARCH',
                                   garchOrder = c(arch, garch)),
                      spec2 = list(model = 'eGARCH',
                                   garchOrder = c(arch, garch)),
                      spec3 = list(model = 'gjrGARCH',
                                   garchOrder = c(arch, garch)),
                      spec4 = list(model = 'apARCH',
                                   garchOrder = c(arch, garch)),
                      spec5 = list(model = 'csGARCH',
                                   garchOrder = c(arch, garch)),
                      spec6 = list(model = 'fGARCH',
                                   garchOrder = c(arch, garch),
                                   submodel = 'NAGARCH'),
                      spec7 = list(model = 'fGARCH',
                                   garchOrder = c(arch, garch),
                                   submodel = 'ALLGARCH'),
                      spec8 = list(model = 'fGARCH',
                                   garchOrder = c(arch, garch),
                                   submodel = 'NGARCH'))

# Mean specification
mean.spec <- list(armaOrder = c(ar,ma),
                  include.mean = TRUE)

# Constant mean for data without significant autocorrelation (random walk with potential drift)
# AR AND MA TERMS WILL PROBABLY NOT BE USED 
# ARMA(1,1) as mean model for data with significant autocorrelation (based on Ljung-Box-Test)
# !! for text: arma model first tried -> lead in some cases to unreasonable unstable mean estimates -> constant mean is theoretical explainable and also act and pacf show that there doesnt seem to be strucutal autocorrelation which adds value to estimation
# coefficient usually equal each other out (e.g. positive ar and negative ma)
# show plot of mean as proof??

# List of all distribution assumptions
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
                        empirical = 'norm') #11  # Normal distribution for QML estimation -> asymptotically consistent (USE IN TEXT AS JUSTIFICATION AND SEARCH FOR REFERENCE)


#################################################################################
####  Data preparation and subsetting of data and specifications             ####
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
# - Serial autocorrelation seems to be non existed or very slight -> mostly uninformative, although direction in BTC interesting
# - Volatility clustering always existent
# - leverage effect seems to be stronger in stock indices than in BTC and espacially stronger than in GLD
# - Standardized returns are not normally distributed (excess kurtosis and negative skewness)
# - slightly positive mean and median of returns (DAX, WIG, GLD: 1e-4, BTC: 1e-3) -> slight positive drift

#################################################################################
####  Forecasting or loading of forecasted values                            ####
#################################################################################

source('scripts/stepwise_VaR_ES_forecasting.R')


#################################################################################
####           Backtesting of forecasts                                      ####
#################################################################################

source('scripts/backtests.R')

#################################################################################
####           Visual inspection of forecasts                                ####
#################################################################################
plotting_1 = FALSE

if(plotting_1){
  # Plotting VaR and ES
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

plotting_2 = F
if(plotting_2){
# Nice plot of VaR and ES for WIG spec2 sged
ggplot(data =  GLD[-1:-(window_width + 2),],
       mapping = aes(x = Date,
                     y = Return)) +
  geom_point(aes(colour = as.factor(Exceeded_VaR_spec8_sged))) +
  geom_line(aes(y = VaR_spec8_sged),
            col = 'orange') +
  geom_line(aes(y = ES_spec8_sged),
            col = 'purple') +
  scale_color_manual(values = c("1" = "red", "0" = "black"),
                     name = "Exceeded VaR",
                     labels = c("No", "Yes"))
}

