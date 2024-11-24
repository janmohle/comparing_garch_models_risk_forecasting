#################################################################################
####           TO-DO-LIST                                                    ####
#################################################################################

# 1. mechanism that if empirical and error somewhere that VaR cannot be calculated -> NA in empirical distribution in other.quantities done


#DEFINITLY:
# 1. consider to change order of backtests (function itself shouldn't go through data -> this should be done outside of function -> function only returns result for one test)
# 2. implement corrected versions of test (with parameter 'corrected' in same function)

# Set correct solver control parameters (investigate how solver works)    done
# Maybe add second optimization if first fails     done
# take previous coefficiants as starting parameter for optimisation    done
# rerun every 100th optimization.   done
# think when to use global local combination!!   done

# create speci_dist in the beginning for whole function done

# continue insert NA into other.quantities done
# handle errors in gradient calculation -> improve paramters of grad(): consider first richardson and if this fails then simple

# implement corrected version of ES backtests (handle potential errors in gradient calculation first and think about how to deal with NA in other.quantities) (special case is empirical distribution)
# add parameter that decides which backtests to make

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
# Implementing ES backtest which is robust against estimation uncertainty 

# IDEAS:

# LATER:
# Recheck all needed packages


# Recently changed:


# NOTE:
# using combination of first global and then local optimiser leads to more accurat results but increases processing time a lot
# maybe taking previous parameter estimates as staring parameter reduces time a lot
# taking hybrid solver for all dist and only use other optimizer if hybrid fails

#################################################################################
####           General set-up                                                ####
#################################################################################

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
window_width = 500

# Tolerance level for VaR and ES
tolerance_lvl = 0.05

################################################################################
### Sub setting parameters for faster calculations in development period     ###
################################################################################

# Number of forecasts
number_forecasts = 50

# Input data - has to be higher than parameter window_width (comment out if not needed) (can be set directly or using parameter number_forecasts)
data_include = 1:(window_width+1+number_forecasts)

# Indices (comment out if not needed)
# Be careful when simulation = TRUE because indices are also subset then
index_include = c(3,4)

# Variance specifications (comment out if not needed)
varspec_include = c(1,2,3)

# Distribution assumptions (comment out if not needed)
dist_include = c(1, 8, 11)

# Should real data or simulated data be used? TRUE for simulated data
simulation = FALSE

# Number of simulations (specifiy if simulation = TRUE)
number_simulations = 2

# Execution of VaR and ES forecast
# TRUE: stepwise VaR and ES forecast calculates all over again (takes multiple hours to run)
# FALSE: old results are being loaded from csv files in output - not recommended to use with simulated data - index_include specifies which index data is loaded
execution_of_VaR_ES_forecasting = TRUE


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
                 'GOLD')
    }


# List of all variance specifications (ADD)
var.spec.list <- list(spec1 = list(model = 'sGARCH',
                                   garchOrder = c(arch, garch)),
                      spec2 = list(model = 'eGARCH',
                                   garchOrder = c(arch, garch)),
                      spec3 = list(model = 'gjrGARCH',
                                   garchOrder = c(arch, garch)))

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
                        nig = 'nig',        #8   # nig leads to problems (a lot of NaN in spec 3 for GOLD -> Should I exclude it)
                        ghst = 'ghst',      #9   # optimazation takes often too long with this distribution -> Sould I exclude it? --- df and skewness parameter interact in complicated way
                        jsu = 'jsu',        #10  # jsu leads to some problems (NaN in sigma in Spec 3 for GOLD -> Should I include it or not?)
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
                                                 lags_Ljung_Box_test = 15,
                                                 lags_ArchTest = 12,
                                                 nu = 5)
}
rm(index)

# DAX
  # -> significant autocorrelation
  # -> significant ARCH effect
  # -> leverage effect
  # -> mean model could potentially be usefull
  
# WIG
  # -> no significant autocorrelation
  # -> significant ARCH effect
  # -> leverage effect
  # NO MEAN MODEL NEEDED
  
# BTC
  # -> no significant autocorrelation
  # -> significant ARCH effect
  # -> no clear leverage effect
  # NO MEAN MODEL NEEDED
  
# GOLD
  # -> no significant autocorrelation (but p < 0.1)
  # -> significant ARCH effect
  # -> no clear leverage effect
  # NO MEAN MODEL NEEDED

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
ggplot(data =  BTC[-1:-(window_width + 2),],
       mapping = aes(x = Date,
                     y = Return)) +
  geom_point(aes(colour = as.factor(Exceeded_VaR_spec1_norm))) +
  geom_line(aes(y = VaR_spec1_norm),
            col = 'orange') +
  geom_line(aes(y = ES_spec1_norm),
            col = 'purple') +
  scale_color_manual(values = c("1" = "red", "0" = "black"),
                     name = "Exceeded VaR",
                     labels = c("No", "Yes"))
}

