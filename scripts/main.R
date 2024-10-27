#################################################################################
####           TO-DO-LIST                                                    ####
#################################################################################
# NOTE:
# old2 are good data sets

# NOT NEEDED ANYMORE IF NO INTERPOLATION:
# Implement way to handle remaining NAs and give out more information where values got interpolated!!! -> vector with date as value and column name as entry name? different vectors for fitting error, integration error, etc.
# Join vectors with NA information with final table for lead and recieve correct date where NAs are interpolated

# MAYBE:
# Create vector to store iteration time of each iteration
# Handle convergence information

#DEFINITLY:
# Try other n.ahead
# Put backtests into one object (Backtests -> index -> tests)
# check that window length can be adjusted everywhere
# Investigate error if Execution is TRUE for real data

# Implement distribution free innovations -> use quantiles for VaR (and ES)




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
width = 500

# Tolerance level for VaR and ES
tolerance_lvl = 0.05

################################################################################
### Sub setting parameters for faster calculations in development period     ###
################################################################################

# Input data (comment out if not needed)
data_include = 1:520

# Indices (comment out if not needed)
#index_include = 2

# Variance specifications (comment out if not needed)
varspec_include = c(1)

# Distribution assumptions (comment out if not needed)
dist_include = c(1, 2)

# Should real data or simulated data be used? TRUE for simulated data
simulation = FALSE

# Number of simulations (specifiy if simulation = TRUE)
number_simulations = 3

# Execution of VaR and ES forecast
# TRUE: stepwise VaR and ES forecast calculates all over again (takes multiple hours to run)
# FALSE: old results are being loaded from csv files in output
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
dist.spec.list <-  list(distr1 = 'norm',
                        distr2 = 'std',
                        distr3 = 'ged',
                        distr4 = 'snorm',
                        distr5 = 'sstd',
                        distr6 = 'sged',
                        distr7 = 'ghyp',
                        distr8 = 'nig', # nig leads to problems (a lot of NaN in spec 3 for GOLD -> Should I exclude it)
                        distr9 = 'ghst',  # optimazation takes often too long with this distribution -> Sould I exclude it?
                        distr10 = 'jsu') # jsu leads to some problems (NaN in sigma in Spec 3 for GOLD -> Should I include it or not?)


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

# VaR Kupiec backtest
for(index in indices){
result_name <- paste0(index, '.VaR.Kupiec.backtest.results')
index_data <- get(index)
result <- VaR_Kupiec_backtest(data = index_data,
                              tolerance_lvl = tolerance_lvl)
assign(result_name, result)
}
rm(result_name, index_data, result, index)

# VaR Christofferson 1 backtest
for(index in indices){
result_name <- paste0(index, '.VaR.Christofferson.1.backtest.results')
index_data <- get(index)
result <- VaR_Christofferson1_backtest(data = index_data)
assign(result_name, result)
}
rm(result_name, index_data, result, index)

# VaR Christofferson 2 backtest
for(index in indices){
result_name <- paste0(index, '.VaR.Christofferson.2.backtest.results')
index_data <- get(index)
result <- VaR_Christofferson2_backtest(data = index_data,
                                       tolerance_lvl = tolerance_lvl)
assign(result_name, result)
}
rm(result_name, index_data, result, index)

# ES unconditional coverage backtest
for(index in indices){
  result_name <- paste0(index, '.ES.UC.backtest.results')
  index_data <- get(index)
  result <- ES_uc_backtest(data = index_data,
                           tolerance_lvl = tolerance_lvl)
  assign(result_name, result)
}
rm(result_name, index_data, result, index)

# ES independence backtest
for(index in indices){
  result_name <- paste0(index, '.ES.indep.backtest.results')
  index_data <- get(index)
  result <- ES_indep_backtest(data = index_data,
                              tolerance_lvl = tolerance_lvl,
                              lags = 5)
  assign(result_name, result)
}
rm(result_name, index_data, result, index)

#################################################################################
####           Visual inspection of forecasts                                ####
#################################################################################
plotting_1 = FALSE

if(plotting_1){
  # Plotting VaR and ES
  for(index in indices){
    data <- get(index)
    plot <- ggplot(data[-1:- (width + 2),], aes(x = as.Date(Date),
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

plotting_2 = FALSE
if(plotting_2){
# Nice plot of VaR and ES for WIG spec2 sged
ggplot(data =  sim1[-1:-502,],
       mapping = aes(x = Date,
                     y = Return)) +
  geom_point(aes(colour = as.factor(Exceeded_VaR_spec1_sstd))) +
  geom_line(aes(y = VaR_spec1_sstd),
            col = 'orange') +
  geom_line(aes(y = ES_spec1_sstd),
            col = 'purple') +
  scale_color_manual(values = c("1" = "red", "0" = "black"),
                     name = "Exceeded VaR",
                     labels = c("No", "Yes"))
}

