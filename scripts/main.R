#################################################################################
####           TO-DO-LIST                                                    ####
#################################################################################

# solve problem that integrate in stepwise_VaR_ES_forecating lead to an error in one observation
# solve problem that ghst distribution sometimes takes too long -> Exclude it?
# solve problem that of a lot of NaN in jsu and nig (sigma cannot be forecasted) -> Exclude?
# SOLVE problem of WIG spec 3 ghyp jsu: part of error marked: leading and tailing NAs are lost: DURING INTERPOLATION set na.rm = FALSE
# Implement way to handle remaining NAs and give out more information where values got interpolated!!! -> vector with date as value and column name as entry name? different vectors for fitting error, integration error, etc.
# Include LR in test output
# Create vector to store iteration time of each iteration
# Join vectors with NA information with final table for lead and recieve correct date where NAs are interpolated
# Set controlling value for running predicions and put function execution into forecast scipt
# Control fitting parameters for faster fitting, e.g. initial parameters and number of iterations



# Handle warning messages
# Put price plots and return plots into one object and handle warnings
# Handle convergence information
# old2 are good data sets

# COMMIT MESSAGE: Adjust solver control parameters in ugarchfit: Changes made to functions.R



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

# Data preparation
source('scripts/preparing_data.R')

# Function definition
source('scripts/functions.R')

# Initial descriptive plots
source('scripts/plots.R')



#################################################################################
####           Model specification set-up                                    ####
#################################################################################

# Vector of all index names
indices <- c('DAX',
             'WIG',
             'BTC',
             'GOLD')

# List of all variance specifications (ADD)
var.spec.list <- list(spec1 = list(model = 'sGARCH',
                                   garchOrder = c(1,1)),
                      spec2 = list(model = 'eGARCH',
                                   garchOrder = c(1,1)),
                      spec3 = list(model = 'gjrGARCH',
                                   garchOrder = c(1, 1)))

# Constant mean for data without significant autocorrelation (random walk with potential drift)
constantmean <- list(armaOrder = c(0,0),
                     include.mean = TRUE)

# WILL PROBABLY NOT BE USED 
# ARMA(1,1) as mean model for data with significant autocorrelation (based on Ljung-Box-Test)
# !! for text: arma model first tried -> lead in some cases to unreasonable unstable mean estimates -> constant mean is theoretical explainable and also act and pacf show that there doesnt seem to be strucutal autocorrelation which adds value to estimation
# coefficient usually equal each other out (e.g. positive ar and negative ma)
# show plot of mean as proof??
armamean <- list(armaOrder = c(1,1),
                 include.mean = TRUE)

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


# Specifying width of estimation window for rolling forecasting
width = 500

# Specifying tolerance level
tolerance_lvl = 0.05


#################################################################################
## Sub setting of specifications for faster calculations in development period ##
#################################################################################

# Input data
data_include = 1:515

# Indices
index_include = 4

# Variance specifications
varspec_include = c(1,3)

# Distribution assumptions
dist_include = c(8)

# Execution of VaR and ES forecast
  # TRUE: stepwise VaR and ES forecast calculates all over again (takes multiple hours to run)
  # FALSE: old results are being loaded from csv files in output
execution_of_VaR_ES_forecasting = TRUE
source('scripts/stepwise_VaR_ES_forecasting.R')

#################################################################################
####           Descriptive part                                              ####
#################################################################################

# Price and Return curves 
DAX.prices.plot
DAX.returns.plot
WIG.prices.plot
WIG.returns.plot
BTC.prices.plot
BTC.returns.plot
GOLD.prices.plot
GOLD.returns.plot

# Main descriptive statistics 
DAX.statistics <- ts_main_statistics(DAX$Return)
# -> significant autocorrelation
# -> significant ARCH effect
# -> leverage effect
# -> mean model could potentially be usefull

WIG.statistics <- ts_main_statistics(WIG$Return)
# -> no significant autocorrelation
# -> significant ARCH effect
# -> leverage effect
# NO MEAN MODEL NEEDED

BTC.statistics <- ts_main_statistics(BTC$Return)
# -> no significant autocorrelation
# -> significant ARCH effect
# -> no clear leverage effect
# NO MEAN MODEL NEEDED

GOLD.statistics <- ts_main_statistics(GOLD$Return)
# -> no significant autocorrelation (but p < 0.1)
# -> significant ARCH effect
# -> no clear leverage effect
# NO MEAN MODEL NEEDED


#################################################################################
####           Testing of forecasts                                          ####
#################################################################################

# VaR Kupiec backtest
for(index in indices){
result_name <- paste0(index, '.VaR.Kupiec.backtest.results')
index_data <- get(index)
result <- VaR_Kupiec_backtest(data = index_data,
                              tolerance_lvl = tolerance_lvl)
assign(result_name, result)
}

# VaR Christofferson 1 backtest
for(index in indices){
result_name <- paste0(index, '.VaR.Christofferson.1.backtest.results')
index_data <- get(index)
result <- VaR_Christofferson1_backtest(data = index_data)
assign(result_name, result)
}

# VaR Christofferson 2 backtest
for(index in indices){
result_name <- paste0(index, '.VaR.Christofferson.2.backtest.results')
index_data <- get(index)
result <- VaR_Christofferson2_backtest(data = index_data,
                                       tolerance_lvl = tolerance_lvl)
assign(result_name, result)
}

# ES unconditional coverage backtest
for(index in indices){
  result_name <- paste0(index, '.ES.UC.backtest.results')
  index_data <- get(index)
  result <- ES_uc_backtest(data = index_data,
                           tolerance_lvl = tolerance_lvl)
  assign(result_name, result)
}

# ES independence backtest
for(index in indices){
  result_name <- paste0(index, '.ES.indep.backtest.results')
  index_data <- get(index)
  result <- ES_indep_backtest(data = index_data,
                              tolerance_lvl = tolerance_lvl,
                              lags = 5)
  assign(result_name, result)
}

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
ggplot(data =  WIG[-1:-502,],
       mapping = aes(x = Date,
                     y = Return)) +
  geom_point(aes(colour = as.factor(Exceeded_VaR_spec2_sstd))) +
  geom_line(aes(y = VaR_spec2_sstd),
            col = 'orange') +
  geom_line(aes(y = ES_spec2_sstd),
            col = 'purple') +
  scale_color_manual(values = c("1" = "red", "0" = "black"),
                     name = "Exceeded VaR",
                     labels = c("No", "Yes"))
}

   