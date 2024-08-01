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
# Controll fitting parameters for faster fitting, e.g. initial parameters and number of iterations



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

# ARMA(1,1) as mean model for all estimations (to capture as much dependencies of consecutive observations as possible while still keeping model parsimonious)
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
data_include = 1:600

# Indices
index_include = 1:4

# Variance specifications
varspec_include = 1:3

# Distribution assumptions
dist_include = c(-8, -9)

# Execution of VaR and ES forecast
  # TRUE: stepwise VaR and ES forecast calculates all over again (takes multiple hours to run)
  # FALSE: old results are being loaded from csv files in output
execution_of_VaR_ES_forecasting = FALSE
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

WIG.statistics <- ts_main_statistics(WIG$Return)
# -> no significant autocorrelation
# -> significant ARCH effect
# -> leverage effect

BTC.statistics <- ts_main_statistics(BTC$Return)
# -> no significant autocorrelation
# -> significant ARCH effect
# -> no clear leverage effect

GOLD.statistics <- ts_main_statistics(GOLD$Return)
# -> no significant autocorrelation (but p < 0.1)
# -> significant ARCH effect
# -> no clear leverage effect



#################################################################################
####           Testing of forecasts                                          ####
#################################################################################

# Kupiec test
for(index in indices){
result_name <- paste0(index, '.Kupiec.test.results')
index_data <- get(index)
result <- Kupiec_test(data = index_data,
                      tolerance_lvl = tolerance_lvl)
assign(result_name, result)
}

# Christofferson 1 test
for(index in indices){
result_name <- paste0(index, '.Christofferson.1.test.results')
index_data <- get(index)
result <- Christofferson1_test(data = index_data)
assign(result_name, result)
}

# Christofferson 2 test
for(index in indices){
result_name <- paste0(index, '.Christofferson.2.test.results')
index_data <- get(index)
result <- Christofferson2_test(data = index_data,
                               tolerance_lvl = tolerance_lvl)
assign(result_name, result)
}

# Write results of Christofferson 2 test of DAX forecasts to console
for(i in 1:length(GOLD.Christofferson.2.test.results)){
name <- names(GOLD.Christofferson.2.test.results[i])
value <- GOLD.Christofferson.2.test.results[[i]][['p_value']]
value <- round(value, 3)
message <- paste0(name, ': ', value, '\n\n')
cat(message)
} 



#################################################################################
####           Visual inspection of forecasts                                ####
#################################################################################

# Plotting VaR and ES
for(index in indices){
data <- get(index)
plot <- ggplot(na.omit(data), aes(x = as.Date(Date),
                                  y = Return)) +
        geom_point() +
        geom_line(aes(y = VaR_spec1snorm),
                  col = 'red') +
        geom_line(aes(y = VaR_spec1sged),
                  col = 'green') +
        geom_line(aes(y = VaR_spec2sstd),
                  col = 'purple') +
        ggtitle(index) +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab('Date')
        print(plot)
}
