# TO DO:
# load all csv files

rm(list = ls())
if (dev.cur() != 1) {
  dev.off()
}
cat('\14')

library(tidyverse)
library(zoo)
library(rugarch)
library(FinTS)

source('scripts/preparing_data.R')
source('scripts/functions.R')
source('scripts/plots.R')

# Only uncomment, if stepwise variance forecast should be calculated all over again (takes multiple hours to run)
# Results can also be loaded, if the have already been calculated
# length_data subsets the input data (mainly used to test code, because whole data set takes a long time to run)
length_data = 550

# Indices to include
start_index = 2
end_index = 3

source('scripts/stepwise_VaR_ES_forecasting.R')
execution_of_VaR_ES_prediction()

# Index:  WIG  Spec:  2 , Dist:  ghyp

DAX.prices.plot
DAX.returns.plot
WIG.prices.plot
WIG.returns.plot
BTC.prices.plot
BTC.returns.plot
GOLD.prices.plot
GOLD.returns.plot

#main statistics
DAX.statistics <- ts.main.statistics(DAX$Return)
# -> significant autocorrelation
# -> significant ARCH effect
# -> leverage effect

WIG.statistics <- ts.main.statistics(WIG$Return)
# -> no significant autocorrelation
# -> significant ARCH effect
# -> leverage effect

BTC.statistics <- ts.main.statistics(BTC$Return)
# -> no significant autocorrelation
# -> significant ARCH effect
# -> no clear leverage effect

GOLD.statistics <- ts.main.statistics(GOLD$Return)
# -> no significant autocorrelation (but p < 0.1)
# -> significant ARCH effect
# -> no clear leverage effect


# Load stepwise forecasted VaR and ES
#DAX.forecasted.VaR <- read.csv('output/DAX_forecasted_VaR.csv')
#DAX.forecasted.ES  <- read.csv('output/DAX_forecasted_ES.csv')

WIG.forecasted.VaR <- read.csv('output/WIG_forecasted_VaR.csv')
WIG.forecasted.ES  <- read.csv('output/WIG_forecasted_ES.csv')

BTC.forecasted.VaR <- read.csv('output/BTC_forecasted_VaR.csv')
BTC.forecasted.ES  <- read.csv('output/BTC_forecasted_ES.csv')

GOLD.forecasted.VaR <- read.csv('output/GOLD_forecasted_VaR.csv')
GOLD.forecasted.ES  <- read.csv('output/GOLD_forecasted_ES.csv')



#load('output/DAX_forecasted_VaR.RData')
#load('output/WIG_forecasted_VaR.RData')
#load('output/BTC_forecasted_VaR.RData')
#load('output/GOLD_forecasted_VaR.RData')
#load('output/DAX_forecasted_ES.RData')
#load('output/WIG_forecasted_ES.RData')
#load('output/BTC_forecasted_ES.RData')
#load('output/GOLD_forecasted_ES.RData')






