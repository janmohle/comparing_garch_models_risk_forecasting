# TO DO:
# solve problem that integrate in stepwise_VaR_ES_forecating lead to an error in one observation
# solve problem that ghst distribution sometimes takes too long
# solve problem that 

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
length_data = 510

# Indices to include
start_index = 4
end_index = 4

source('scripts/stepwise_VaR_ES_forecasting.R')
execution_of_VaR_ES_forecasting()


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


# Loading forecasted VaR and ES
for(index in indices){
  index_name <- index
  data <- read.csv(paste0('output/', index_name, '_with_forecasted_VaR_ES.csv'))
  
  # Converting Date column to date value
  data <- data %>%
    mutate(Date = as.Date(Date))
  
  assign(index_name, data)
  rm(index_name, data)
}



VaR         ES         
2018-09-12 -0.05679853 -0.08371857
2018-09-13 -0.05626866 -0.08307191
2018-09-14 -0.05335943 -0.07867583
2018-09-15 -0.04497699 -0.06715522





# Plotting VaR and ES
ggplot(na.omit(DAX), aes(x = as.Date(Date),
                y = Return)) +
  geom_point() +
  geom_line(aes(y = VaR_spec3sstd),
            col = 'yellow') +
  geom_line(aes(y = ES_spec3sstd),
            col = 'green') +
  geom_line(aes(y = VaR_spec3snorm),
            col = 'pink') +
  geom_line(aes(y = ES_spec3snorm),
            col = 'orange') +
  geom_line(aes(y = VaR_spec3ged),
            col = 'red') +
  geom_line(aes(y = ES_spec3ged),
            col = 'purple')
