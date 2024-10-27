#setwd('/Users/janmohle/Library/CloudStorage/OneDrive-Personal/01Studium/01Göttingen/08SoSe_24/Bachelorarbeit/comparing_garch_risk_forecasting')

#needed libraries
#library(tidyverse)


if(simulation){
  
  #Execute simulation
  source('scripts/simulate_data.R')
  
  # Read simulated data (simulated in simulate_data.R)
  for(i in 1:number_simulations){
    
    sim_i_name <- paste0('sim', i)
    sim_i_data <- read.csv(paste0('simulated_data/', sim_i_name, '.csv'), header = TRUE)
    sim_i_data <- sim_i_data %>%
      mutate(Date = as.Date(Date))
    assign(sim_i_name, sim_i_data)
  }
  rm(sim_i_name, sim_i_data, i)
  
  } else {
    # Read real data
    
    #DAX from https://www.google.com/finance/quote/DAX:INDEXDB?hl=en
    DAX <- read.csv('data/DAX.csv')
    DAX <- DAX %>%
      mutate(Date = substring(Date, 1, nchar(Date) - 9)) %>%
      mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
      rename(Price = Close) %>%
      mutate(Return = c(NA, diff(log(Price))))
    
    #WIG from https://www.google.com/finance/quote/WIG:WSE?hl=en
    WIG <- read.csv('data/WIG.csv')
    WIG <- WIG %>%
      mutate(Date = substring(Date, 1, nchar(Date) - 9)) %>%
      mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
      mutate(Close = ifelse(Close > 1000000, Close / 100, Close)) %>% #some data points had decimal at wrong position
      rename(Price = Close) %>%
      mutate(Return = c(NA, diff(log(Price))))
    
    #Bitcoin from https://finance.yahoo.com/quote/BTC-USD
    BTC <- read.csv('data/BTCUSD.csv')
    BTC <- BTC %>%
      dplyr::select(Date, Close) %>%
      mutate(Date = as.Date(Date, format = '%Y-%m-%d')) %>%
      rename(Price = Close) %>%
      mutate(Return = c(NA, diff(log(Price))))
    
    #Gold spot prices in USD from https://www.investing.com/currencies/xau-usd-historical-data
    GOLD <- read.csv('data/XAUUSD.csv', dec = '.') %>%
      dplyr::select(Date, Price) %>%
      mutate(Price = gsub(',', '', Price)) %>%
      mutate(Price = as.numeric(Price)) %>%
      mutate(Date = as.Date(Date, format = '%m/%d/%Y')) %>%
      arrange(Date) %>%
      mutate(Return = c(NA, diff(log(Price))))
  }




