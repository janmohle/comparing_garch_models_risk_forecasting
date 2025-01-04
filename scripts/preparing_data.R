#################################################################################
####           Preparing data                                                ####
#################################################################################

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
    
    # Subsets indices for faster step wise forecasting if index_include specified
    if(exists('index_include')){
      indices <- indices[index_include]
    }
  
    #DAX from https://www.google.com/finance/quote/DAX:INDEXDB?hl=en (downloaded in Google Sheets with: =GOOGLEFINANCE("INDEXDB:DAX", "price", DATE(2016,12,1), DATE(2024,11,30), "DAILY"))
    if('DAX' %in% indices){
      DAX <- read.csv('data/DAX.csv')
      DAX <- DAX %>%
        mutate(Date = substring(Date, 1, nchar(Date) - 9)) %>%
        mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
        mutate(Close = ifelse(Close > 1000000, Close / 100, Close)) %>% #some data points had decimal at wrong position
        rename(Price = Close) %>%
        mutate(Return = c(NA, diff(log(Price))))
    }

    #WIG from https://www.google.com/finance/quote/WIG:WSE?hl=en (downloaded in Google Sheets with: =GOOGLEFINANCE("WSE:WIG", "price", DATE(2016,12,1), DATE(2024,11,30), "DAILY"))
    if('WIG' %in% indices){
      WIG <- read.csv('data/WIG.csv')
      WIG <- WIG %>%
        mutate(Date = substring(Date, 1, nchar(Date) - 9)) %>%
        mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
        mutate(Close = ifelse(Close > 1000000, Close / 100, Close)) %>% #some data points had decimal at wrong position
        rename(Price = Close) %>%
        mutate(Return = c(NA, diff(log(Price))))
    }
    
    #Bitcoin historical data downloaded from https://www.investing.com/currencies/xau-usd-historical-data
    if('BTC' %in% indices){
      BTC <- read.csv('data/BTCUSD.csv') %>%
        dplyr::select(Date, Price) %>%
        mutate(Price = gsub(',', '', Price)) %>%
        mutate(Price = as.numeric(Price)) %>%
        mutate(Date = as.Date(Date, format = '%m/%d/%Y')) %>%
        arrange(Date) %>%
        mutate(Return = c(NA, diff(log(Price))))
    }
    
    #Gold spot prices in USD historical data downloaded from https://www.investing.com/crypto/bitcoin/historical-data
    if('GLD' %in% indices){
      GLD <- read.csv('data/XAUUSD.csv', dec = '.') %>%
        dplyr::select(Date, Price) %>%
        mutate(Price = gsub(',', '', Price)) %>%
        mutate(Price = as.numeric(Price)) %>%
        mutate(Date = as.Date(Date, format = '%m/%d/%Y')) %>%
        arrange(Date) %>%
        mutate(Return = c(NA, diff(log(Price))))
    }
  }




