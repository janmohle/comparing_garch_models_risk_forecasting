#################################################################################
####           Backtesting of forecasts                                      ####
#################################################################################

Backtest.results <- list()

for(index in indices){
  
  index.backtest.results <- list()
  index_data <- get(index)
  
  # VaR Kupiec backtest
  index.backtest.results[['VaR.Kupiec']] <- VaR_Kupiec_backtest(data = index_data,
                                                                tolerance_lvl = tolerance_lvl)
  
  # VaR Christofferson 1 backtest
  index.backtest.results[['VaR.Christofferson.1']] <- VaR_Christofferson1_backtest(data = index_data)
  
  # VaR Christofferson 2 backtest
  index.backtest.results[['VaR.Christofferson.2']] <- VaR_Christofferson2_backtest(data = index_data,
                                                                         tolerance_lvl = tolerance_lvl)
  
  # ES unconditional coverage backtest
  index.backtest.results[['ES.UC']] <- ES_uc_backtest(data = index_data,
                                                      tolerance_lvl = tolerance_lvl)
  
  # ES independence backtest
  index.backtest.results[['ES.indep']] <- ES_indep_backtest(data = index_data,
                                                            tolerance_lvl = tolerance_lvl,
                                                            lags = 5)
  
  Backtest.results[[index]] <- index.backtest.results
  
rm(index_data)
}
rm(index, index.backtest.results)

