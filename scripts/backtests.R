#################################################################################
####           Backtesting of forecasts                                      ####
#################################################################################

execution_of_Backtests <- function(){
  
  # Empty result list
  Backtest_results <- list()
  
  # Loop over indices
  for(index in indices){
    
    # Extract data of current index
    index_data <- get(index)
    
    # Create empty result list for each backtest
    VaR_Kupiec_results <- list(LR = vector(),
                               p = vector())
    VaR_Christofferson1_results <- list(LR = vector(),
                                        p = vector())
    VaR_Christofferson2_results <- list(LR = vector(),
                                        p = vector())
    ES_UC_results <- list(U = vector(),
                          p = vector())
    ES_UC_results_robust  <- list(U = vector(),
                                  p = vector())
    ES_CC_results <- list(C = vector(),
                          p = vector())
    ES_CC_results_robust <- list(C = vector(),
                                 p = vector())
    
    # Loop over each column of data of current index
    for(col in names(index_data)){
      
      # VaR backtests
      # Test if column consists of exceedences
      is_name_Exceeded <- substr(col, 1, 8)
      if(is_name_Exceeded == 'Exceeded'){
        speci_dist <- substr(col, 14, nchar(col))
        
        # Kupiec backtest execution
        col_result <- VaR_Kupiec_backtest(exceedences = index_data[[col]],
                                          speci_dist = speci_dist,
                                          tolerance_lvl = tolerance_lvl)
        VaR_Kupiec_results[['LR']] <- c(VaR_Kupiec_results[['LR']], col_result[['LR']])
        VaR_Kupiec_results[['p']] <- c(VaR_Kupiec_results[['p']], col_result[['p']])
        
        # Christofferson 1 backtest execution
        col_result <- VaR_Christofferson1_backtest(exceedences = index_data[[col]],
                                                   speci_dist = speci_dist)
        VaR_Christofferson1_results[['LR']] <- c(VaR_Christofferson1_results[['LR']], col_result[['LR']])
        VaR_Christofferson1_results[['p']] <- c(VaR_Christofferson1_results[['p']], col_result[['p']])
        
        # Christofferson 2 backtest execution
        col_result <- VaR_Christofferson2_backtest(exceedences = index_data[[col]],
                                                   speci_dist = speci_dist,
                                                   tolerance_lvl = tolerance_lvl)
        VaR_Christofferson2_results[['LR']] <- c(VaR_Christofferson2_results[['LR']], col_result[['LR']])
        VaR_Christofferson2_results[['p']] <- c(VaR_Christofferson2_results[['p']], col_result[['p']])
      }
      
      # ES backtests
      # Test if column consists of cumulative violations
      is_CumVio <- substr(col, 1, 6)
      if(is_CumVio == 'CumVio'){
        speci_dist <- substr(col, 8, nchar(col))
        
        
        # Unconditional coverage backtest not robust
        col_result <- ES_uc_backtest(CumVio = index_data[[col]],
                                     speci_dist = speci_dist,
                                     tolerance_lvl = tolerance_lvl,
                                     robust = FALSE)
        ES_UC_results[['U']] <- c(ES_UC_results[['U']], col_result[['U']])
        ES_UC_results[['p']] <- c(ES_UC_results[['p']], col_result[['p']])

        # Unconditional coverage backtest robust
        col_result <- ES_uc_backtest(CumVio = index_data[[col]],
                                     speci_dist = speci_dist,
                                     tolerance_lvl = tolerance_lvl,
                                     robust = TRUE,
                                     index = index,
                                     cov_matrices = other.quantities[[index]][[speci_dist]][['cov_matrix']],
                                     window_width = window_width,
                                     grad_mu = other.quantities[[index]][[speci_dist]][['grad_mu']],
                                     grad_sigma = other.quantities[[index]][[speci_dist]][['grad_sigma']],
                                     r = index_data[['Return']],
                                     mu = index_data[[paste0('mu_', speci_dist)]],
                                     sigma = index_data[[paste0('sigma_', speci_dist)]],
                                     skew = index_data[[paste0('skew_', speci_dist)]],
                                     shape = index_data[[paste0('shape_', speci_dist)]],
                                     dist = index_data[[paste0('dist_', speci_dist)]])
        ES_UC_results_robust[['U']] <- c(ES_UC_results_robust[['U']], col_result[['U']])
        ES_UC_results_robust[['p']] <- c(ES_UC_results_robust[['p']], col_result[['p']])

        # Conditional coverage backtest not robust
        col_result <- ES_cc_backtest(CumVio = index_data[[col]],
                                     speci_dist = speci_dist,
                                     tolerance_lvl = tolerance_lvl,
                                     lags = 5,
                                     robust = FALSE)
        ES_CC_results[['C']] <- c(ES_CC_results[['C']], col_result[['C']])
        ES_CC_results[['p']] <- c(ES_CC_results[['p']], col_result[['p']])
        
        # Conditional coverage backtest robust
        col_result <- ES_cc_backtest(CumVio = index_data[[col]],
                                     speci_dist = speci_dist,
                                     tolerance_lvl = tolerance_lvl,
                                     lags = 5,
                                     robust = TRUE,
                                     index = index,
                                     cov_matrices = other.quantities[[index]][[speci_dist]][['cov_matrix']],
                                     window_width = window_width,
                                     grad_mu = other.quantities[[index]][[speci_dist]][['grad_mu']],
                                     grad_sigma = other.quantities[[index]][[speci_dist]][['grad_sigma']],
                                     r = index_data[['Return']],
                                     mu = index_data[[paste0('mu_', speci_dist)]],
                                     sigma = index_data[[paste0('sigma_', speci_dist)]],
                                     skew = index_data[[paste0('skew_', speci_dist)]],
                                     shape = index_data[[paste0('shape_', speci_dist)]],
                                     dist = index_data[[paste0('dist_', speci_dist)]])
        ES_CC_results_robust[['C']] <- c(ES_CC_results_robust[['C']], col_result[['C']])
        ES_CC_results_robust[['p']] <- c(ES_CC_results_robust[['p']], col_result[['p']])
      }
    }
    
    # Store results in final list
    Backtest_results[[index]] <- list(VaR.Kupiec = VaR_Kupiec_results,
                                      VaR.Christofferson.1 = VaR_Christofferson1_results,
                                      VaR.Christofferson.2 = VaR_Christofferson2_results,
                                      ES.UC = ES_UC_results,
                                      ES.UC.robust = ES_UC_results_robust,
                                      ES.CC = ES_CC_results,
                                      ES.CC.robust = ES_CC_results_robust)
  }
  # Return results
  return(Backtest_results)
}


# Execution of backtests

if(execute_Backtest){
  Backtest.results <- execution_of_Backtests()
}



