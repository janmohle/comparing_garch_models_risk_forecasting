################################################
#### Loss calculation for all tested models ####
################################################
Loss.results.all <- list()

for(index in indices){
  
  # Create new empty vector for each index
  Loss.results.all[[index]] <- vector()
  
  # Extract data and returns series
  data <- get(index)
  returns <- data[['Return']]
  
  for(speci in names(var.spec.list)){
    for(dist in names(dist.spec.list)){
      
      # Extract currently needed VaR and ES sequence
      VaR <- data[[paste0('VaR_', speci, '_', dist)]]
      ES <- data[[paste0('ES_', speci, '_', dist)]]
      
      # Execute loss function
      FZLoss.function.result <- FZLoss_func(returns = returns,
                                            VaR = VaR,
                                            ES = ES,
                                            alpha = tolerance_lvl,
                                            window_width = window_width)
      mean_loss <- FZLoss.function.result[['mean']]
      
      # Assign names of current loss
      names(mean_loss) <- paste0(speci, '_', dist)
      
      # Assign loss to Loss.results
      Loss.results.all[[index]] <- c(Loss.results.all[[index]], mean_loss)
    }
  }
  
  # Order results ascending
  Loss.results.all[[index]] <- sort(Loss.results.all[[index]])
}

rm(index, data, returns, speci, dist, VaR, ES, FZLoss.function.result, mean_loss)

############################################################################################
#### Loss calculation for models which have passed CC tests for VaR and ES with p > 0.1 ####
############################################################################################
Loss.results <- list()

for(index in indices){
  
  # Create new empty vector for each index
  Loss.results[[index]][['means']] <- vector()
  
  # Create list for loss sequences
  Loss.results[[index]][['sequence']] <- list()
  
  # Extract data and returns series
  data <- get(index)
  returns <- data[['Return']]
  
  for(speci in names(var.spec.list)){
    for(dist in names(dist.spec.list)){
      
      # Extracting VaR Christofferson 2 result for current model
      p_VaR_Chr2 <- Backtest.results[[index]][['VaR.Christofferson.2']][['p']][paste0(speci,'_', dist)]
      
      # Extracting ES CC robust test result for current model
      p_ES_CC <- Backtest.results[[index]][['ES.CC.robust']][['p']][paste0(speci,'_', dist)]
      
      # Only execute loss calculation if CC backtest didn't reject the model, else assign 9999
      if(p_VaR_Chr2>0.1 & p_ES_CC>0.1){

        # Extract currently needed VaR and ES sequence
        VaR <- data[[paste0('VaR_', speci, '_', dist)]]
        ES <- data[[paste0('ES_', speci, '_', dist)]]
        
        # Execute loss function
        FZLoss.function.result <- FZLoss_func(returns = returns,
                                                  VaR = VaR,
                                                  ES = ES,
                                                  alpha = tolerance_lvl,
                                                  window_width = window_width)
        mean_loss <- FZLoss.function.result[['mean']]
        loss_sequence <- FZLoss.function.result[['sequence']]
        
      } else {
        # Assign very 9999 to indicate that previous backtests failed
        mean_loss <- 9999
        loss_sequence <- NA
      }
      
      # Assign names of current loss
      names(mean_loss) <- paste0(speci, '_', dist)
      
      # Assign loss to Loss.results
      Loss.results[[index]][['means']] <- c(Loss.results[[index]][['means']], mean_loss)
      Loss.results[[index]][['sequence']][[paste0(speci, '_', dist)]] <- loss_sequence
      
    }
  }
  
  # Order results in ascending order
  Loss.results[[index]][['means']] <- sort(Loss.results[[index]][['means']])
}

rm(index, data, returns, speci, dist, p_VaR_Chr2, p_ES_CC, VaR, ES, FZLoss.function.result, mean_loss, loss_sequence)

###############################################################################
####         Create data frame with 10 best models                         ####
###############################################################################

best.models <- list()

for(index in indices){
  best.models[[index]] <- head(names(Loss.results[[index]][['means']]), 10)
  best.models[[paste0('FZ_Loss_', index)]]<- round(head(Loss.results[[index]][['means']], 10), 3)
}

# Transform list to data frame
best.models <- as.data.frame(best.models)
rownames(best.models) <- NULL

# Add column with p-value of Diebold-Mariano test for significant differences between best model and tested model
for(index in indices){
  
  # Extract loss sequence of best model
  best_loss_seq <- Loss.results[[index]][['sequence']][[best.models[[index]][1]]]
  
  for(i in 1:length(best.models[[index]])){
  
    if(i == 1){
      
      # Assign p value of 1 for best model
      best.models[[paste0('p_', index)]][i] <- 1
      
    } else {

      # Execute Diebold-Mariano test
      test_result <- DM_test(basis_loss_sequence = best_loss_seq,
                             compare_loss_sequence = Loss.results[[index]][['sequence']][[best.models[[index]][i]]])
      
      # Insert p value into best.models
      best.models[[paste0('p_', index)]][i] <- round(test_result[['p']], digits = 3)
    }
  }
}

rm(index, best_loss_seq, i, test_result)

# Sort best models manually
best.models <- best.models[, c('DAX', 'FZ_Loss_DAX', 'p_DAX', 'WIG', 'FZ_Loss_WIG', 'p_WIG', 'BTC', 'FZ_Loss_BTC', 'p_BTC', 'GLD', 'FZ_Loss_GLD', 'p_GLD')]


# Assign variance specification and distribution names
source('scripts/assign_names_ranking.R')


##################################################
#### Loss results view creation in data frame ####
##################################################

garch_models_in_order = c('ARCH', 'GARCH', 'EGARCH', 'GJR-GARCH', 'TGARCH', 'AVGARCH', 'APARCH', 'NGARCH', 'NAGARCH', 'FGARCH', 'CGARCH')
dist_names_in_order = c('NORM', 'STD', 'GED', 'SNORM', 'SSTD', 'SGED', 'GHYP', 'JSU', 'EMPIRICAL')

Loss.results.data.frames <- list()

for(index in indices){
  
  # Create data frame inside list for each index
  Loss.results.data.frames[[index]] <- data.frame()
  
  for(speci in names(var.spec.list)){
    for(dist in names(dist.spec.list)){
      
      # Insert loss value
      result_speci_dist <- round(Loss.results[[index]][['means']][paste0(speci, '_', dist)], digits = 3)
      Loss.results.data.frames[[index]][speci, dist] <- ifelse(is.na(result_speci_dist) | result_speci_dist == 9999, 'NP', result_speci_dist)
    }
  }
  
  # Rename rows and columns with more describtive names
  rownames(Loss.results.data.frames[[index]]) <- garch_models_in_order
  colnames(Loss.results.data.frames[[index]]) <- dist_names_in_order
}

rm(garch_models_in_order, dist_names_in_order, index, result_speci_dist)


# Create list and data frame for R and in color for latex of model ranks
source('scripts/ranking_in_data_frame.R')






