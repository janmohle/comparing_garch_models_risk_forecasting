########################################################
#### View creation of backtest results              ####
########################################################

garch_models_in_order = c('ARCH', 'GARCH', 'EGARCH', 'GJR-GARCH', 'TGARCH', 'AVGARCH', 'APARCH', 'NGARCH', 'NAGARCH', 'FGARCH', 'CGARCH')
dist_names_in_order = c('NORM', 'STD', 'GED', 'SNORM', 'SSTD', 'SGED', 'GHYP', 'JSU', 'EMPIRICAL')

Backtest.data.frames <- list()

for(index in names(Backtest.results)){
  Backtest.data.frames[[index]] <- list()
  
  for(backtest in names(Backtest.results[[index]])){
    Backtest.data.frames[[index]][[backtest]] <- data.frame()
    
    # Only include calculated backtests
    if(length(Backtest.results[[index]][[backtest]][['p']]) > 0){
      
      for(speci in names(var.spec.list)){
        for(dist in names(dist.spec.list)){
          Backtest.data.frames[[index]][[backtest]][speci, dist] <- round(Backtest.results[[index]][[backtest]][['p']][paste0(speci, '_', dist)], digits = 3)
        }
      }
      
      rownames(Backtest.data.frames[[index]][[backtest]]) <- garch_models_in_order
      colnames(Backtest.data.frames[[index]][[backtest]]) <- dist_names_in_order

    }
    
  }
}

#################################################################
#### View creation of backtest results with colors in latex  ####
#################################################################

Backtest.data.frames.color <- list()

for(index in names(Backtest.results)){
  Backtest.data.frames.color[[index]] <- list()
  
  for(backtest in names(Backtest.results[[index]])){
    Backtest.data.frames.color[[index]][[backtest]] <- data.frame()
    
    # Only include calculated backtests
    if(length(Backtest.results[[index]][[backtest]][['p']]) > 0){
      
      for(speci in names(var.spec.list)){
        for(dist in names(dist.spec.list)){
          
          p_value <- round(Backtest.results[[index]][[backtest]][['p']][paste0(speci, '_', dist)], digits = 3)
          
          # Color assignment

          # NA
          if(is.na(p_value) | is.nan(p_value)){
            p_value_with_color <- 'NA'
          } else {
            
            # red
            if(p_value <= 0.05){
              p_value_with_color <- paste0('\\textcolor{red}{', sprintf('%.3f', p_value) , '}')
            }
            
            # orange
            if(p_value > 0.05 & p_value <= 0.1 ){
              p_value_with_color <- paste0('\\textcolor{orange}{', sprintf('%.3f', p_value) , '}')
            }
            
            # teal
            if(p_value > 0.1){
              p_value_with_color <- paste0('\\textcolor{teal}{', sprintf('%.3f', p_value) , '}')
            }
          }
          
          Backtest.data.frames.color[[index]][[backtest]][speci, dist] <- p_value_with_color
        }
      }
      
      rownames(Backtest.data.frames.color[[index]][[backtest]]) <- garch_models_in_order
      colnames(Backtest.data.frames.color[[index]][[backtest]]) <- dist_names_in_order
      
    }
    
  }
}

rm(garch_models_in_order, dist_names_in_order, index, backtest, speci, dist, p_value, p_value_with_color)



