###############################################################################
#### Create table with ranks based on loss function                        ####
###############################################################################

# 1. Create list with vectors of model names and ranks for each index

# Create empty list for results
Loss.results.rankings <- list()

for(index in indices){
  
  # Store sorted mean loss results
  Loss.results.rankings[[index]] <- Loss.results[[index]][['means']]
  
  # Replace loss result with rank
  for(i in 1:length(Loss.results.rankings[[index]])){
    
    name <- names(Loss.results.rankings[[index]][i])
    
    Loss.results.rankings[[index]][i] <- ifelse(is.na(Loss.results.rankings[[index]][i]) | Loss.results.rankings[[index]][i] == 9999, 'NP', i)
    
    names(Loss.results.rankings[[index]][i]) <- name
  }
}

# 2. Display results in matrix form 

# Column and row names
garch_models_in_order = c('ARCH', 'GARCH', 'EGARCH', 'GJR-GARCH', 'TGARCH', 'AVGARCH', 'APARCH', 'NGARCH', 'NAGARCH', 'FGARCH', 'CGARCH')
dist_names_in_order = c('NORM', 'STD', 'GED', 'SNORM', 'SSTD', 'SGED', 'GHYP', 'JSU', 'EMPIRICAL')

Loss.results.ranking.data.frames <- list()

for(index in indices){
  
  # Create empty data frame for each index
  Loss.results.ranking.data.frames[[index]] <- data.frame()
  
  for(speci in names(var.spec.list)){
    for(dist in names(dist.spec.list)){
      
      # Insert rank for each model-distribution combination (NP = not passed)
      Loss.results.ranking.data.frames[[index]][speci, dist] <- Loss.results.rankings[[index]][paste0(speci, '_', dist)]
    }
  }
  
  # Assign names of models and distributions
  rownames(Loss.results.ranking.data.frames[[index]]) <- garch_models_in_order
  colnames(Loss.results.ranking.data.frames[[index]]) <- dist_names_in_order
}


###############################################################################
####          Create views for latex with colors                          ####
###############################################################################

Loss.results.ranking.data.frames.color <- list()

for(index in indices){
  
  # Create empty data frame for each index
  Loss.results.ranking.data.frames.color[[index]] <- data.frame()
  
  # Store loss sequence of best model for each index
  best_loss_seq <- Loss.results[[index]][['sequence']][[best.models[[index]][1]]]
  
  for(speci in names(var.spec.list)){
    for(dist in names(dist.spec.list)){
      
      # Current model name
      speci_dist <- paste0(speci, '_', dist)
      
      # Extract ranking
      rank_speci_dist <- as.character(Loss.results.rankings[[index]][speci_dist])
      
      if(rank_speci_dist == 'NP'){
        
        # Insert NP in gray for models which haven't passed CC backtests
        Loss.results.ranking.data.frames.color[[index]][speci, dist] <- paste0('\\textcolor{red}{np}')
        
      } else {
        
        if(speci_dist == best.models[[index]][1]){
          
          # Assign teal 1* if current model is best model of current index
          Loss.results.ranking.data.frames.color[[index]][speci, dist] <- paste0('\\textcolor{teal}{1*}') #paste0('\\textcolor{teal}{\\textbf{1}}')
          
        } else {
          
          # Execute Diebold-Mariano test for significant difference between
          test_result <- DM_test(basis_loss_sequence = best_loss_seq,
                                 compare_loss_sequence = Loss.results[[index]][['sequence']][[speci_dist]])
          
          if(test_result[['p']] > 0.1){
            
            if(speci_dist %in% best.models[[index]][1:10]){
             
              # Rank in teal with *
              Loss.results.ranking.data.frames.color[[index]][speci, dist] <- paste0('\\textcolor{teal}{', rank_speci_dist ,'*}')#paste0('\\textcolor{teal}{\\textbf{', rank_speci_dist ,'}}')
              
            } else {
              
              # Rank in teal
              Loss.results.ranking.data.frames.color[[index]][speci, dist] <- paste0('\\textcolor{teal}{', rank_speci_dist ,'}')
            }
            
          } else {
            
            # Rank in orange
            Loss.results.ranking.data.frames.color[[index]][speci, dist] <- paste0('\\textcolor{orange}{', rank_speci_dist ,'}')
          }
        }
      }
    }
  }
  
  # Assign names of models and distributions
  rownames(Loss.results.ranking.data.frames.color[[index]]) <- garch_models_in_order
  colnames(Loss.results.ranking.data.frames.color[[index]]) <- dist_names_in_order
}

rm(index, i, name, garch_models_in_order, dist_names_in_order, speci, dist, best_loss_seq, speci_dist, rank_speci_dist, test_result)






