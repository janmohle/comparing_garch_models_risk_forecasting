####################################################################################
#### Subsetting of data and specifications for faster processing in development ####
####################################################################################

# Subsets data for faster step wise forecasting if data_include is specified either for simulated data or real data
if(simulation){
  if(exists('data_include')){
    for(i in 1:number_simulations){
      sim_i_name <- paste0('sim', i)
      sim_i <- get(sim_i_name)
      sim_i <- sim_i[data_include,]
      assign(sim_i_name, sim_i)
      rm(sim_i_name, sim_i)
    }
    rm(i)
  }
} else {
  if(exists('data_include')){
    DAX <- DAX[data_include,]
    WIG <- WIG[data_include,]
    BTC <- BTC[data_include,]
    GOLD <- GOLD[data_include,]
  }
}

# Subsets indices for faster step wise forecasting if index_include specified either for simulated data or real data
if(exists('index_include')){
  indices <- indices[index_include]
}

# Subsets variance specifications for faster step wise forecasting if varspec_include specified
if(exists('varspec_include')){
  var.spec.list <- var.spec.list[varspec_include]
}

# Subsets distribution assumptions for faster step wise forecasting if dist_include specified
if(exists('dist_include')){
  dist.spec.list <- dist.spec.list[dist_include]
}
