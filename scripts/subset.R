###########################################################################################
#### Sub setting of data and specifications for faster processing in development period ###
###########################################################################################

# Sub sets data for faster step wise forecasting if data_include is specified
if(exists('data_include')){
  for(index_name in indices){
    index_data <- get(index_name)
    index_data <- index_data[data_include,]
    assign(index_name, index_data)
    rm(index_data)
  }
  rm(index_name)
}

# Subsets variance specifications for faster step wise forecasting if varspec_include specified
if(exists('varspec_include')){
  var.spec.list <- var.spec.list[varspec_include]
}

# Subsets distribution assumptions for faster step wise forecasting if dist_include specified
if(exists('dist_include')){
  dist.spec.list <- dist.spec.list[dist_include]
}
