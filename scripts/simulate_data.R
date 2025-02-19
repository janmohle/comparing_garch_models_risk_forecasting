################################################################################################
###              Simulation of GARCH(q,p) models                                             ###
################################################################################################

# simulated_data and simulated_output have to be specified!

for(i in 1:number_simulations){

  # GARCH(q,p) specification
  # Root of price simulation is 100
  garchspec <- ugarchspec(variance.model = list(model = 'sGARCH',
                                                garchOrder = c(1,1)),
                          mean.model = list(armaOrder = c(0,0)),
                          distribution.model = 'norm',#sstd
                          fixed.pars = list(mu = 0.00025,
                                            omega = 0.00005,
                                            alpha1 = 0.1,
                                            beta1 = 0.85)#,
                                            #skew = -1,
                                            #shape = 5)
  )
  
  # Simulation
  garchsimlation <- ugarchpath(spec = garchspec,
                               n.sim = window_width+number_forecasts+2,
                               n.start = window_width+1,
                               m.sim = 1)
  
  # Storing values in data frame and adding dates
  garchsimlation_df <- as.data.frame(garchsimlation@path$seriesSim)
  
  
  garchsimlation_df <- garchsimlation_df %>%
    mutate(Date = as.Date('2000-01-01') + 1:nrow(garchsimlation_df)) %>%
    rename(Return = V1) %>%
    mutate(Price = 100 * exp(cumsum(Return)))
  
  # Creating first row of data frame with NA in Return and 100 as starting price
  garchsimlation_df_first_row <- data.frame(Date = as.Date('2000-01-01'),
                                            Return = NA,
                                            Price = 100)
  
  # Binding both data frames
  garchsimlation_df <- bind_rows(garchsimlation_df_first_row, garchsimlation_df)
  
  
  # Storing data in simulation folder
  file_name <- paste0('simulated_data/sim', i, '.csv')
  write.csv(x = garchsimlation_df,
            file = file_name,
            row.names = FALSE)
  
  # Delete simulted data set from R session
  rm(garchspec, garchsimlation, garchsimlation_df, file_name, garchsimlation_df_first_row)
}

rm(i)

