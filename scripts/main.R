#################################################################################
####           Important information!!!                                      ####
#################################################################################

# Program has to be parameterized in 'General parameter setting', 'Sub setting of input data and specifications' and 'Steering of program routines'.
# Important: Please read trough explanations of these parameters and set them accordingly before running the program!
# Current setting loads forecasting and backtesting results without executing the forecasting and backtesting routine.
# I added parameter values as comments to test the forecasting and backtesting routine on a subset of the data. To use them, uncomment these values, and comment currently uncommented values. Additionally, TRUE and FALSE parameters have to be adjusted according to explanations.
# If the forecasting results should only be loaded, but the backtesting routine should be executed, program automatically downloads other_quantities.RData from dropbox, as long as download_other_quantities = TRUE. It is a 600 MB list consisting of covariance matrices and gradient vectors needed for robust ES backtests and stored during forecasting routine.
# If forecasting and / or backtesting routine are executed, currently stored results will be overwritten. I recommend to rename the output folder and add new folder named 'output'.

#################################################################################
####           General set-up                                                ####
#################################################################################

# Open connection to write console messages into a txt file (uncomment if needed)
#sink('console_messages.txt', split = TRUE)

# Initial cleaning
rm(list = ls())
if (dev.cur() != 1) {
  dev.off()
}
cat('\14')

# Restore packages of renv
# If library not synchronized to lockfile, a dialog pops up and asks if all listed packages should be installed. Enter 'Y' in console. Packages are then installed with correct version.
renv::restore()

# Print start time
Sys.time()

# Loading required libraries
library(tidyverse)  # comprehensive package for data manipulation
library(zoo)        # handling of time series data
library(rugarch)    # comprehensive package for GARCH modeling and distributions
library(FinTS)      # used for Arch test
library(numDeriv)   # calculation of numerical derivatives
library(moments)    # moment calculation
library(tseries)    # used for JB test

# Definition of functions
source('scripts/functions.R')

#################################################################################
####           General parameter setting                                     ####
#################################################################################

# Parameters of this section set general structure of models. Current setting was used for paper. I recommend to leave them as they are!

# Number of autoregressive terms for mean model
ar = 0

# Number of moving average terms for mean model
ma = 0

# Number of ARCH terms for variance model
arch = 1

# Number of GARCH terms for variance model
garch = 1

# Width of estimation window for rolling forecasting
window_width = 750

# Tolerance level for VaR and ES
tolerance_lvl = 0.05

#################################################################################
###     Sub setting of input data and specifications                          ###
#################################################################################

# Parameters of this section are used to subset data and model specification. They are especially interesting for program-testing purposes.
# I added parameters as a comment that can be used to test the forecasting and backtesting routines on a subset of input data and specifications. For that, uncomment them and comment out current parameters.

# Parameter sets the number of forecasts. Can be commented if whole input data should be used, and if forecast results should only be loaded.
# To test the program on a subset of input data, uncomment it.
#number_forecasts = 50

# Parameter subsets the input data directly or indirectly based on number_forecasts and window_width. 
# If input data should be subset to test program, uncomment it.
# I recommend to leave it dependent on number_forecasts and window_width.
#data_include = 1:(window_width+1+number_forecasts)

# Parameter sets the indices that should be included from variable 'indices'.
# It can be commented if all indices should be used.
#index_include = c(1,2)

# Parameter sets the GARCH specification that should be included from list 'var.spec.list'.
# It can be commented if all specifications should be included. Paper includes c(1:11).
varspec_include = c(1:11) #c(2,3,4,10)

# Parameter sets the distribution assumptions that should be included from 'dist.spec.list'.
# It can be commented if all distributions should be used. Paper includes c(1:7,10,11).
dist_include = c(1:7,10,11) #c(1,2,7,11)


#################################################################################
###     Steering of program routines                                          ###
#################################################################################

# Parameters steer the execution routines of the program.
# Current parameter settings load the forecasting and backtesting results from my paper without executing the forecasting and backtesting routine.
# Important: If forecasting and / or backtesting routine are executed, currently stored results will be overwritten, so I recommend to rename the output folder and add a new folder named 'output' in the head folder of this project.

# Execution of VaR and ES forecasting routine.
# TRUE: stepwise VaR and ES forecasting gets executed
# FALSE: result are loaded from csv files in 'output' folder. index_include specifies which index data is loaded
# On full input data set combined with all specifications, program takes multiple days to run. If forecasting routine should be executed to test it, I highly recommend to subset input data and specifications in section 'Sub setting of input data and specifications'.
execution_of_VaR_ES_forecasting = FALSE

# Execution of VaR and ES backtesting routine.
# TRUE: Backtesting routine gets executed. If execution_of_VaR_ES_forecasting = FALSE at the same time, and other_quantities.RData doesn't exist output folder, it gets downloaded from dropbox to execute robust ES backtest.
# FALSE: List called 'Backtest_results.RData' gets loaded from output folder.
execute_Backtest = FALSE

# If plot_all_calc_models = TRUE, all forecasting results get plot and stored in list 'VaR.ES.plot'. From there, they can be accessed.
plot_all_calc_models = FALSE

# If execute_view_creation_backtest = TRUE, scripts gets executed that create views of backtest results in a data frame and for latex. I used some of these views in my paper.
# Only set to TRUE if varspec_include = c(1:11); dist_include = c(1:7,10,11); all 4 indices are included; simulation = FALSE. Otherwise errors would appear.
execute_view_creation_backtest = TRUE

# If TRUE, script gets executed that calculate rankings and views of these rankings in a data frame and for latex. I used some of these views in my paper.
# Only set to TRUE if varspec_include = c(1:11); dist_include = c(1:7,10,11); all 4 indices are included; simulation = FALSE. Otherwise errors would appear.
# Additionally, other_quantities.RData has to exist if execution_of_VaR_ES_forecasting = FALSE and execute_Backtest = TRUE.
execute_loss_function_and_ranking = TRUE

# Parameter decides whether real index data or simulated data should be used
# TRUE: Input data gets simulated. For that, folder 'simulated_input' and 'simulated_output' have to exist in head folder of this project. Furthermore, number_simulations, number_forecasts and data_include have to exist and execution_of_VaR_ES_forecasting and execute_Backtest should be TRUE.
# Script used for simulation can be found in scripts/simulate_data.R.
# FALSE: Real index data is used.
simulation = FALSE

# Parameter sets number of simulations. It has to be specified if simulation = TRUE.
#number_simulations = 5

# Manually prevent downloading of other_quantities.RData from dropbox if download_other_quantities=FALSE.
# other_quantities.RData is needed to execute robust ES backtests in cases where forecasting results are only read in and not executed (execution_of_VaR_ES_forecasting = FALSE).
# other_quantities.RData consists of covariance matrices, gradients of mu & sigma estimates and coefficients for each model and window shift.
# The large size of around 600 MB is due to the high number of matrices and gradient vectors stored in it.
# other_quantities.RData only gets downloaded if other_quantities.RData does not exist in output folder, execute_Backtest=TRUE, execution_of_VaR_ES_forecasting=FALSE, simulation = FALSE and download_other_quantities = TRUE.
# If all of the above mentioned criterion are true, but other_quantities.RData should still not be downloaded, then set download_other_quantities=FALSE.
# -> as a result, robust ES backtest do not get executed.
download_other_quantities = TRUE

# Parameter sets number of window shifts after which complex_ugarchfit should be executed. Explanation can be found in function.R at position of complex_ugarchfit.
# In cases where new_coef_est_counter = 1, this parameter has no effect!
# I recommend to leave it as it is, as it is more of a remaining from attempts to speed up the whole program!!
n_compl_opti = 100

# Parameter sets number of times after which fitting is executed without parameters of previous run as starting parameters. I recommend to set it to 1, as it leads to faster executions but also introduces biases in some models.
# I recommend to leave it as it is, as it is more of a remaining from attempts to speed up the whole program!!
new_coef_est_counter = 1

#################################################################################
####           General model specification set-up                            ####
#################################################################################

if(simulation){
  
  # Vector of all simulated data
  indices <- vector()
  for(i in 1:number_simulations){
    indices <- c(indices, paste0('sim', i))
    }
  
  } else {
    
    # Vector of all index names
    indices <- c('DAX',
                 'WIG',
                 'BTC',
                 'GLD')
    }

# List of possible variance specifications from rugarch
var.spec.list <- list(spec1 = list(model = 'sGARCH',              # ARCH
                                   garchOrder = c(10, 0)),
                      spec2 = list(model = 'sGARCH',              # GARCH
                                   garchOrder = c(arch, garch)),
                      spec3 = list(model = 'eGARCH',              # EGARCH
                                   garchOrder = c(arch, garch)),
                      spec4 = list(model = 'gjrGARCH',            # GJR-GARCH
                                   garchOrder = c(arch, garch)),
                      spec5 = list(model = 'fGARCH',              # TGARCH
                                   garchOrder = c(arch, garch),
                                   submodel = 'TGARCH'),
                      spec6 = list(model = 'fGARCH',              # AVGARCH
                                   garchOrder = c(arch, garch),
                                   submodel = 'AVGARCH'),
                      spec7 = list(model = 'apARCH',              # APARCH
                                   garchOrder = c(arch, garch)),
                      spec8 = list(model = 'fGARCH',              # NGARCH
                                   garchOrder = c(arch, garch),
                                   submodel = 'NGARCH'),
                      spec9 = list(model = 'fGARCH',              # NAGARCH
                                   garchOrder = c(arch, garch),
                                   submodel = 'NAGARCH'),
                      spec10 = list(model = 'fGARCH',             # FGARCH
                                    garchOrder = c(arch, garch),
                                    submodel = 'ALLGARCH'),
                      spec11 = list(model = 'csGARCH',            # CGARCH
                                    garchOrder = c(arch, garch)),
                      spec12 = list(model = 'iGARCH',             # IGARCH (not used in paper)
                                    garchOrder = c(arch, garch)),
                      spec13 = list(model = 'realGARCH',          # realized GARCH (not used in paper)
                                    garchOrder = c(arch, garch)),
                      spec14 = list(model = 'fiGARCH',            # FIGARCH (not used in paper)
                                    garchOrder = c(arch, garch)))


# Mean specification
mean.spec <- list(armaOrder = c(ar,ma),
                  include.mean = TRUE)

# List of all possible distribution assumptions in rugarch package
dist.spec.list <-  list(norm = 'norm',      #1
                        std = 'std',        #2
                        ged = 'ged',        #3
                        snorm = 'snorm',    #4
                        sstd = 'sstd',      #5
                        sged = 'sged',      #6
                        ghyp = 'ghyp',      #7   # nests sstd and nig
                        nig = 'nig',        #8   (not used in paper)
                        ghst = 'ghst',      #9   (not used in paper)
                        jsu = 'jsu',        #10
                        empirical = 'norm') #11  # Normal distribution for QML estimation -> asymptotically consistent


#################################################################################
####  Data preparation and sub setting of data and specifications           ####
#################################################################################

source('scripts/preparing_data.R')

source('scripts/subset.R')


#################################################################################
####           Descriptive part                                              ####
#################################################################################

price.return.plots <- list()
main.statistics <- list()

for(index in indices){
  
  # Price and return plots
  price.return.plots[[index]] <- price_return_plots_func(index = index)
  
  # Main descriptive statistics
  main.statistics[[index]] <- ts_main_statistics(index = index,
                                                 lags_Ljung_Box_test = 10,
                                                 lags_ArchTest = 10,
                                                 nu = 5)
}
rm(index)

#Storing both lists in output folder
saveRDS(price.return.plots, file=paste0(ifelse(simulation, 'simulated_output/', 'output/'), 'price_return_plots.RData'))
saveRDS(main.statistics, file=paste0(ifelse(simulation, 'simulated_output/', 'output/'), 'main_statistics.RData'))

# DAX
  # -> significant autocorrelation (10 lags) (but looks very uninformative and weak)
  # -> significant ARCH effect (10 lags)
  # -> leverage effect
  
# WIG
  # -> no significant autocorrelation (10 lags)
  # -> significant ARCH effect (10 lags)
  # -> leverage effect
  
# BTC
  # -> significant autocorrelation (10 lags) (potentially informative: first day - negative reaction; second day - positive reaction -- but very small coefficients)
  # -> significant ARCH effect (10 lags)
  # -> no clear leverage effect (only very slight)
  
# GLD
  # -> no significant autocorrelation (10 lags)
  # -> significant ARCH effect (10 lags)
  # -> no leverage effect

# GENERAL
# - Serial autocorrelation seems to be non existent or very weak -> mostly uninformative, although direction in BTC might be interesting
# - Volatility clustering always existent
# - leverage effect seems to be stronger in stock indices than in BTC and especially stronger than in GLD
# - Standardized returns are not normally distributed (excess kurtosis and negative skewness)
# - slightly positive mean and median of returns (DAX, WIG, GLD: 1e-4, BTC: 1e-3)

#################################################################################
####  Forecasting or loading forecasted data                                 ####
#################################################################################

source('scripts/stepwise_VaR_ES_forecasting.R')

#################################################################################
####           Backtesting of forecasts                                      ####
#################################################################################

source('scripts/backtests.R')

#################################################################################
####           Visual inspection of forecasts                                ####
#################################################################################

# Plot all calculated models if plot_all_calc_models = TRUE
if(plot_all_calc_models){
  
  VaR.ES.plot <- list()
  
  for(index in indices){
    
    index_data <- get(index)
    
    for(speci in names(var.spec.list)){
      for(dist in names(dist.spec.list)){
        
        data <- data.frame(Date = index_data[['Date']],
                           Return = index_data[['Return']],
                           Exceeded_VaR = index_data[[paste0('Exceeded_VaR_', speci, '_', dist)]],
                           VaR = index_data[[paste0('VaR_', speci, '_', dist)]],
                           ES = index_data[[paste0('ES_', speci, '_', dist)]])
        
        VaR.ES.plot[[index]][[paste0(speci, '_', dist)]] <- ggplot(data =  data[-1:-(window_width + 2),],
                                                                   mapping = aes(x = Date,
                                                                                 y = Return)) +
          geom_point(aes(colour = as.factor(Exceeded_VaR)), size = 0.8) +
          geom_line(aes(y = VaR, linetype = "VaR"), col = 'orange') +
          geom_line(aes(y = ES, linetype = "ES"), col = 'purple') +
          scale_color_manual(values = c("1" = "red", "0" = "black"),
                             name = "Exceeded VaR",
                             
                             labels = c("No", "Yes")) +
          scale_linetype_manual(name = "Risk Measures",
                                values = c("VaR" = "solid", "ES" = "solid"),
                                breaks = c('VaR', 'ES'),
                                guide = guide_legend(override.aes = list(color = c("orange", "purple")))) +
          labs(title = paste0('VaR and ES vs historical returns: ', speci, '_', dist)) +
          theme(plot.title = element_text(hjust = 0.5),
                panel.background = element_rect(fill = 'white'),
                panel.grid = element_blank(),
                axis.line = element_line(color = 'black'))
      }
    }
  }
  saveRDS(VaR.ES.plot, file=paste0(ifelse(simulation, 'simulated_output/', 'output/'), 'VaR_ES_plot.RData'), compress = 'xz')
  rm(index, index_data, speci, dist, data)
}

# Other plots
plot_1 = FALSE
if(plot_1){
  for(index in indices){
    data <- get(index)
    plot <- ggplot(data[-1:-(window_width + 2),], aes(x = as.Date(Date),
                                                y = Return)) +
      geom_point() +
      geom_line(aes(y = VaR_spec2_norm),
                col = 'red') +
      geom_line(aes(y = VaR_spec2_ged),
                col = 'green') +
      geom_line(aes(y = VaR_spec2_sstd),
                col = 'purple') +
      ggtitle(index) +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab('Date')
    print(plot)
  }
}

plot_2 = FALSE
if(plot_2){
ggplot(data =  DAX[-1:-(window_width + 2),],
       mapping = aes(x = Date,
                     y = Return)) +
  geom_point(aes(colour = as.factor(Exceeded_VaR_spec8_norm))) +
  geom_line(aes(y = VaR_spec8_norm),
            col = 'orange') +
  geom_line(aes(y = ES_spec8_norm),
            col = 'purple') +
  scale_color_manual(values = c("1" = "red", "0" = "black"),
                     name = "Exceeded VaR",
                     labels = c("No", "Yes"))
}

# Print finish time
Sys.time()

# Close connection to txt file (uncomment if needed)
#sink()


#################################################################################
####    Execution of backtest and loss function view creation for paper      ####
#################################################################################

if(execute_view_creation_backtest){source('scripts/view_creation_backtests.R')}

if(execute_loss_function_and_ranking){source('scripts/loss_function_and_ranking.R')}
