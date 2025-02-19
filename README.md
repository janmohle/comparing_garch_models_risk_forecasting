This is a repository for my bachelor thesis: Comparing Different GARCH Models with Respect to Financial Risk Forecasting

Instructions how to run the code:

1. Clone repository to local machine
2. Download other_quantities.RData and optionally VaR_ES_plot.RData from shared OneDrive folder: https://1drv.ms/f/s!AoS3k2oMmWvihL5irAKA8GgvZYAyJw?e=gkjJRf and store it in output folder
3. Open scripts/main.R: From here, the program can be parameterized and run
4. Follow instructions in main.R regarding parameter settings before running the code
5. Before running, make sure renv is installed. This can be done with the following code: install.packages('renv'). Use renv::restore() and follow dialog to set up libraries.

Additional notes:

I would not recomment to run the program without using subsetting options as explained in main.R.
I would recomment to either only load the results by setting the parameters accordingly or if one wants to test the program, to utilize subsetting parameters.

1. To only load results: Set execution_of_VaR_ES_forecasting = FALSE and optionally also execute_Backtest = FALSE. It is important to have other_quantities.RData downloaded and stored in output folder if execute_Backtest = TRUE and all parameters as set in published version and as explained in comments
2. To run the forecasting, especially utilize the follwing parameters and follow explanations in main.R: number_forecasts, index_include, varspec_include, dist_include
3. To run model on simulated data, first create simulated_data and simulated_output folders and adjust the follwing parameters: simulation, number_simulations

The published code has parameters set to load results. These results are the ones from my thesis. Additionally, I copied the final results into output_final.
