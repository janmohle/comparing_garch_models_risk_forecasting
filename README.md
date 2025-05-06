This is a repository for my bachelor thesis: "Comparing Different GARCH Models with Respect to Financial Risk Forecasting"

The written text of my thesis can be found in 00_text.pdf.

Instructions how to run the code:

1. Clone repository to local machine
2. Make sure R and RStudio are installed and click on comparing_garch_models_risk_forecasting.Rproj to open the project
3. Open scripts/main.R: From here, the program can be parameterized and run
4. Important: Follow instructions in scripts/main.R regarding parameter settings before running the code (start with reading section 'Important information!!!' on top of scripts/main.R)
5. To execute the program, mark whole scripts/main.R and press run

Additional notes:

I would not recommend to run the program without using subsetting options as explained in scripts/main.R, as this takes multiple days.
I would recommend to either only load the results by setting the parameters accordingly, or if the program routines should be tested, to utilize subsetting parameters.

In published code, parameters are set to soley load the results from my thesis without executing the forecasting and backtesting routines. If this is the goal, the code can be run without any parameter adjustments.
