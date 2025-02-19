####################################################################################
####          Export tables and plots to LaTeX format                           ####
####################################################################################

library("xtable")     # package "Export Tables to LaTeX or HTML"
library("tikzDevice") # package "R Graphics Output in LaTeX Format"

# Define output table path
path_tab = '../text/tables/'

# Define output plots path
path_plot = '../text/plots/'

####################################################
#### Export summary statistics table to LaTeX   ####
####################################################

# Create table
summary.statistics.table <- data.frame(row.names = indices)
for(index in indices){
  summary.statistics.table[index, 'Mean'] <- main.statistics[[index]][['mean']]
  summary.statistics.table[index, 'Median'] <- main.statistics[[index]][['quantiles']]['50%']
  # summary.statistics.table[index, '25\\%'] <- main.statistics[[index]][['quantiles']]['25%']
  # summary.statistics.table[index, '75\\%'] <- main.statistics[[index]][['quantiles']]['75%']
  summary.statistics.table[index, 'S.D.'] <- main.statistics[[index]][['sd']]
  summary.statistics.table[index, 'Skewness'] <- main.statistics[[index]][['skewness']]
  summary.statistics.table[index, 'Kurtosis'] <- main.statistics[[index]][['kurtosis']]
  summary.statistics.table[index, 'JB Test p'] <- main.statistics[[index]][['JB_test']][['p.value']]
  summary.statistics.table[index, 'LB Test p'] <- main.statistics[[index]][['Ljung_Box_test']][['p.value']]
  summary.statistics.table[index, 'Arch Test p'] <- main.statistics[[index]][['ArchTest']][['p.value']]
}
summary.statistics.table <- round(summary.statistics.table, 4)

# Set options
options(xtable.floating = FALSE) # no "\begin{table}[ht] \centering..." 
options(xtable.timestamp = "")   # no timestamp
options(xtable.comment = TRUE)  # no xtable comment
options(xtable.sanitize.text.function = identity)  # no sanitization within table

# Export the raw table of basic statistics
summary.statistics.table.export <- xtable(summary.statistics.table, digits = 4)
print(summary.statistics.table.export, file=paste0(path_tab, 'Summary_stats.tex'), hline.after = c(-1, -1, 0, nrow(summary.statistics.table.export), nrow(summary.statistics.table.export)))


####################################################
#### Export price and return plots to LaTeX     ####
####################################################

# Set plot options
textwidth = 15.5/2.54  # LaTeX textwidth from "cm" into "inch"
options(tikzMetricsDictionary = "tikzMetricsDictionary") # create MetricsDictionary in file

# Export price and return plots
for(index in indices){
  for(i in c('Price', 'Return')){
    tikz(file=paste0(path_plot,index,'_',i,'.tex'), width=textwidth, height=0.5*textwidth)
    plot(price.return.plots[[index]][[i]])
    dev.off()
  }
}

# Export all plots from main.statistics
for(index in indices){
  for(plot_name in names(main.statistics[[index]])){
    if(is.ggplot(main.statistics[[index]][[plot_name]])){
      tikz(file=paste0(path_plot,index,'_',plot_name,'.tex'), width=textwidth, height=0.33*textwidth)
      plot(main.statistics[[index]][[plot_name]])
      dev.off()
    }
  }
}


########################################################
#### Export Backtest results to latex               ####
########################################################

# DAX Christofferson 2
DAX_Christofferson_2 <- xtable(Backtest.data.frames.color$DAX$VaR.Christofferson.2, digits = 3)
print(DAX_Christofferson_2, file=paste0(path_tab, 'DAX_Christofferson_2.tex'), hline.after = c(-1, -1, 0, nrow(Backtest.data.frames.color$DAX$VaR.Christofferson.2), nrow(Backtest.data.frames.color$DAX$VaR.Christofferson.2)))

# WIG Christofferson 2
WIG_Christofferson_2 <- xtable(Backtest.data.frames.color$WIG$VaR.Christofferson.2, digits = 3)
print(WIG_Christofferson_2, file=paste0(path_tab, 'WIG_Christofferson_2.tex'), hline.after = c(-1, -1, 0, nrow(Backtest.data.frames.color$WIG$VaR.Christofferson.2), nrow(Backtest.data.frames.color$WIG$VaR.Christofferson.2)))

# BTC Christofferson 2
BTC_Christofferson_2 <- xtable(Backtest.data.frames.color$BTC$VaR.Christofferson.2, digits = 3)
print(BTC_Christofferson_2, file=paste0(path_tab, 'BTC_Christofferson_2.tex'), hline.after = c(-1, -1, 0, nrow(Backtest.data.frames.color$BTC$VaR.Christofferson.2), nrow(Backtest.data.frames.color$BTC$VaR.Christofferson.2)))

# GLD Christofferson 2
GLD_Christofferson_2 <- xtable(Backtest.data.frames.color$GLD$VaR.Christofferson.2, digits = 3)
print(GLD_Christofferson_2, file=paste0(path_tab, 'GLD_Christofferson_2.tex'), hline.after = c(-1, -1, 0, nrow(Backtest.data.frames.color$GLD$VaR.Christofferson.2), nrow(Backtest.data.frames.color$GLD$VaR.Christofferson.2)))


# DAX ES CC
DAX_ES_CC <- xtable(Backtest.data.frames.color$DAX$ES.CC.robust, digits = 3)
print(DAX_ES_CC, file=paste0(path_tab, 'DAX_ES_CC.tex'), hline.after = c(-1, -1, 0, nrow(Backtest.data.frames.color$DAX$ES.CC), nrow(Backtest.data.frames.color$DAX$ES.CC)))

# WIG ES CC
WIG_ES_CC <- xtable(Backtest.data.frames.color$WIG$ES.CC.robust, digits = 3)
print(WIG_ES_CC, file=paste0(path_tab, 'WIG_ES_CC.tex'), hline.after = c(-1, -1, 0, nrow(Backtest.data.frames.color$WIG$ES.CC), nrow(Backtest.data.frames.color$WIG$ES.CC)))

# BTC ES CC
BTC_ES_CC <- xtable(Backtest.data.frames.color$BTC$ES.CC.robust, digits = 3)
print(BTC_ES_CC, file=paste0(path_tab, 'BTC_ES_CC.tex'), hline.after = c(-1, -1, 0, nrow(Backtest.data.frames.color$BTC$ES.CC), nrow(Backtest.data.frames.color$BTC$ES.CC)))

# GLD ES CC
GLD_ES_CC <- xtable(Backtest.data.frames.color$GLD$ES.CC.robust, digits = 3)
print(GLD_ES_CC, file=paste0(path_tab, 'GLD_ES_CC.tex'), hline.after = c(-1, -1, 0, nrow(Backtest.data.frames.color$GLD$ES.CC), nrow(Backtest.data.frames.color$GLD$ES.CC)))


###################################################################
#### Export loss matrices and list of best models to latex    ####
###################################################################

for(index in indices){
  result_table <- xtable(Loss.results.ranking.data.frames.color[[index]], digits = 1)
  print(result_table, file=paste0(path_tab, index, '_loss_ranking_matrix.tex'), hline.after = c(-1, -1, 0, nrow(Loss.results.ranking.data.frames.color[[index]]), nrow(Loss.results.ranking.data.frames.color[[index]])))
}

print(xtable(best.models.with.names, digits = 3), file=paste0(path_tab, 'best_models.tex'), hline.after = c(-1, -1, 0, nrow(best.models.with.names), nrow(best.models.with.names)))

# Remove all temporary objects
rm(path_tab, path_plot,
   summary.statistics.table.export,
   DAX_Christofferson_2, WIG_Christofferson_2, BTC_Christofferson_2, GLD_Christofferson_2,
   DAX_ES_CC, WIG_ES_CC, BTC_ES_CC, GLD_ES_CC,
   result_table)

############################################################################################
#### Export VaR and ES vs log-returns series plots of models with rank 1 for each index  ###
############################################################################################

# These plots only exit if plot_all_calc_models = TRUE
tikz(file=paste0(path_plot, 'DAX_rank1_forecasts.tex'), width=textwidth, height=0.5*textwidth)
plot(VaR.ES.plot$DAX$spec6_sged + ggtitle('DAX: AVGARCH with skewed GED'))
dev.off()
tikz(file=paste0(path_plot, 'WIG_rank1_forecasts.tex'), width=textwidth, height=0.5*textwidth)
plot(VaR.ES.plot$WIG$spec10_ghyp + ggtitle('WIG: FGARCH with Generalized Hyperbolic Distribution'))
dev.off()
tikz(file=paste0(path_plot, 'BTC_rank1_forecasts.tex'), width=textwidth, height=0.5*textwidth)
plot(VaR.ES.plot$BTC$spec3_ged + ggtitle('Bitcoin: EGARCH with GED'))
dev.off()
tikz(file=paste0(path_plot, 'GLD_rank1_forecasts.tex'), width=textwidth, height=0.5*textwidth)
plot(VaR.ES.plot$GLD$spec11_sged + ggtitle('Gold: CGARCH with skewed GED'))
dev.off()









