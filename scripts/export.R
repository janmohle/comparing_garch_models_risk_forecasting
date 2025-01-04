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



