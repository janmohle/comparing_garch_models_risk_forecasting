rm(list = ls())
if (dev.cur() != 1) {
  dev.off()
}
cat('\14')

library(tidyverse)
library(zoo)
library(rugarch)
library(FinTS)

source('/Users/janmohle/Library/CloudStorage/OneDrive-Personal/01Studium/01Göttingen/08SoSe_24/Bachelorarbeit/comparing_garch_risk_forecasting/scripts/preparing_data.R')
source('/Users/janmohle/Library/CloudStorage/OneDrive-Personal/01Studium/01Göttingen/08SoSe_24/Bachelorarbeit/comparing_garch_risk_forecasting/scripts/functions.R')
source('/Users/janmohle/Library/CloudStorage/OneDrive-Personal/01Studium/01Göttingen/08SoSe_24/Bachelorarbeit/comparing_garch_risk_forecasting/scripts/plots.R')
#!!!!! if project is set, change wd in source and R file behind source

DAX.prices.plot
DAX.returns.plot
WIG.prices.plot
WIG.returns.plot
BTC.prices.plot
BTC.returns.plot
GOLD.prices.plot
GOLD.returns.plot

#main statistics
DAX.statistics <- ts.main.statistics(DAX$Return)
# -> significant autocorrelation
# -> significant ARCH effect
# -> leverage effect

WIG.statistics <- ts.main.statistics(WIG$Return)
# -> no significant autocorrelation
# -> significant ARCH effect
# -> leverage effect

BTC.statistics <- ts.main.statistics(BTC$Return)
# -> no significant autocorrelation
# -> significant ARCH effect
# -> no clear leverage effect

GOLD.statistics <- ts.main.statistics(GOLD$Return)
# -> no significant autocorrelation (but p < 0.1)
# -> significant ARCH effect
# -> no clear leverage effect



#converting retruns into zoo object
returns <- zoo(WIG$Return, WIG$Date) %>% na.omit()


#testing rolling forecast garch normal
roll_VaR <- rollapply(returns,
                      width = 250,
                      FUN = function(x) VaR_forecast_1_ahead_garch(data = x),
                      align = 'right')

roll_VaR_shifted <- stats::lag(roll_VaR, k = -1)

WIG <- WIG %>%
  mutate(VaR_forecast_sgarch_normal = c(rep(NA, nrow(WIG) - length(roll_VaR_shifted)),
                                        coredata(roll_VaR_shifted))) %>%
  mutate(exceeded_garch_normal = ifelse(Return > VaR_forecast_sgarch_normal, 0, 1))


ggplot(WIG, aes(x = Date, y = Return)) +
  geom_point(aes(color = factor(exceeded_garch_normal))) + 
  scale_color_manual(values = c('0' = 'grey', '1' = 'red')) +
  geom_line(aes(y = VaR_forecast_sgarch_normal), col = 'purple') +
  labs(color = 'Exceeded') +
  theme_minimal() +
  labs(title = 'VaR Exceedence Plot') +
  theme(plot.title = element_text(hjust = 0.5))



#testing rolling forecast egarch t
roll_VaR <- rollapply(returns,
                      width = 250,
                      FUN = function(x) VaR_forecast_1_ahead_garch(data = x,
                                                                   distribution = 'std',
                                                                   variance.model = list(model = 'eGARCH', garchOrder = c(1,1)),
                                                                   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                                                                   distribution.model = 'std'),
                      align = 'right')

roll_VaR_shifted <- stats::lag(roll_VaR, k = -1)

WIG <- WIG %>%
  mutate(VaR_forecast_egarch_t = c(rep(NA, nrow(WIG) - length(roll_VaR_shifted)),
                                        coredata(roll_VaR_shifted))) %>%
  mutate(exceeded_egarch_t = ifelse(Return > VaR_forecast_sgarch_normal, 0, 1))


ggplot(WIG, aes(x = Date, y = Return)) +
  geom_point(aes(color = factor(exceeded_egarch_t))) + 
  scale_color_manual(values = c('0' = 'grey', '1' = 'red')) +
  geom_line(aes(y = VaR_forecast_egarch_t), col = 'purple') +
  labs(color = 'Exceeded') +
  theme_minimal() +
  labs(title = 'VaR Exceedence Plot') +
  theme(plot.title = element_text(hjust = 0.5))






