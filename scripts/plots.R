#needed libraries
#library(tidyverse)

#plotting prices and returns
DAX.prices.plot <- ggplot(DAX,
                          aes(x = Date,
                              y = Price)) +
  geom_line() +
  labs(title = 'DAX') +
  theme(plot.title = element_text(hjust = 0.5))

DAX.returns.plot <- ggplot(DAX,
                           aes(x = Date,
                               y = Return)) +
  geom_line() +
  labs(title = 'DAX') +
  theme(plot.title = element_text(hjust = 0.5))

WIG.prices.plot <- ggplot(WIG,
                          aes(x = Date,
                              y = Price)) +
  geom_line() +
  labs(title = 'WIG') +
  theme(plot.title = element_text(hjust = 0.5))

WIG.returns.plot <- ggplot(WIG,
                           aes(x = Date,
                               y = Return)) +
  geom_line() +
  labs(title = 'WIG') +
  theme(plot.title = element_text(hjust = 0.5))

BTC.prices.plot <- ggplot(BTC,
                          aes(x = Date,
                              y = Price)) +
  geom_line() +
  labs(title = 'BTC') +
  theme(plot.title = element_text(hjust = 0.5))

BTC.returns.plot <- ggplot(BTC,
                           aes(x = Date,
                               y = Return)) +
  geom_line() +
  labs(title = 'BTC') +
  theme(plot.title = element_text(hjust = 0.5))

GOLD.prices.plot <- ggplot(GOLD,
                           aes(x = Date,
                               y = Price)) +
  geom_line() +
  labs(title = 'GOLD') +
  theme(plot.title = element_text(hjust = 0.5))

GOLD.returns.plot <- ggplot(GOLD,
                            aes(x = Date,
                                y = Return)) +
  geom_line() +
  labs(title = 'GOLD') +
  theme(plot.title = element_text(hjust = 0.5))
