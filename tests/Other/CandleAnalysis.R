# Real Account: 7496
# Paper Account: 7497
library(IBrokers)
library(quantstrat)
library(tseries)
library(IKTrading)
library(PerformanceAnalytics)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(htmltools)
library(htmlwidgets)
library(knitr)
library(lattice)
library(pander)
library(tidyr)
library(webshot)
library(doMC)
library(RQuantAPI)

tws <- twsConnect(port = 7497, clientId = 998)

USDCAD <- getIBForexHist(tws, "10 Y", "1 day", "USD", "CAD")
head(USDCAD$BIDASK) # candle

data = USDCAD$BIDASK
head(data)


# mark bullish candles
data$bull = 1
data$bull = ifelse(data$USD.CAD.Close >= data$USD.CAD.Open, 1, 0)

# count consecutive bullish / bearish candles
data$count <- 1
for(i in 2:nrow(data)){
    if(as.numeric(data[i,]$bull) == as.numeric(data[i-1,]$bull))
        data[i,]$count = as.numeric(data[i-1,]$count + 1)
}


# example 1: prob. of trend change
data$nextcount = shift(data$count, 1)

mytable = table(data$count, data$nextcount)
mytable

percent.table <- prop.table(mytable, 1)
percent.table[percent.table==0] <- NA
percent.table


# when count if 7, buy/sell the opposite way
data$nextopen = shift(Op(data), 1)
data$nextclose = shift(Cl(data), 1)
trades <- subset(data, data$count >= 7)

trades$direction <- 1
trades$direction <- ifelse(trades$bull == 1, -1, trades$direction)

trades$cost = 0.000

trades$profit = trades$direction * (trades$nextclose - trades$nextopen) - trades$cost
trades$balance <- cumsum(trades$profit)

plot(trades$balance)



# Cyclicality
data$row = seq(1, nrow(data), 1)

data$smoothed <- ksmooth(data$row, data$USD.CAD.Close, "normal", bandwidth = 10)$y

plot(data$USD.CAD.Close)
lines(data$smoothed, col = "red")

# find the peaks and troughs
peaks = which(diff(diff(data$smoothed)>=0)<0) + 1
troughs = which(diff(diff(data$smoothed)>0)>0) + 1
data$minmax = 0
data$minmax = ifelse(data$row %in% peaks, 1, data$minmax)
data$minmax = ifelse(data$row %in% troughs, -1, data$minmax)

ggplot(data, aes(row)) +
    geom_point(aes(y = USD.CAD.Close, colour = "close"), size = 3) +
    geom_line(aes(y = smoothed, colour = 'smoothed'), size = 1.2) +
    geom_point(data = subset(data, data$minmax == 1), aes(y=smoothed), color = 'red', size = 4) +
    geom_point(data = subset(data, data$minmax == -1), aes(y=smoothed), color = 'green', size = 4) +
    theme_bw() +
    theme(legend.position = "none")

# plot cyclicality
tmp1 = subset(data, data$minmax != 0)
tmp1$pricediff = c(NA, abs(diff(tmp1$smoothed)))
