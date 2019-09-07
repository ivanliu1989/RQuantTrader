rm(list = ls()); gc()
# 0. Call libraries -------------------------------------------------------
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
registerDoMC(cores = parallel::detectCores())
source("tests/Utilities.R")

# 1. Initial setup --------------------------------------------------------
rm(list = ls(.blotter), envir = .blotter); ls(.blotter)
rm(list = ls(.strategy), envir = .strategy); ls(.strategy)

portfolio.st <- "Port.PairsStrat"
account.st <- "Acct.PairsStrat"
strategy.st <- "Strat.PairsStrat"
rm.strat(portfolio.st)
rm.strat(account.st)

Sys.setenv(TZ="UTC")
symb1 <- 'AUD_USD'
symb2 <- 'USD_CAD'
# AUD USD
AUD_USD = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = symb1, price = 'M', granularity = 'D', count = 1500)
AUD_USD$complete <- NULL
names(AUD_USD) <- c("time", "Volume", "Close", "High", "Low", "Open")
AUD_USD[, 2:6] <- sapply(AUD_USD[, 2:6], as.numeric)
AUD_USD = xts(AUD_USD[,-1],as.POSIXct(as.Date(AUD_USD$time)))
# USD CAD
USD_CAD = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = symb2, price = 'M', granularity = 'D', count = 1500)
USD_CAD$complete <- NULL
names(USD_CAD) <- c("time", "Volume", "Close", "High", "Low", "Open")
USD_CAD[, 2:6] <- sapply(USD_CAD[, 2:6], as.numeric)
USD_CAD = xts(USD_CAD[,-1],as.POSIXct(as.Date(USD_CAD$time)))


# hedgeRatio = lag(rollapply(merge(Cl(AUD_USD), Cl(1/USD_CAD)), width = 20,
#                        FUN = function(x){ return(HedgeRatioOLS(x[,1], x[,2])$beta)},
#                        by.column = FALSE))
spreads <- OHLC(AUD_USD)-OHLC(1/USD_CAD)
# spreads <- log(OHLC(AUD_USD)/OHLC(1/USD_CAD))
# y = OHLC(1/USD_CAD); y= merge(y, hedgeRatio)
# y$Open = y$Open * y$hedgeRatio
# y$High = y$High * y$hedgeRatio
# y$Low = y$Low * y$hedgeRatio
# y$Close = y$Close * y$hedgeRatio
# spreads <- OHLC(AUD_USD) - OHLC(y)

symbols <- c("spreads")
currency("USD")
instrument(symbols, currency = "USD", multiplier = 1, tick_size = 0.00001)
# stock(symbols,currency = "USD",multiplier = 1, tick_size = 0.00001)

chart_Series(spreads)
add_TA(EMA(Cl(spreads), n=20), on=1, col="blue", lwd=1.5)
legend(x=5, y=50, legend=c("EMA 20"), fill=c("blue"), bty="n")



# fetch market data and plot the spread
startDate = as.character(min(index(spreads)))
endDate = as.character(max(index(spreads)))
initDate = as.character(min(as.Date(index(spreads)))-1)
initEq = 1000000
N.ADF = 60
N = 20


#Inititalize strategy, portfolio, account and orders
initPortf(portfolio.st, symbols = symbols, initDate=initDate)
initAcct(account.st, portfolios=portfolio.st,initEq=initEq, initDate=initDate)
initOrders(portfolio.st, symbols = symbols, initDate=initDate)

# Save strategy
strategy(strategy.st, store = TRUE)
# rm.strat(pairStrat) # only when trying a new test
ls(.blotter) # .blotter holds the portfolio and account object
ls(.strategy) # .strategy holds the orderbook and strategy object



# 2. Statistics Testing ---------------------------------------------------
# a) Z-Score
Price.Ratio <- PairRatio(c(symb1[1],symb2[1]))
Price.Ratio.MA <- MaRatio(Price.Ratio)
Price.Ratio.SD <- Sd(Price.Ratio)

# b) Augmented Dickey Fuller
P.Value <- Pval(Price.Ratio)

# Z.Score <- ZScore(x=merge(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD))
Z.Score <- zscores.ma(t =  Price.Ratio, long = 60, short = 20)
# Z.Score <- zscores(t = na.omit(lag(diff(Price.Ratio))))
plot(main = "Z-Score Time Series", xlab = "Date" , ylab = "Z-Score",Z.Score, type = "l" )
abline(h = 2, col = 2, lwd = 3 ,lty = 2)
abline(h = -2, col = 3, lwd = 3 ,lty = 2)



# 3. Add Indicators -------------------------------------------------------
add.indicator(strategy = strategy.st, name = "ZScore", arguments =
                  list(x=merge(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD)))
add.indicator(strategy = strategy.st, name = "Pval", arguments =
                  list(x=quote(Price.Ratio)))


# set it to 0.1 for a 10% significance level.
# to turn ADF test off (2nd indicator), change it to 1
alpha = 0.3
# alpha = seq(0.05, 1, length.out = 20)
# Z-Score entry and exit thresholds:
buyThresh = -1
sellThresh = -buyThresh
exitlong = 0
exitshort = 0


# 4. Add Signals ----------------------------------------------------------
# Before running our backtest, we have to add the signals, position limits and rules of our strategy:
add.signal(strategy.st, name="sigThreshold",arguments=list(column="Z.Score", threshold=buyThresh,
                                                           relationship="lt", cross=FALSE),label="longEntryZ")
add.signal(strategy.st, name="sigThreshold",arguments=list(column="P.Value", threshold= alpha,
                                                           relationship="lt", cross=FALSE),label="PEntry")
add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("longEntryZ", "PEntry"), cross=FALSE),
           label="longEntry")
add.signal(strategy.st, name="sigThreshold",arguments=list(column="Z.Score", threshold= exitlong,
                                                           relationship="gt", cross=FALSE),label="longExit")
add.signal(strategy.st, name="sigThreshold",arguments=list(column="Z.Score", threshold=sellThresh,
                                                           relationship="gt", cross=FALSE),label="shortEntryZ")
add.signal(strategy.st, name="sigAND", arguments=list(columns=c("shortEntryZ", "PEntry"), cross=FALSE),
           label="shortEntry")
add.signal(strategy.st, name="sigThreshold",arguments=list(column="Z.Score", threshold= exitshort,
                                                           relationship="lt", cross=FALSE),label="shortExit")


# 5. Set Rules ------------------------------------------------------------
addPosLimit( portfolio = portfolio.st, # add position limit rules
             symbol = 'spreads',
             timestamp = initDate,
             maxpos = initEq * 0.02,
             longlevels = 1,
             minpos = -initEq * 0.02)

add.rule(strategy.st, name='ruleSignal',arguments = list(sigcol="longEntry",
                                                         sigval=TRUE, orderqty=initEq * 0.02,.osFUN = osMaxPos, replace = FALSE, ordertype='market',
                                                         orderside='long', prefer = "open"), type='enter' )
add.rule(strategy.st, name='ruleSignal', arguments = list(sigcol="shortEntry",
                                                          sigval=TRUE, orderqty=-initEq * 0.02,.osFUN = osMaxPos, replace = FALSE,ordertype='market',
                                                          orderside='short', prefer = "open"), type='enter')
add.rule(strategy.st, name='ruleSignal', arguments = list(sigcol="longExit",
                                                          sigval=TRUE, orderqty= 'all', ordertype='market', orderside='short', prefer = "open"), type='exit')
add.rule(strategy.st, name='ruleSignal', arguments = list(sigcol="shortExit",
                                                          sigval=TRUE, orderqty= 'all' , ordertype='market', orderside='long', prefer = "open"), type='exit')

# 6. Check Strategy and Run Strategy --------------------------------------
summary(get.strategy(strategy.st))
results <- applyStrategy(strategy.st, portfolios = portfolio.st, mktdata = spreads)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    # save(list = "results", file = results_file)
    save.strategy(strategy.st)
}


tns <-getTxns(Portfolio=portfolio.st, Symbol= symbols)

# 7. Out of Sample test ---------------------------------------------------
chart.P2(portfolio.st, "spreads", prefer = "close")
returns <- PortfReturns(account.st)
charts.PerformanceSummary(returns, geometric=FALSE, wealth.index=TRUE, main = "Pair Strategy Returns")

# save.strategy(portfolio.st)
