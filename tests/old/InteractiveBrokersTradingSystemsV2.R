rm(list=ls());gc()
library(caret)
library(xgboost)
library(quantmod)
library(tseries)
library(TTR)
library(data.table)
library(xts)
library(lattice)
library(timeSeries)
library(PerformanceAnalytics)
library(zoo)
library(IBrokers)
source('tests/AutomatedSystems/IBPaperTradingIndicators.R')
source('tests/h2o/h2oDeeplearningMain.R')
source('tests/caret/caretMachineLearningEnsemble.R')
Sys.setenv(TZ='US/Eastern')


# Sunday approximately 5 p.m. to Friday 5 p.m
AUDUSD = prepareIBForexPrices(ib.duration = "15 Y", ib.barsize = "1 day", Cur1 = "AUD", Cur2 = "USD", ibAcc = 'paper')
USDCAD = prepareIBForexPrices(ib.duration = "15 Y", ib.barsize = "1 day", Cur1 = "USD", Cur2 = "CAD", ibAcc = 'paper')
USDRUB = prepareIBForexPrices(ib.duration = "15 Y", ib.barsize = "1 day", Cur1 = "USD", Cur2 = "RUB", ibAcc = 'paper')
USDHUF = prepareIBForexPrices(ib.duration = "15 Y", ib.barsize = "1 day", Cur1 = "USD", Cur2 = "HUF", ibAcc = 'paper')

# tws <- twsConnect(port = 7497, clientId = 998)
# ccy <- reqContractDetails(tws, twsCurrency('AUD', 'USD'))[[1]]$contract
# AUDUSD_latest = prepareIBForexPrices(ib.duration = "1 D", ib.barsize = "1 day", Cur1 = "AUD", Cur2 = "USD", ibAcc = 'paper')

# 1. Price model ----------------------------------------------------------
aud = Cl(AUDUSD$IB.MID); index(aud) = as.Date(index(aud))
cad = Cl(1/USDCAD$IB.MID); index(cad) = as.Date(index(cad))
rub = Cl(1/USDRUB$IB.MID); index(rub) = as.Date(index(rub))
huf = Cl(1/USDHUF$IB.MID); index(huf) = as.Date(index(huf))

cols = c('AUDUSD', 'USDCAD', 'USDRUB', 'USDHUF')
pairs = na.omit(merge(aud, cad, rub, huf)); names(pairs) = cols

dat.all = tail(pairs[, cols], 1200)
inLong = FALSE
inShort = FALSE
initAssets = 1000000
posiRate = 0.05
johansen.lookback = 63
hurst.lookback = 20
nfx = 4
threshold = 1.1
adf.threshold = 0.4
jc.threshold = 0.2
hurst.threshold = 0.6
stoploss = -0.1


goLong = FALSE
goShort = FALSE
closePos = FALSE

# settings
dat = dat.all[(r-johansen.lookback):r, cols]
dat.log = log(dat)

# 1. Statistics Test ------------------------------------------------------

# stats tests
jc.res=JohansenCointegrationTest(dat.log, type = "eigen", ecdet = 'const', K = 2)
ols.fit = lm(AUDUSD~. , dat.log)
ols.r2 = summary(ols.fit)$r.squared

# Spreads
hedgeRatio = c(1, -ols.fit$coefficients[-1])
spreads = dat %*% t(data.frame(hedgeRatio))[1:nfx]
spreads = as.xts(spreads, order.by = index(dat))
# chart_Series(spreads)
spreads = na.omit(spreads)

# stats tests 2
adf.res=adf.test(spreads) #0.06999
hurst.res=as.numeric(tail(HurstExponentTest(spreads, hurst.lookback)$hurstKY, 1)) # > 0.5 trending | < 0.5 mean reverting
half.life <- HalfLifeMeanReversion(spreads)$half.life.round

# Technical indicators
bbands = na.omit(BollingerBands(spreads, 20, 2))
rsi = RSI(spreads)
stochOsc = stoch(spreads)

# mean reversion signals
adf.signal=adf.res$p.value
hurst.signal=hurst.res
jc.signal=as.numeric(((jc.res$p.value[1]-jc.res$r.1)/((jc.res$p.value[3]-jc.res$p.value[1])/10)+10)/100)
bb.signal = ifelse(bbands$spread<=bbands$BBlow, -1, ifelse(bbands$spread>=bbands$BBhigh, 1, 0))
rsi.signal = ifelse(rsi<=30, -1, ifelse(rsi>=70, 1, 0))
stock.signal = stochOsc$fastK - stochOsc$slowD
stock.signal = diff(sign(stock.signal))
stock.signal = ifelse(stock.signal == -2, 'sell', ifelse(stock.signal == 2, 'buy', 'na'))
# momentum signals





    # positioning
    zScore=tail(zscores(spreads),1)
    numUnits=-tail(zScore,1)
    hedgeRatio = t(data.frame(hedgeRatio))
    sizing = abs(numUnits)/1
    posUnits = initAssets * posiRate
    sizingVal = as.numeric(posUnits * sizing)
    # hedgedMktVal = sizingVal / as.numeric(scale(as.numeric(hedgeRatio),center = F))
    positions = data.frame(round(sizingVal * hedgeRatio))
    prices = tail(dat,1)
    names(positions) = paste0('POS.', cols)
    # units = round(as.numeric(posUnits * sizing) / hedgeRatio * prices)
    # units * prices * hedgeRatio

    # stats arbitragy triggers
    if(zScore >= threshold & (adf.signal <= adf.threshold | jc.signal <= jc.threshold | hurst.signal <= hurst.threshold)){
        goLong = FALSE
        goShort = TRUE # buy
        closePos = FALSE
    }else if(zScore <= -threshold & adf.signal < adf.threshold){
        goLong = TRUE # sell
        goShort = FALSE
        closePos = FALSE
    }else if(abs(zScore) < 0.1 | adf.signal < adf.threshold | unrealizedPnL <= stoploss){
        goLong = FALSE
        goShort = FALSE
        closePos = TRUE # sell OR buy
    }

    # strategy logic
    if(r == (johansen.lookback+1)){
        lstRcd = initPortf
        lstPos = lstRcd[11:14]
    }else{
        lstRcd = Order.Book[r-johansen.lookback,]
        lstPos = lstRcd[11:14]
    }

    if(goLong){
        pos.fnl = positions + lstPos
        mkt.fnl = pos.fnl * data.frame(prices)
        inLong = TRUE
        inShort = FALSE
    }else if(goShort){
        pos.fnl = -positions + lstPos
        mkt.fnl = pos.fnl * data.frame(prices)
        inLong = FALSE
        inShort = TRUE
    }else if(closePos & (inLong | inShort)){
        pos.fnl = lstPos-lstPos
        mkt.fnl = pos.fnl * data.frame(prices)
        inLong = FALSE
        inShort = FALSE
    }else{
        pos.fnl = lstPos
        mkt.fnl = pos.fnl * data.frame(prices)
    }
    names(mkt.fnl) = paste0('MKT.', cols)
    sumMktValue = sum(mkt.fnl)
    actualValue = lstRcd[25] - sumMktValue

    unrealizedPnL = sum((data.frame(diff(tail(dat,2))[2,]) * lstPos)) / sum((data.frame(tail(dat,2)[1,]) * abs(lstPos)))
    unrealizedPnL = round(ifelse(is.nan(unrealizedPnL), 0, unrealizedPnL), 5)


    # PnL = sumMktValue + actualValue + unrealizedPnL
    PnL = round(sum((data.frame(diff(tail(dat,2))[2,]))) / sum((data.frame(tail(dat,2)[1,]))),5)

    orderBook = data.frame(tail(dat,1), adf.signal, jc.signal, hurst.signal, half.life, zScore, ols.r2,
                           pos.fnl, mkt.fnl, inLong, inShort, goLong, goShort, closePos,
                           sumMktValue, actualValue, unrealizedPnL, PnL)
    names(orderBook) = c(paste0('P.', names(dat)), 'ADF', 'Johansen', 'Hurst', 'H.L', 'ZScore', 'ols.r2',
                         paste0('Pos.', names(dat)), paste0('Mkt.', names(dat)),
                         'inLong', 'inShort', 'goLong', 'goShort', 'closePos',
                         'sumMktValue', 'actualValue', 'unrealizedPnL', 'PnL')
    if(r == (johansen.lookback+1)){
        names(initPortf) = names(orderBook)
        Order.Book = rbind(initPortf, orderBook)
    }else{
        Order.Book = rbind(Order.Book,orderBook)
    }

    cat(paste0('\n', unrealizedPnL, ' | ', PnL))

