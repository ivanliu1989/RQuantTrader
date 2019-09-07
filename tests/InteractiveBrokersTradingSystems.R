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
AUDUSD = prepareForexData(ib.duration = "8 Y", ib.barsize = "1 day", Cur1 = "AUD", Cur2 = "USD",
                          oanda.granularity = 'D', QuandlSymbol1 = 'AUS', QuandlSymbol2 = 'USA', ibAcc = 'paper')
USDCAD = prepareForexData(ib.duration = "8 Y", ib.barsize = "1 day", Cur1 = "USD", Cur2 = "CAD",
                          oanda.granularity = 'D', QuandlSymbol1 = 'USA', QuandlSymbol2 = 'CAN', ibAcc = 'paper')

# 1. Price model ----------------------------------------------------------
aud = Cl(AUDUSD$IB.MID); index(aud) = as.Date(index(aud))
cad = Cl(1/USDCAD$IB.MID); index(cad) = as.Date(index(cad))
pairs = na.omit(merge(aud, cad))
spreads = pairs[, 1] - pairs[, 2]
johansen.lookback = 126
adf.lookback = 63
hurst.lookback = 126
half.life.lookback = 126
zscore.threshold = 1
t = nrow(pairs)

basic.dt = data.frame(Date = tail(index(aud),1),
                      AUD = as.numeric(tail(aud,1)),
                      CAD = as.numeric(tail(cad,1)))


# 1. Statistical Tests ----------------------------------------------------
# Johansen - Cointegration
# ADF - Cointegration
# Hurst - Mean-reverting or Trending
jc.res=JohansenCointegrationTest(pairs[(t-adf.lookback):t, ], type = "eigen", ecdet = 'const')
adf.res=adf.test(Cl(spreads)[(t-adf.lookback):t])
hurst.res=as.numeric(tail(HurstExponentTest(Cl(spreads), hurst.lookback)$hurstKY, 1)) # > 0.5 trending | < 0.5 mean reverting
hedgeRatio=as.numeric(jc.res$jc.test@V[1:2,1])
half.life <- HalfLifeMeanReversion(spreads[(t-half.life.lookback):t, ])$half.life.round
hedgeRatio.ols=HedgeRatioOLS(aud[(t-half.life):t, ], cad[(t-half.life):t, ], add_const = TRUE)

adf.signal=adf.res$p.value
jc.signal=as.numeric(((jc.res$p.value[1]-jc.res$r.1)/((jc.res$p.value[3]-jc.res$p.value[1])/10)+10)/100)

cointegration = (jc.signal < 0.15 & adf.signal < 0.3)
meanreverting_trending = ifelse(hurst.res < 0.5, 'mean-reverting', 'trending')

if(meanreverting_trending == 'mean-reverting'){
    yport=pairs[(t-adf.lookback+1):t, ] %*% c(1, -hedgeRatio[2])
}else{
    yport=pairs[(t-adf.lookback+1):t, ] %*% c(1, -hedgeRatio.ols$beta)
}
zScore=zscores(yport)
zscore.signal=as.numeric(tail(zScore,1))
numUnits=-tail(zScore,1)
if(meanreverting_trending == 'mean-reverting'){
    positions=c(numUnits,numUnits)*c(1, -(-1/hedgeRatio[2]))*pairs[t]
}else{
    positions=c(1, -(-1/hedgeRatio.ols$beta))*pairs[t] # long & short
}

# 1.2 Meansurement and Report
stats.dt = data.frame(adf.signal = adf.signal,
                      jc.signal = jc.signal,
                      hurst.res = hurst.res,
                      cointegration = cointegration,
                      meanreverting_trending = meanreverting_trending,
                      half.life = half.life,
                      zscore.signal = zscore.signal,
                      hedgeRatio.jc = hedgeRatio[2],
                      hedgeRatio.ols = hedgeRatio.ols[2],
                      positions.aud = positions[,1],
                      positions.cad = positions[,2]
)
colnames(stats.dt) <- c('ADF.Test', 'Johansen.Test', 'Hurst.Test', 'Cointegrated', 'MeanReverting.Trending', 'Half.life', 'Zscore',
                        'HedgeRatio.Eigen', 'HedgeRatio.OLS', 'AUD.Pos', 'CAD.Pos')


# 2. Machine Learning Predictions -----------------------------------------
AUD.dat = HLC(x = AUDUSD$IB.MID)
if(meanreverting_trending == 'mean-reverting'){
    CAD.dat = 1/HLC(x = USDCAD$IB.MID)
}else{
    CAD.dat = HLC(x = USDCAD$IB.MID)
}
# Pricing model data
AUDUSD.Prices.Model = modelPricingFeatures(AUD.dat, 63)
CADUSD.Prices.Model = modelPricingFeatures(CAD.dat, 63)
# Candle model data
# AUDUSD.Consec.Ret.Model = as.data.frame(modelCandleFeatures(AUDUSD$IB.MID))
# USDCAD.Consec.Ret.Model = as.data.frame(modelCandleFeatures(USDCAD$IB.MID))
# Interest Rates
# AUDUSD.IR.model = modelBondIRFeatures(AUDUSD$Quandl.YC.Bond, AUDUSD$Quandl.YC.Bond2)
# USDCAD.IR.model = modelBondIRFeatures(USDCAD$Quandl.YC.Bond2, USDCAD$Quandl.YC.Bond)
# AUDUSD.IR.model = modelMultivariableFeatures(AUDUSD.IR.model)
# USDCAD.IR.model = modelMultivariableFeatures(USDCAD.IR.model)
# Index
# AUDUSD.Index.model = modelMultivariableFeatures(AUDUSD$Quandl.EquityIndex)
# USDCAD.Index.model = modelMultivariableFeatures(USDCAD$Quandl.EquityIndex)
# CBOT

AUDUSD.Model = AUDUSD.Prices.Model
CADUSD.Model = CADUSD.Prices.Model

# 2.1 H2O Deep Learning
AUD.h2o = h2oDeeplearingEnsemble(AUDUSD.Model$allBC, AUDUSD.Model$testBC, AUDUSD.Model$predictors, AUDUSD.Model$response, FALSE)
# save(AUD.h2o, file = 'tests/ModelResult/AUD_h2o_Models.RData')
Sys.sleep(5)
CAD.h2o = h2oDeeplearingEnsemble(CADUSD.Model$allBC, CADUSD.Model$testBC, CADUSD.Model$predictors, CADUSD.Model$response, FALSE)
# save(CAD.h2o, file = 'tests/ModelResult/CAD_h2o_Models.RData')

# 2.2 SVM
all.pints = nrow(AUDUSD.Model$allBC) - 200
train.points = round(all.pints * 0.9)
test.points = all.pints - train.points

AUD.caret = caretMachineLearningEnsemble(AUDUSD.Model$allBC, train.points, test.points)
save(AUD.caret, file = 'tests/ModelResult/AUD_caret_Models.RData')
CAD.caret = caretMachineLearningEnsemble(CADUSD.Model$allBC, train.points, test.points)
save(CAD.caret, file = 'tests/ModelResult/CAD_caret_Models.RData')

# 2.3 Blending Predictions and Report
AUD.h2o.pred = c(mean(unlist(AUD.h2o[1:5])), mean(unlist(AUD.h2o[11:15]))); names(AUD.h2o.pred) <- c('h2oDL.AUD', 'w.h2o.DL.AUD')
CAD.h2o.pred = c(mean(unlist(CAD.h2o[1:5])), mean(unlist(CAD.h2o[11:15]))); names(CAD.h2o.pred) <- c('h2oDL.CAD', 'w.h2o.DL.CAD')
AUD.caret.pred = unlist(AUD.caret[c(1:5,11:15)]); names(AUD.caret.pred) <- paste0(names(AUD.caret.pred), '.AUD')
CAD.caret.pred = unlist(CAD.caret[c(1:5,11:15)]); names(CAD.caret.pred) <- paste0(names(CAD.caret.pred), '.CAD')
all.AUD.pred = c(AUD.caret.pred[1:5], AUD.h2o.pred[1], AUD.caret.pred[6:10], AUD.h2o.pred[2])
all.CAD.pred = c(CAD.caret.pred[1:5], CAD.h2o.pred[1], CAD.caret.pred[6:10], CAD.h2o.pred[2])

fnl.w = c(0.5, 0.25, 0.12, 0.06, 0.04, 0.03)
fnl.AUD.pred = sum(ifelse(all.AUD.pred[7:12] < 0.5, 1-all.AUD.pred[1:6], all.AUD.pred[1:6]) * fnl.w); names(fnl.AUD.pred) = 'fnl.AUD.pred'
fnl.CAD.pred = sum(ifelse(all.CAD.pred[7:12] < 0.5, 1-all.CAD.pred[1:6], all.CAD.pred[1:6]) * fnl.w); names(fnl.CAD.pred) = 'fnl.CAD.pred'

ml.dt = c(all.AUD.pred, fnl.AUD.pred, all.CAD.pred, fnl.CAD.pred)


# 3. Risk Management ------------------------------------------------------
sd.lookback = 63
AUD.sd = sd(tail((Hi(AUDUSD$IB.MID) - Lo(AUDUSD$IB.MID)), sd.lookback))
CAD.sd = sd(tail((Hi(USDCAD$IB.MID) - Lo(USDCAD$IB.MID)), sd.lookback))
AUD.takeprofit = as.numeric(tail(aud, 1) + 1.5*AUD.sd)
AUD.stoploss = as.numeric(tail(aud, 1) - 1.5*AUD.sd)
CAD.takeprofit = as.numeric(tail(1/cad, 1) + 1.5*CAD.sd)
CAD.stoploss = as.numeric(tail(1/cad, 1) - 1.5*CAD.sd)

risk.dt = data.frame(AUD.takeprofit = AUD.takeprofit,
                     AUD.stoploss = AUD.stoploss,
                     CAD.takeprofit = CAD.takeprofit,
                     CAD.stoploss = CAD.stoploss)

# 4. Place Orders ---------------------------------------------------------
initAsset = 1000000
perc = 0.05
AUD.units = round(initAsset*perc*stats.dt$AUD.Pos)
CAD.units = round(initAsset*perc*stats.dt$CAD.Pos)


### Interactive Brokers ############################################

# Orders ------------------------------------------------------------------
# Sunday - Friday: 17:15 - 17:00 ET
# Limit Conditional (cash, cmdty, stkonly)
# Adjustable Stop Orders

tws <- twsConnect(port = 7497, clientId = 998)

if(meanreverting_trending == 'mean-reverting'){
    # CLOSE POSITIONS
    if(zscore.signal >= 1){
        # goshort: buy short & sell long
    }else if(zscore.signal <= -1){
        # golong: buy long & sell short
    }else if(abs(zscore.signal) <= 0.05){
        # close positions
    }else if(zscore.signal > 0 & fnl.CAD.pred >= 0.5 & fnl.AUD.pred <= 0.5){
        # goshort: buy short & sell long

    }else if(zscore.signal < 0 & fnl.CAD.pred <= 0.5 & fnl.AUD.pred >= 0.5){
        # golong: buy long & sell short
    }
}else{

    IB.Acc = reqAccountUpdates(tws, subscribe = T)        # this will return a AccountUpdate object
    Portfolio = twsPortfolioValue(x = IB.Acc)
    ### AUD ###
    AUD.action = ifelse(0.5-fnl.AUD.pred < 0, 'SELL', 'BUY')
    ### CLOSE POSITIONS
    if((AUD.action=='BUY' & Portfolio[Portfolio$local == 'AUD.USD', 'position'] < 0) | (AUD.action=='SELL' & Portfolio[Portfolio$local == 'AUD.USD', 'position'] > 0)){
        AUD.units = AUD.units + abs(Portfolio[Portfolio$local == 'AUD.USD', 'position'])
    }
    IB.Contract.AUD = twsCurrency("AUD")
    # Order
    Order.Id.AUD = reqIds(tws)
    IB.Order.AUD = twsOrder(orderId = Order.Id.AUD,
                            action = AUD.action,
                            totalQuantity = AUD.units,
                            orderType = 'LMT',
                            tif = 'DAY',
                            lmtPrice = round(ifelse(AUD.action == 'BUY', tail(Cl(AUDUSD$IB.BID),1), tail(Cl(AUDUSD$IB.ASK),1)),4))
    Exe.IB.Order.AUD <- placeOrder(twsconn = tws, Contract = IB.Contract.AUD, Order = IB.Order.AUD)
    # Take-profit
    Order.Id.AUD.TP = reqIds(tws)
    IB.TP.AUD = twsOrder(orderId = Order.Id.AUD.TP,
                         action = ifelse(AUD.action == 'BUY', 'SELL', 'BUY'),
                         totalQuantity = AUD.units,
                         orderType = 'LMT',
                         tif = 'DAY',
                         lmtPrice = round(ifelse(AUD.action == 'BUY', AUD.takeprofit, AUD.stoploss),4))
    Exe.IB.TP.AUD <- placeOrder(twsconn = tws, Contract = IB.Contract.AUD, Order = IB.TP.AUD)
    # Stop-loss
    Order.Id.AUD.SL = reqIds(tws)
    IB.SL.AUD = twsOrder(orderId = Order.Id.AUD.SL,
                         action = ifelse(AUD.action == 'BUY', 'BUY', 'SELL'),
                         totalQuantity = AUD.units,
                         orderType = 'LMT',
                         tif = 'DAY',
                         lmtPrice = round(ifelse(AUD.action == 'BUY', AUD.stoploss, AUD.takeprofit),4))
    Exe.IB.SL.AUD <- placeOrder(twsconn = tws, Contract = IB.Contract.AUD, Order = IB.SL.AUD)

    ### CAD ###
    CAD.action = ifelse(0.5-fnl.CAD.pred < 0, 'SELL', 'BUY')
    ### CLOSE POSITIONS
    if((CAD.action=='BUY' & Portfolio[Portfolio$local == 'USD.CAD', 'position'] < 0) | (CAD.action=='SELL' & Portfolio[Portfolio$local == 'USD.CAD', 'position'] > 0)){
        CAD.units = CAD.units + abs(Portfolio[Portfolio$local == 'USD.CAD', 'position'])
    }

    IB.Contract.CAD = twsCurrency("USD",currency='CAD',exch='IDEALPRO')
    # Order
    Order.Id.CAD = reqIds(tws)
    IB.Order.CAD = twsOrder(orderId = Order.Id.CAD,
             action = CAD.action,
             totalQuantity = CAD.units,
             orderType = 'LMT',
             tif = 'DAY',
             lmtPrice = round(ifelse(AUD.action == 'BUY', tail(Cl(USDCAD$IB.BID),1), tail(Cl(USDCAD$IB.ASK),1)),4))
    Exe.IB.Order.CAD <- placeOrder(twsconn = tws, Contract = IB.Contract.CAD, Order = IB.Order.CAD)
    # Take-profit
    Order.Id.CAD.TP = reqIds(tws)
    IB.TP.CAD = twsOrder(orderId = Order.Id.CAD.TP,
             action = ifelse(CAD.action == 'BUY', 'SELL', 'BUY'),
             totalQuantity = CAD.units,
             orderType = 'LMT',
             tif = 'DAY',
             lmtPrice = round(ifelse(CAD.action == 'BUY', CAD.takeprofit, CAD.stoploss),4))
    Exe.IB.TP.CAD <- placeOrder(twsconn = tws, Contract = IB.Contract.CAD, Order = IB.TP.CAD)
    # Stop-loss
    Order.Id.CAD.SL = reqIds(tws)
    IB.SL.CAD = twsOrder(orderId = Order.Id.CAD.SL,
             action = ifelse(CAD.action == 'BUY', 'BUY', 'SELL'),
             totalQuantity = CAD.units,
             orderType = 'LMT',
             tif = 'DAY',
             lmtPrice = round(ifelse(CAD.action == 'BUY', CAD.stoploss, CAD.takeprofit),4))
    Exe.IB.SL.CAD <- placeOrder(twsconn = tws, Contract = IB.Contract.CAD, Order = IB.SL.CAD)

    # cancelOrder(twsconn = tws, '4')
}


# Account -----------------------------------------------------------------
# reqOpenOrders(twsconn = tws)
IB.Acc = reqAccountUpdates(tws, subscribe = T)        # this will return a AccountUpdate object
twsPortfolioValue(x = IB.Acc)

AUD.OrderBook = IB.Acc[[2]][[length(IB.Acc[[2]]) - 1]]
CAD.OrderBook = IB.Acc[[2]][[length(IB.Acc[[2]])]]
twsDisconnect(tws)

# Order Information -------------------------------------------------------
order.dt = data.frame(AUD.action = AUD.action,
                      AUD.units = AUD.OrderBook$portfolioValue$position,
                      AUD.prices = AUD.OrderBook$portfolioValue$marketPrice,
                      AUD.value = AUD.OrderBook$portfolioValue$marketValue,
                      CAD.action = CAD.action,
                      CAD.units = CAD.OrderBook$portfolioValue$position,
                      CAD.prices = CAD.OrderBook$portfolioValue$marketPrice,
                      CAD.value = CAD.OrderBook$portfolioValue$marketValue)

### Interactive Brokers ############################################


# Order Books -------------------------------------------------------------
modelConfig = list(basic.dt = basic.dt,
                   stats.dt = stats.dt,
                   ml.dt = ml.dt,
                   risk.dt = risk.dt,
                   order.dt = order.dt)
save(modelConfig, file = paste0('tests/ModelResult/AUDCAD_PairTrading_', modelConfig$basic.dt$Date, '.RData'))

orderBook = read.csv('~/analytics/common/PaperTradingOrderBook/IB_FX_AUDCAD_v1.csv')
orderBook = rbind(orderBook, as.data.frame(c(basic.dt, stats.dt, ml.dt, risk.dt, order.dt)))
write.csv(orderBook, file = '~/analytics/common/PaperTradingOrderBook/IB_FX_AUDCAD_v1.csv', row.names = FALSE)


# Write to SQL ------------------------------------------------------------
# SQLQUERY = as.data.frame(t(unlist(c(basic.dt, stats.dt, ml.dt, risk.dt, order.dt))))
# conn <- InsitesSqlConnectJDBC()
# res = dbWriteTable(conn, "ivan.dbo.IBFXAUDCADHandBook", SQLQUERY)
# queryResults <- dbGetQuery(conn, "select * from ivan.dbo.IBFXAUDCADHandBook")

Sys.setenv(TZ='GMT')

