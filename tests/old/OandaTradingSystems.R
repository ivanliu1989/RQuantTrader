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
source('tests/AutomatedSystems/IBPaperTradingIndicators.R')
source('tests/h2o/h2oDeeplearningMain.R')
source('tests/caret/caretMachineLearningEnsemble.R')
Sys.setenv(TZ='US/Eastern')

# Sunday approximately 5 p.m. to Friday 5 p.m
AUDUSD = prepareForexOandaData(oanda.count = 2500, Cur1 = "AUD", Cur2 = "USD", oanda.granularity = 'D', QuandlSymbol1 = 'AUS', QuandlSymbol2 = 'USA')
USDCAD = prepareForexOandaData(oanda.count = 2500, Cur1 = "USD", Cur2 = "CAD", oanda.granularity = 'D', QuandlSymbol1 = 'USA', QuandlSymbol2 = 'CAN')
# save(AUDUSD, USDCAD, file = 'data/SampleTradingDatasetFXOanda.RData')

# 1. Price model ----------------------------------------------------------
aud = Cl(AUDUSD$OA.MID); index(aud) = as.Date(index(aud))
cad = Cl(1/USDCAD$OA.MID); index(cad) = as.Date(index(cad))
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
AUD.dat = AUDUSD$OA.MID[, c(2,3,1)]
if(meanreverting_trending == 'mean-reverting'){
    CAD.dat = 1/USDCAD$OA.MID[, c(2,3,1)]
}else{
    CAD.dat = USDCAD$OA.MID[, c(2,3,1)]
}
# model data
modelData = function(dat, test.p = 63){
    finIndicators = finIndicatorHLC(dat)

    target <- lag(diff(Cl(dat)), -1)
    colnames(target) = 'target'
    all = na.omit(merge(finIndicators, target))

    test = all[(nrow(all)-test.p):nrow(all), ]
    training = all[1:(nrow(all)-(test.p+1)), ]
    preProcValues <- preProcess(as.data.frame(all[, -ncol(all)]), method = c("scale"))
    trainBC <- predict(preProcValues, as.data.frame(training))
    testBC <- predict(preProcValues, as.data.frame(test))
    allBC <- predict(preProcValues, as.data.frame(all))
    # Reg and BC
    trainREG = trainBC
    testREG = testBC
    allREG = allBC
    trainBC$target = as.factor(ifelse(trainBC$target >= 0, 1, 0))
    testBC$target = as.factor(ifelse(testBC$target >= 0, 1, 0))
    allBC$target = as.factor(ifelse(allBC$target >= 0, 1, 0))
    response <- "target"
    predictors <- setdiff(names(trainBC), response)

    return(list(all=all,
                allBC=allBC,
                allREG=allREG,
                trainBC=trainBC,
                testBC=testBC,
                trainREG=trainREG,
                testREG=testREG,
                response=response,
                predictors=predictors))
}
AUDUSD.Model = modelData(AUD.dat, 63)
CADUSD.Model = modelData(CAD.dat, 63)

# 2.1 H2O Deep Learning
AUD.h2o = h2oDeeplearingEnsemble(AUDUSD.Model$allBC, AUDUSD.Model$testBC, AUDUSD.Model$predictors, AUDUSD.Model$response, FALSE)
save(AUD.h2o, file = 'tests/ModelResult/AUD_h2o_Models.RData')
CAD.h2o = h2oDeeplearingEnsemble(CADUSD.Model$allBC, CADUSD.Model$testBC, CADUSD.Model$predictors, CADUSD.Model$response, FALSE)
save(CAD.h2o, file = 'tests/ModelResult/CAD_h2o_Models.RData')

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
CAD.caret.pred = unlist(CAD.caret[c(1:5,11:15)]); names(CAD.caret.pred) <- paste0(names(CAD.caret.pred), '.AUD')
all.AUD.pred = c(AUD.caret.pred[1:5], AUD.h2o.pred[1], AUD.caret.pred[6:10], AUD.h2o.pred[2])
all.CAD.pred = c(CAD.caret.pred[1:5], CAD.h2o.pred[1], CAD.caret.pred[6:10], CAD.h2o.pred[2])

fnl.w = c(0.5, 0.25, 0.12, 0.06, 0.04, 0.03)
fnl.AUD.pred = sum(ifelse(all.AUD.pred[7:12] < 0.5, 1-all.AUD.pred[1:6], all.AUD.pred[1:6]) * fnl.w); names(fnl.AUD.pred) = 'fnl.AUD.pred'
fnl.CAD.pred = sum(ifelse(all.CAD.pred[7:12] < 0.5, 1-all.CAD.pred[1:6], all.CAD.pred[1:6]) * fnl.w); names(fnl.CAD.pred) = 'fnl.CAD.pred'

ml.dt = c(all.AUD.pred, all.CAD.pred, fnl.AUD.pred, fnl.CAD.pred)


# 3. Risk Management ------------------------------------------------------
sd.lookback = 63
AUD.sd = sd(tail((Hi(AUDUSD$OA.MID) - Lo(AUDUSD$OA.MID)), sd.lookback))
CAD.sd = sd(tail((Hi(USDCAD$OA.MID) - Lo(USDCAD$OA.MID)), sd.lookback))
AUD.takeprofit = as.numeric(tail(aud, 1) + 2*AUD.sd)
AUD.stoploss = as.numeric(tail(aud, 1) - 2*AUD.sd)
CAD.takeprofit = as.numeric(tail(1/cad, 1) + 2*CAD.sd)
CAD.stoploss = as.numeric(tail(1/cad, 1) - 2*CAD.sd)

risk.dt = data.frame(AUD.takeprofit = AUD.takeprofit,
                     AUD.stoploss = AUD.stoploss,
                     CAD.takeprofit = CAD.takeprofit,
                     CAD.stoploss = CAD.stoploss)

# 4. Place Orders ---------------------------------------------------------
initAsset = 1000000
perc = 0.05
AUD.units = round(initAsset*perc*stats.dt$AUD.Pos)
CAD.units = round(initAsset*perc*stats.dt$CAD.Pos)

### Oanda ############################################
library(httr)
ACCOUNT_ID = '101-011-4686012-001'
# getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('AUD_USD'))
# getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID)
# getOandaCurPricing(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('AUD_USD'))

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
    # CLOSE POSITIONS
    aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD',
                                     UNITS = sign(0.5-fnl.AUD.pred)*AUD.units)#,
                                     # TAKEPROFIT = ifelse(fnl.AUD.pred<0.5, AUD.stoploss, AUD.takeprofit),
                                     # STOPLOSS = ifelse(fnl.AUD.pred>0.5, AUD.stoploss, AUD.takeprofit))
    cad.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CAD',
                                     UNITS = sign(0.5-fnl.CAD.pred)*CAD.units)#,
                                     # TAKEPROFIT = ifelse(fnl.CAD.pred<0.5, CAD.stoploss, CAD.takeprofit),
                                     # STOPLOSS = ifelse(fnl.CAD.pred>0.5, CAD.stoploss, CAD.takeprofit))
}
### Oanda ############################################

order.dt = data.frame(AUD.units = AUD.units,
                     CAD.units = CAD.units)
modelConfig = list(basic.dt = basic.dt,
                   stats.dt = stats.dt,
                   ml.dt = ml.dt,
                   risk.dt = risk.dt,
                   order.dt = order.dt)
save(modelConfig, file = paste0('tests/ModelResult/AUDCAD_PairTrading_', modelConfig$basic.dt$Date, '.RData'))

Sys.setenv(TZ='GMT')
# Orders ------------------------------------------------------------------
# tws <- twsConnect(port = 7496, clientId = 999)
# if(long){
#     Order.Id = reqIds(tws)
#     IB.Contract = twsCurrency("AUD")
#     IB.Order = twsOrder(orderId = Order.Id, action = 'BUY', totalQuantity = 10, orderType = 'MKT')
#     Exe.IB.Order <- placeOrder(twsconn = tws, Contract = IB.Contract, Order = IB.Order)
#     # cancelOrder(twsconn = tws, '4')
# }else if(short){
#     Order.Id = reqIds(tws)
#     IB.Contract = twsCurrency("AUD")
#     IB.Order = twsOrder(orderId = Order.Id, action = 'SELL', totalQuantity = 10, orderType = 'MKT') # risk management stopprices | allOrNone
#     Exe.IB.Order <- placeOrder(twsconn = tws, Contract = IB.Contract, Order = IB.Order)
#     # cancelOrder(twsconn = tws, '4')
# }
# reqOpenOrders(twsconn = tws)


# Account -----------------------------------------------------------------
# IB.Acc = reqAccountUpdates(tws, subscribe = T)        # this will return a AccountUpdate object
# twsPortfolioValue(x = IB.Acc)
# twsDisconnect(tws)




