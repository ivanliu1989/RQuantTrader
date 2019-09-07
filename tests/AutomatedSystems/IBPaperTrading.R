USDCAD = prepareForexData(ib.duration = "6 Y", ib.barsize = "1 day", Cur1 = "USD", Cur2 = "CAD",
                          oanda.granularity = 'D', QuandlSymbol1 = 'CAN', QuandlSymbol2 = 'USA')

AUDUSD = prepareForexData(ib.duration = "6 Y", ib.barsize = "1 day", Cur1 = "AUD", Cur2 = "USD",
                          oanda.granularity = 'D', QuandlSymbol1 = 'AUS', QuandlSymbol2 = 'USA')

# save(USDCAD, AUDUSD, file = '~/analytics/common/IBBacktesting_20170103.RData')
# load('~/analytics/common/IBBacktesting_20161218.RData')


# ### S&P 500 Index
# library(IBrokers)
# instrument = 'SPX'
# tws <- twsConnect(port = 7497, clientId = 998)
#
# idx = twsIndex(symbol = instrument)
# ccy <- reqContractDetails(tws, idx)[[1]]$contract
# IB.BIDASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = '1 day',
#                                duration = '1 Y', useRTH = "1")
#
#
# ### AAPL
# instrument = 'AAPL'
# stk = twsSTK(symbol = instrument)
# ccy <- reqContractDetails(tws, stk)[[1]]$contract
# IB.BIDASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = '1 day',
#                                duration = '1 Y', useRTH = "1", whatToShow = 'BID_ASK')


### Model
library(RQuant)
library(RQuantTrader)
library(quantmod)
price.ratio <- getPriceRatio(Cl(AUDUSD$IB.MID), Cl(USDCAD$IB.MID), log = T)
all.points = length(price.ratio)
all.periods = 1:all.points
all.date.idx = as.character(index(price.ratio))
train.periods = paste0(all.date.idx[1], "/", all.date.idx[round(all.points*0.8)])
test.periods = paste0(all.date.idx[round(all.points*0.8)+1], "/", all.date.idx[all.points])

# 1. EquityIndex
# features
all.equity.index = AUDUSD$Quandl.EquityIndex[all.date.idx,]

# for(idx in 1:ncol(all.equity.index)){
#     print(colnames(all.equity.index)[idx])
#     finIndicators = finIndicatorCommon(all.equity.index[, idx])
#     colnames(finIndicators) = paste0(colnames(all.equity.index)[idx], '.', colnames(finIndicators))
#
#     if(idx == 1){
#         finIndicatorsAll = finIndicators
#     }else{
#         finIndicatorsAll = merge(finIndicators, finIndicatorsAll)
#     }
# }

equityIdxRet1 <- ROC(all.equity.index, n = 1, type = "discrete")
equityIdxRet7 <- ROC(all.equity.index, n = 7, type = "discrete")
equityIdxRet14 <- ROC(all.equity.index, n = 14, type = "discrete")
equityIdxRet28 <- ROC(all.equity.index, n = 28, type = "discrete")

equityIdxMACD <- all.equity.index
for(idx in 1:ncol(all.equity.index)){
    macd_data = MACD(all.equity.index[, idx], nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = TRUE)
    equityIdxMACD[,idx] <- macd_data$macd - macd_data$signal
}
tail(equityIdxMACD)

# model data
target <- ifelse(lag(diff(price.ratio), -1) >= 0, 1, -1)
all = merge(finIndicatorsAll, target)
training_data = na.omit(all[train.periods, ])
test_data = na.omit(all[test.periods, ])

library(e1071)
SVM = svm(as.factor(price.ratio)~., data=training_data, kernel="linear",cost=1,gamma=0.5)
idx.svm.pred = predict(SVM, test_data, type = "class")
idx.svm.pred = as.numeric(idx.svm.pred)-1

SVM = svm(as.factor(price.ratio)~., data=training_data, kernel="radial",cost=1,gamma=0.5)
idx.svmR.pred = predict(SVM, test_data, type = "class")
idx.svmR.pred = as.numeric(idx.svmR.pred)-1

GLM = glm(as.factor(price.ratio)~., data=training_data, family = "binomial")
idx.glm.pred = predict(GLM, test_data, type = "response")
idx.glm.pred = ifelse(idx.glm.pred >= 0.5, 1, 0)

EI.Pred = ifelse((idx.svm.pred + idx.glm.pred + idx.svmR.pred)/3 >= 0.5, 1, -1)
EI.Acc = (sum(EI.Pred == test_data$price.ratio)/nrow(test_data))*100
print(EI.Acc) # 51.18483


# 2. Interest Rates
cad.bonds = USDCAD$Quandl.YC.Bond
names(cad.bonds) <- c('CAN2Y', 'CAN3Y', 'CAN5Y', 'CAN10Y')
aud.bonds = AUDUSD$Quandl.YC.Bond
names(aud.bonds) <- c('AUS2Y', 'AUS3Y', 'AUS5Y', 'AUS10Y')
usa.bonds = AUDUSD$Quandl.YC.Bond2
names(usa.bonds) <- c('USA2Y', 'USA3Y', 'USA5Y', 'USA10Y')

all.bonds <- na.omit(merge(aud.bonds, cad.bonds, usa.bonds))

# Uncovered Interest Rate Parity
all.bonds$IR.AUSUSA.2 = (all.bonds$AUS2Y+100)/(all.bonds$USA2Y+100)
all.bonds$IR.AUSUSA.3 = (all.bonds$AUS3Y+100)/(all.bonds$USA3Y+100)
all.bonds$IR.AUSUSA.5 = (all.bonds$AUS5Y+100)/(all.bonds$USA5Y+100)
all.bonds$IR.AUSUSA.10 = (all.bonds$AUS10Y+100)/(all.bonds$USA10Y+100)
all.bonds$IR.CANUSA.2 = (all.bonds$CAN2Y+100)/(all.bonds$USA2Y+100)
all.bonds$IR.CANUSA.3 = (all.bonds$CAN3Y+100)/(all.bonds$USA3Y+100)
all.bonds$IR.CANUSA.5 = (all.bonds$CAN5Y+100)/(all.bonds$USA5Y+100)
all.bonds$IR.CANUSA.10 = (all.bonds$CAN10Y+100)/(all.bonds$USA10Y+100)
all.bonds$IR.AUSCAN.2 = (all.bonds$AUS2Y+100)/(all.bonds$CAN2Y+100)
all.bonds$IR.AUSCAN.3 = (all.bonds$AUS3Y+100)/(all.bonds$CAN3Y+100)
all.bonds$IR.AUSCAN.5 = (all.bonds$AUS5Y+100)/(all.bonds$CAN5Y+100)
all.bonds$IR.AUSCAN.10 = (all.bonds$AUS10Y+100)/(all.bonds$CAN10Y+100)

# Calculate Forward Rate
all.bonds$IR.AUS.10.2 = (all.bonds$AUS10Y+100)/(all.bonds$AUS2Y+100)
all.bonds$IR.AUS.10.3 = (all.bonds$AUS10Y+100)/(all.bonds$AUS3Y+100)
all.bonds$IR.AUS.10.5 = (all.bonds$AUS10Y+100)/(all.bonds$AUS5Y+100)
all.bonds$IR.CAN.10.2 = (all.bonds$CAN10Y+100)/(all.bonds$CAN2Y+100)
all.bonds$IR.CAN.10.3 = (all.bonds$CAN10Y+100)/(all.bonds$CAN3Y+100)
all.bonds$IR.CAN.10.5 = (all.bonds$CAN10Y+100)/(all.bonds$CAN5Y+100)

all.bonds = all.bonds[, -c(1:12)]

bondsRet1 <- ROC(all.bonds, n = 1, type = "discrete")
bondsRet7 <- ROC(all.bonds, n = 7, type = "discrete")
bondsRet14 <- ROC(all.bonds, n = 14, type = "discrete")
bondsRet28 <- ROC(all.bonds, n = 28, type = "discrete")
bondsMACD <- all.bonds
for(idx in 1:ncol(all.bonds)){
    macd_data = MACD(all.bonds[, idx], nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = TRUE)
    bondsMACD[,idx] <- macd_data$macd - macd_data$signal
}
tail(bondsMACD)

# model data
target <- ifelse(lag(diff(price.ratio), -1) >= 0, 1, -1)
all = merge(bondsRet1, bondsRet7, bondsRet14, bondsRet28, bondsMACD, target)
training_data = na.omit(all[train.periods, ])
test_data = na.omit(all[test.periods, ])

library(e1071)
SVM = svm(as.factor(price.ratio)~., data=training_data, kernel="linear",cost=1,gamma=0.5)
idx.svm.pred = predict(SVM, test_data, type = "class")
idx.svm.pred = as.numeric(idx.svm.pred)-1

SVM = svm(as.factor(price.ratio)~., data=training_data, kernel="radial",cost=1,gamma=0.5)
idx.svmR.pred = predict(SVM, test_data, type = "class")
idx.svmR.pred = as.numeric(idx.svmR.pred)-1

GLM = glm(as.factor(price.ratio)~., data=training_data, family = "binomial")
idx.glm.pred = predict(GLM, test_data, type = "response")
idx.glm.pred = ifelse(idx.glm.pred >= 0.5, 1, 0)

IR.Pred = ifelse((idx.svm.pred + idx.glm.pred + idx.svmR.pred)/3 >= 0.5, 1, -1)
IR.Acc = (sum(IR.Pred == test_data$price.ratio)/nrow(test_data))*100
print(IR.Acc) # 53.125


# 3. CBOT
AUDUSD$OA.CBOT


# 4. Hist Position
AUDUSD$OA.HistPos

# 5. Volume & BidAsk & Spreads
AUDUSD$OA.VOLUME
AUDUSD$OA.BIDASK
AUDUSD$OA.SPREADS.Perc

# 6. IB Bid Ask & Spreads & Volatility
AUDUSD$IB.SPREADS.Perc
AUDUSD$IB.VOLATILITY
AUDUSD$IB.MID
