rm(list=ls);gc()
library(caret)
library(xgboost)
library(quantmod)
source('tests/AutomatedSystems/IBPaperTradingIndicators.R')
Sys.setenv(TZ='US/Eastern')

AUDUSD = prepareForexData(ib.duration = "8 Y", ib.barsize = "1 day", Cur1 = "AUD", Cur2 = "USD",
                          oanda.granularity = 'D', QuandlSymbol1 = 'AUS', QuandlSymbol2 = 'USA', ibAcc = 'paper')
AUDUSD$OA.MID = (AUDUSD$OA.BIDASK[, 1:4] + AUDUSD$OA.BIDASK[, 5:8]) / 2

USDCAD = prepareForexData(ib.duration = "8 Y", ib.barsize = "1 day", Cur1 = "USD", Cur2 = "CAD",
                          oanda.granularity = 'D', QuandlSymbol1 = 'USA', QuandlSymbol2 = 'CAN', ibAcc = 'paper')
USDCAD$OA.MID = (USDCAD$OA.BIDASK[, 1:4] + USDCAD$OA.BIDASK[, 5:8]) / 2


# 1. Price model ----------------------------------------------------------
spreads = AUDUSD$IB.MID - 1/USDCAD$IB.MID
y = Cl(AUDUSD$IB.MID)
x = Cl(1/USDCAD$IB.MID)
# basic stats test
adf.ratio <- AugmentedDickeyFullerTest(Cl(spreads), type = "drift", lags = 1)
jc.test <- JohansenCointegrationTest(merge(y,x), type = "trace", ecdet = "none", K = 2)
half.life <- OrnsteinUhlenbeckHalfLife(Cl(spreads))$half.life.round
hurst.test <- HurstExponentTest(Cl(spreads), half.life)
hedgeRatio = HedgeRatioOLS(y, x)$beta

# zscore
zc.ma <- zscores.ma(Cl(spreads), 60, 10)

# model data
modelData = function(dat){
    finIndicators = finIndicatorHLC(dat)

    target <- lag(diff(Cl(dat)), -1)
    colnames(target) = 'target'
    all = na.omit(merge(finIndicators, target))

    test = all[(nrow(all)-100):nrow(all), ]
    training = all[1:(nrow(all)-101), ]
    preProcValues <- preProcess(as.data.frame(training[, -ncol(training)]), method = c("scale"))
    trainBC <- predict(preProcValues, as.data.frame(training))
    testBC <- predict(preProcValues, as.data.frame(test))
    # Reg and BC
    trainREG = trainBC
    testREG = testBC
    trainBC$target = as.factor(ifelse(trainBC$target >= 0, 1, 0))
    testBC$target = as.factor(ifelse(testBC$target >= 0, 1, 0))
    response <- "target"
    predictors <- setdiff(names(trainBC), response)


    return(list(all=all,
                trainBC=trainBC,
                testBC=testBC,
                trainREG=trainREG,
                testREG=testREG,
                response=response,
                predictors=predictors))
}
AUDUSD.Model = modelData(AUDUSD$IB.MID[, c(2, 3, 4)])
CADUSD.Model = modelData(1/USDCAD$IB.MID[, c(2, 3, 4)])





# 1 mean reversion
aud = Cl(AUDUSD$IB.MID)
cad = 1./Cl(AUDUSD$IB.MID)
pairs = merge(aud, cad)
trainlen = 250
lookback = 20

hedgeRatio = cbind(rep(NA, nrow(pairs)),rep(NA, nrow(pairs)))
numUnits = rep(NA, nrow(pairs))

for(t in (trainlen+1):nrow(pairs)){
    res=JohansenCointegrationTest(pairs[(t-trainlen):(t-1), ], type = "eigen", ecdet = 'const')
    hedgeRatio[t,]=as.numeric(res$jc.test@V[1:2,1])
    yport=pairs[(t-lookback+1):t, ] %*% hedgeRatio[t,]
    ma=mean(yport)
    mstd=sd(yport)
    zScore=(tail(yport,1)-ma)/mstd
    numUnits[t]=-(tail(yport,1)-ma)/mstd
}

positions=cbind(numUnits,numUnits)*hedgeRatio*pairs
pnl=rowSums(lag(positions,1)*diff(pairs))/lag(pairs,1)
ret=pnl/rowSums(abs(lag(positions,1)))

# 2 predict (dl)


















# Orders ------------------------------------------------------------------
tws <- twsConnect(port = 7496, clientId = 999)
if(long){
    Order.Id = reqIds(tws)
    IB.Contract = twsCurrency("AUD")
    IB.Order = twsOrder(orderId = Order.Id, action = 'BUY', totalQuantity = 10, orderType = 'MKT')
    Exe.IB.Order <- placeOrder(twsconn = tws, Contract = IB.Contract, Order = IB.Order)
    # cancelOrder(twsconn = tws, '4')
}else if(short){
    Order.Id = reqIds(tws)
    IB.Contract = twsCurrency("AUD")
    IB.Order = twsOrder(orderId = Order.Id, action = 'SELL', totalQuantity = 10, orderType = 'MKT') # risk management stopprices | allOrNone
    Exe.IB.Order <- placeOrder(twsconn = tws, Contract = IB.Contract, Order = IB.Order)
    # cancelOrder(twsconn = tws, '4')
}
reqOpenOrders(twsconn = tws)



# Account -----------------------------------------------------------------
IB.Acc = reqAccountUpdates(tws, subscribe = T)        # this will return a AccountUpdate object
twsPortfolioValue(x = IB.Acc)

twsDisconnect(tws)




Sys.setenv(TZ='GMT')
