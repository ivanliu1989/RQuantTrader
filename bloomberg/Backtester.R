rm(list = ls());gc()
ACCESS_TOKEN = 'PUT_YOUR_TOKEN_HERE'
ACCOUNT_TYPE = 'real'
loadQuantPackages()
Sys.setenv(TZ='US/Eastern')

xgbBackTest = function(train){
    trainBC = as.data.frame(train[1:round(nrow(train)*4/5),])
    testBC = as.data.frame(train[round(nrow(train)*4/5):nrow(train),])

    predictors = names(trainBC)[!names(trainBC) %in% c('target')]
    response = 'target'

    dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
    dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
    watchlist <- list(train = dtrain, eval = dtest)

    param <- list(
        max_depth = 3,
        eta = 0.1,
        nthread = 6,
        # objective = "binary:logistic",
        objective = "reg:linear",
        eval_metric = "rmse",
        booster = "gbtree",
        gamma = 0.001,
        min_child_weight = 1,
        subsample = 1,
        colsample_bytree = 0.06
        # max_depth = 6,
        # eta = 0.01,
        # nthread = 6,
        # objective = "binary:logistic",
        # eval_metric = "rmse",
        # booster = "gbtree",
        # gamma = 0.0001,
        # min_child_weight = 1,
        # subsample = 1,
        # colsample_bytree = 0.06
    )


    for(i in 1:5){
        xgbFitClass <- xgb.train(param,
                                 dtrain,
                                 nrounds = 1000,
                                 watchlist,
                                 early_stopping_rounds = 100,
                                 print_every_n = 1000
        )
        if(i == 1){
            best_score = xgbFitClass$best_score
            xgbFit = xgbFitClass
        }else{
            if(best_score > xgbFitClass$best_score){
                best_score = xgbFitClass$best_score
                xgbFit = xgbFitClass
            }
        }
    }
    return(xgbFit)
}

# Rolling Moving Average
MaRatio <- function(x, N){
    Mavg <- rollapply(x, N , mean)
    colnames(Mavg) <- 'Price.Ratio.MA'
    Mavg
}
# Rolling Standard Deviation
Sd <- function(x, N){
    Stand.dev <- rollapply(x, N, sd)
    colnames(Stand.dev) <- "Price.Ratio.SD"
    Stand.dev
}
# Z Score
ZScore <- function(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD){
    a1 <- Price.Ratio
    b1 <- Price.Ratio.MA
    c1 <- Price.Ratio.SD
    z <- (a1-b1)/c1
    colnames(z)<- 'Z.Score'
    z
}

PRICE.OA = prepareForexOandaPrices(ACCOUNT_TYPE, ACCESS_TOKEN,
                                   oanda.count = 2500, Cur1 = 'AUD', Cur2 = 'CAD',
                                   oanda.granularity = 'D')
price.oa = PRICE.OA$OA.MID
spreads = Cl(price.oa)
portf = na.omit(ROC(spreads, n = 1, type = 'discrete')); names(portf) = 'AUDCAD'
prices = na.omit(spreads); names(prices) = 'AUDCAD'

# 2. Configurations -------------------------------------------------------
inLong = FALSE
inShort = FALSE
initAssets = 1000000
posiRate = 0.5
threshold = 1
adf.threshold = 0.5
jc.threshold = 0.5
hurst.threshold = 0.8
obs = nrow(portf)
train.length = 200
ml.length = 1800
backtest.n = 1800
min.lookback = 21
max.lookback = 63
pred.n = 6

rnd = 0
initPortf = c(rep(0,7), rep(FALSE, 5), 0, initAssets, 0, initAssets)
initPortf = data.frame(t(initPortf))
unrealizedPnL = 0

pred = c()
for(r in (backtest.n+1):obs){

    goLong = FALSE
    goShort = FALSE
    closePos = FALSE

    # Model data --------------------------------------------------------------
    tmp.price.oa = price.oa[(r-train.length):r, ]
    tmp.prices = Cl(tmp.price.oa)
    ml.price.oa = price.oa[(r-ml.length):r, ]
    spreads = tmp.prices

    # Half Life ---------------------------------------------------------------
    partial.y = fit.par(spreads)
    pvmr.par = partial.y$pvmr # 0 to 1 indicating mean reversion
    if(partial.y$rho<0){
        half.life.par = 0
        look.back = HalfLifeMeanReversion(spreads)$half.life.round
    }else{
        half.life.par = log(0.5)/log(partial.y$rho)
        look.back = round(half.life.par)
    }
    look.back = min(max(c(look.back, min.lookback)), max.lookback)

    # 3. Statistics Test ------------------------------------------------------
    # Stationary Tests
    # ADF
    adf.res = adf.test(spreads, k = 0)
    adf.res.lag1 = adf.test(spreads, k = 1) #price change
    adf.signal=adf.res$p.value
    adf.signal.lag1=adf.res.lag1$p.value
    # Hurst
    hurst.signal = tryCatch({
        hurst.res = hurstexp(spreads, d = 2, display = F)
        return(hurst.res$Hal)
    }, error = function(e){
        hurst.res = HurstExponentTest(spreads, lookback = look.back)
        return(as.numeric(tail(hurst.res$hurstKY, 1)))
    })


    # Variance ratio test
    vr.ratio = Auto.VR(spreads)
    var.ratio = vr.ratio$stat/vr.ratio$sum

    # 4. Mean Reversion -------------------------------------------------------
    # hedge ratio
    spreads.MA <- MaRatio(spreads, look.back)
    spreads.SD <- Sd(spreads, look.back)
    Z.Score <- ZScore(spreads,spreads.MA,spreads.SD)
    # Z.Score <- zscores.ma(spreads, 21, 8)

    # Position Sizing
    zScore=tail(Z.Score,1)
    numUnits=-tail(zScore,1)
    # Dollar neutrality
    sizing = abs(numUnits)/1
    posUnits = initAssets * posiRate
    dollars = round(as.numeric(posUnits * sizing))
    # positions
    last.price = tail(tmp.prices,1)
    positions = as.data.frame(round(dollars / tail(last.price,1)))
    names(positions) = paste0('POS.AUDCAD')


    # 5. Risk Management ------------------------------------------------------
    oa.ret = lag(ROC(Cl(ml.price.oa), n = pred.n, type = 'discrete'), -pred.n); names(oa.ret) = 'target'
    oa.ret = ifelse(oa.ret >= 0, 1,0)  ##<<<===============================================
    price.feat.aud =  prepareMachinelearningFeatures(ml.price.oa)
    xgbFitClass = xgbBackTest(na.omit(merge(price.feat.aud, oa.ret)))

    # price.feat.aud =  tail(prepareMachinelearningFeatures(tail(ml.price.oa, 201)), 1)
    aud.ml.ret = predict(xgbFitClass, data.matrix(tail(price.feat.aud,1)))
    aud.ret.multiplier = sign(aud.ml.ret - 0.5) * (abs(aud.ml.ret - 0.5) + 0.5)/0.5 ##<<<===============================================


    # 5. Execution ------------------------------------------------------------

    # strategy logic
    if(r == (backtest.n+1)){
        lstRcd = initPortf
        lstPos = lstRcd[,6]
    }else{
        lstRcd = Order.Book[r-backtest.n,]
        lstPos = lstRcd[,6]
    }

    # stats arbitragy triggers
    if(zScore >= threshold & aud.ml.ret <= 0.5 ){ # 0.5 ##<<<===============================================
        # if(zScore >= threshold){
        goLong = FALSE
        goShort = TRUE # buy
        closePos = FALSE
    }else if(zScore <= -threshold & aud.ml.ret >= 0.5){ # 0.5 ##<<<===============================================
        # }else if(zScore <= -threshold){
        goLong = TRUE # sell
        goShort = FALSE
        closePos = FALSE
    }else if((inLong & zScore > threshold) | (inShort & zScore < -threshold)){
        goLong = FALSE
        goShort = FALSE
        closePos = TRUE # sell OR buy
    }

    if(goLong){
        pos.fnl = 1 * (aud.ret.multiplier^6) * posiRate + lstPos
        # pos.fnl = 1 * posiRate + lstPos
        mkt.fnl = pos.fnl * data.frame(last.price)
        inLong = TRUE
        inShort = FALSE
    }else if(goShort){
        pos.fnl = -1 * (aud.ret.multiplier^6) * posiRate  + lstPos
        # pos.fnl = -1 * posiRate + lstPos
        mkt.fnl = pos.fnl * data.frame(last.price)
        inLong = FALSE
        inShort = TRUE
    }else if(closePos & (inLong | inShort)){
        pos.fnl = lstPos-lstPos
        mkt.fnl = pos.fnl * data.frame(last.price)
        inLong = FALSE
        inShort = FALSE
    }else{
        pos.fnl = lstPos
        mkt.fnl = pos.fnl * data.frame(last.price)
    }
    names(mkt.fnl) = paste0('MKT.', 'AUDCAD')
    sumMktValue = sum(mkt.fnl)
    actualValue = lstRcd[14] - sumMktValue

    unrealizedPnL = sum((data.frame(diff(tail(tmp.prices,2))[2,]) * lstPos)) #/ sum((data.frame(tail(tmp.prices,2)[1,]) * abs(lstPos)))
    unrealizedPnL = round(ifelse(is.nan(unrealizedPnL), 0, unrealizedPnL), 5)

    PnL = round(sum((data.frame(diff(tail(tmp.prices,2))[2,]))),5) #/ sum((data.frame(tail(tmp.prices,2)[1,])))

    orderBook = data.frame(last.price, adf.signal, hurst.signal, half.life.par, zScore,
                           pos.fnl, mkt.fnl, inLong, inShort, goLong, goShort, closePos,
                           sumMktValue, actualValue, unrealizedPnL, PnL)
    names(orderBook) = c(paste0('P.', names(tmp.prices)), 'ADF', 'Hurst', 'H.L', 'ZScore',
                         paste0('Pos.', names(tmp.prices)), paste0('Mkt.', names(tmp.prices)),
                         'inLong', 'inShort', 'goLong', 'goShort', 'closePos',
                         'sumMktValue', 'actualValue', 'unrealizedPnL', 'PnL')
    if(r == (backtest.n+1)){
        names(initPortf) = names(orderBook)
        Order.Book = rbind(initPortf, orderBook)
    }else{
        Order.Book = rbind(Order.Book,orderBook)
    }
    pred = c(pred, aud.ml.ret)
    cat(paste0('\n',rownames(orderBook), " | ", unrealizedPnL * 100, ' | ', PnL * 100, " | Pred:", aud.ml.ret, " | ZScore:", zScore))
}

Order.Book = as.xts(Order.Book[-1,], order.by = as.Date(rownames(Order.Book[-1,])))
ret = Order.Book[,c('unrealizedPnL', 'PnL', 'ADF', 'Pos.OA.Mid.Close')]
names(ret) = c('Strategy', 'Benchmark', 'ADF', "Positions")
cat(paste0('\n Returns: ',sum(ret[,1]), '|', sum(ret[,2])))

par(mfcol = c(2,1))
chart_Series(cumsum(ret[, 1] * 100), name = paste0("Cumulative Returns ", round(sum(ret[, 1] * 100),2)," %"))
chart_Series(cumsum(ret[, 2] * 100), name = paste0("Cumulative Returns ", round(sum(ret[, 2] * 100),2)," %"))

### save results
zScore1_thres1_ml6_train252_reg01_xgb07 = ret
zScore1_thres1_ml6_train252_reg01_xgb006 = ret # champian

zScore1_threshold1_ml6_sizing_train201_reg_ret_pos02_xgb1 = ret #
zScore1_threshold1_ml6_sizing_train252_reg_ret_pos02 = ret
zScore1_threshold1_ml6_sizing_train252_reg_ret = ret
zScore1_threshold1_ml6_sizing_train252_reg_sl = ret
zScore1_threshold1_ml6_sizing_train252_cl_sl = ret
zScore1_threshold1_ml6_sizing_train201_iter = ret
zScore1_threshold1_ml3_sizing_train201_iter = ret
zScore1_threshold1_ml6_sizing_train201 = ret
zScore1_threshold1_ml6_sizing_train252 = ret
zScore1_threshold1.5_ml6_sizing_train252 = ret
zScore1_threshold1.5 = ret
zScore1_threshold2 = ret
zScore1_threshold1 = ret
zScore1_threshold1_train201 = ret
zScore1_threshold1_train201_ml6 = ret
zScore2_threshold1 = ret

save(zScore1_thres1_ml6_train252_reg01_xgb07,
     zScore1_thres1_ml6_train252_reg01_xgb006,
     zScore1_threshold1_ml6_sizing_train201_reg_ret_pos02_xgb1,
     zScore1_threshold1_ml6_sizing_train252_reg_ret_pos02,
     zScore1_threshold1_ml6_sizing_train252_reg_ret,
     zScore1_threshold1_ml6_sizing_train252_reg_sl,
     zScore1_threshold1_ml6_sizing_train252_cl_sl,
     zScore1_threshold1_ml6_sizing_train201,
     zScore1_threshold1_ml6_sizing_train252,
     zScore1_threshold1.5_ml6_sizing_train252,
     # zScore1_threshold1_train201,
     # zScore1_threshold1_train201_ml6,
     zScore1_threshold1.5,
     zScore1_threshold2,
     zScore1_threshold1,
     zScore2_threshold1,
     file = "./bloomberg/backtestResult.RData"
)

load("./bloomberg/backtestResult.RData")
