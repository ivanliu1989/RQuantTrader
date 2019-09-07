rm(list=ls());gc()
loadQuantPackages()
library(Quandl)
Sys.setenv(TZ='GMT')

# Required Functions ------------------------------------------------------
# Machine learning main
xgbBackTest = function(train){
    trainBC = as.data.frame(train[1:round(nrow(train)*4/5),])
    testBC = as.data.frame(train[round(nrow(train)*4/5):nrow(train),])
    predictors = names(trainBC)[!names(trainBC) %in% c('target')]
    # predictors = predictors[!predictors%in%c('HigherOpen','White','Black','LowerOpen')]
    response = 'target'
    dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
    dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
    watchlist <- list(train = dtrain, eval = dtest)
    param <- list(
        max_depth = 3,
        eta = 0.1,
        nthread = 6,
        objective = "binary:logistic", #"reg:linear",
        eval_metric = "rmse",
        booster = "gbtree",
        gamma = 0.001,
        min_child_weight = 1,
        subsample = 1,
        colsample_bytree = 0.06
    )
    for(i in 1:15){
        xgbFitClass <- xgb.train(param,dtrain,nrounds = 1000,watchlist,
                                 early_stopping_rounds = 100,verbose = 0)
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
    var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
    # xgb.plot.importance(var.imp[1:20,])
    val = predict(xgbFit, dtest)
    xgbRes = binaryClassifierEvaluation(val, testBC$target)
    return(list(xgbFit = xgbFit,
                xgbRes = xgbRes,
                var.imp = var.imp))
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
    z <- (Price.Ratio-Price.Ratio.MA)/Price.Ratio.SD
    colnames(z)<- 'Z.Score'
    z
}

configs = list(
    order.book.file = '~/Common/audcad_intraday_d1/orderbook/ivanliu_AUDCAD_Pairs_d1.csv' # Order book location
    ,order.book.file.fin = '~/Common/audcad_intraday_d1/orderbook/ivanliu_AUDCAD_Pairs_d1_fin.csv' # Order book location
    ,log.file = file(paste0('~/Common/audcad_intraday_d1/log/ivanliu_AUDCAD_Pairs_d1_', as.Date(Sys.time()), '.Rout'), open = 'wt') # Log file location
    ,plotPath = "~/Common/audcad_intraday_d1/plots/audcad_intraday_d1_" # ZScore plots location
    ,modelPath = "~/Common/audcad_intraday_d1/models/audcad_intraday_d1_"
    ,strategyPath = '~/Common/audcad_intraday_d1/' # Strategy root folder
    ,pair1 = c('AUD', 'CAD') # Currency Pair
    ,initAssets = 1000000 # net cap
    ,leverage = 20 # leverage
    ,reinvest = TRUE # reinvest or not
    ,posiRate = 0.03 # position ratio
    ,threshold = 1 # mean reversion threshold
    ,mean.threshold = 0.49 # mean reversion p value
    ,moment.threshold = 1.25 # trending threshold - effect * direction
    ,stoploss = -0.10 # NOT USING
    ,ACCESS_TOKEN_PRICE = .oandaEnv$ACCESS_TOKEN#'48b9f5f3b3daf6b776d4c5aaaae11bac-981312f63e38b4dc54e33c77caedfafa' # Broker Token
    ,ACCOUNT_TYPE_PRICE = .oandaEnv$ACCOUNT_TYPE#'real' # Broker account type
    ,ACCOUNT_ID = '101-011-4686012-003' # Broker account number
    ,train.length = 200 # Mean reversion lookback
    ,ml.length = 1800 # Machine learning lookback
    ,backtest.n = 1800 # Backtest periods
    ,min.lookback = 21 # Min half life
    ,max.lookback = 63 # Max half life
    ,sd.n = 2 # Standard deviation for SL & TP
    ,freq = 'D' # Trading interval
    ,pred.n = 2 # Machine learning look ahead
    ,exe.pip = 0.0006 # threshold of execution pip difference
    ,volatility.threshold = 0.05 # Volatility multiplier, NOT USING for now
    ,classifier.threshold = 0.5 # Return multiplier
    ,VIX.threshold = 20
)


# Pricing Functions ------------------------------------------------------
# Get Latest Prices
getLatestPrices = function(configs){
    AUDCAD = prepareForexOandaPrices(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, oanda.count = 2, Cur1 = configs$pair1[1], Cur2 = configs$pair1[2], oanda.granularity = configs$freq)
    audcad = Cl(AUDCAD$OA.MID);
    audcad.ret = log10(Cl(AUDCAD$OA.MID)/Op(AUDCAD$OA.MID))
    audcad.spreads = Hi(AUDCAD$OA.MID) - Lo(AUDCAD$OA.MID)
    AUDCAD.bid = AUDCAD$OA.BID
    AUDCAD.ask = AUDCAD$OA.ASK

    cols = c('AUDCAD')
    portf.new = na.omit(audcad.ret); names(portf.new) = cols
    prices.new = na.omit(audcad); names(prices.new) = cols
    cur.spreads = na.omit(audcad.spreads); names(cur.spreads) = cols

    return(list(AUDCAD = tail(AUDCAD$OA.MID, 1),
                portf.new = tail(portf.new,1),
                prices.new = tail(prices.new,1),
                cur.spreads = tail(cur.spreads,1),
                update.date = index(tail(prices.new,1)),
                AUDCAD.bid = tail(AUDCAD.bid,1),
                AUDCAD.ask = tail(AUDCAD.ask,1)))
}
# Get Latest Trade IDs
getLatestTradeID = function(configs){
    return(list(
        audcad.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = c('AUD_CAD'))$trades$id[1]
    ))
}
getLatestTrades = function(configs){
    return(list(
        audcad.trade = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = c('AUD_CAD'))$trades[1,]
    ))
}
getLatestVIX = function(latest = TRUE, n = 2500){
    if(latest){
        vix = tail(Quandl("CBOE/VIX", api_key="H83fxGxUfi-GBx1JyDx8", start_date=Sys.Date()-5, order = 'asc'))
    }else{
        vix = Quandl("CBOE/VIX", api_key="H83fxGxUfi-GBx1JyDx8", order = 'asc')
    }
    vix = OHLC(vix)
    vix.spreads = tail(Hi(vix) - Lo(vix),n)
    vix.change = tail(Cl(vix) - Op(vix), n)
    return(list(vix = tail(Cl(vix), n),vix.spreads=vix.spreads,vix.change=vix.change))
}


####################################################
### OANDA MEAN REVERSION STRATEGY -- Daily BASIS ###
####################################################
inLong = FALSE
inShort = FALSE
tradeType = 'None'

# Sunday approximately 5 p.m. to Friday 5 p.m
# Get prices univers (maximum last 2500 points)
AUDCAD = prepareForexOandaPrices(configs$ACCOUNT_TYPE_PRICE, configs$ACCESS_TOKEN_PRICE,
                                 oanda.count = 2500, Cur1 = configs$pair1[1], Cur2 = configs$pair1[2],
                                 oanda.granularity = configs$freq)
# Caculate some basic price series
audcad = Cl(AUDCAD$OA.MID);
audcad.ret = log10(Cl(AUDCAD$OA.MID)/Op(AUDCAD$OA.MID))
audcad.spreads = Hi(AUDCAD$OA.MID) - Lo(AUDCAD$OA.MID)
audcad.vol = Vo(AUDCAD$OA.MID)
cols = c('AUDCAD')
portf = na.omit(audcad.ret); names(portf) = cols
prices = na.omit(audcad); names(prices) = cols
cur.spreads = na.omit(audcad.spreads); names(cur.spreads) = cols
AUDCAD = AUDCAD$OA.MID
# VIX
vix = getLatestVIX(F, nrow(AUDCAD))

sink(configs$log.file)
sink(configs$log.file, type="message")
while(TRUE){
    tryCatch({
        latestPrices = getLatestPrices(configs)
        lastDate = index(tail(prices, 1))
        # gtdtime = format(lastDate + 300, '%Y-%m-%dT%H:%M:%S.000000Z')

        if(latestPrices$update.date > lastDate){

            latestVIX = getLatestVIX(T, 1)
            cat(paste0('\n\nNew Event Found! Start trading with ', lastDate, '............\n'))
            portf = rbind(portf, latestPrices$portf.new)
            AUDCAD = rbind(AUDCAD, latestPrices$AUDCAD)
            prices = rbind(prices, latestPrices$prices.new)
            cur.spreads = rbind(cur.spreads, latestPrices$cur.spreads)
            if(!tail(vix$vix,1) == tail(latestVIX$vix,1)){
                vix$vix = c(vix$vix, latestVIX$vix)
                vix$vix.spreads = c(vix$vix.spreads, latestVIX$vix.spreads)
                vix$vix.change = c(vix$vix.change, latestVIX$vix.change)
            }
            r = nrow(prices)
            goLong = FALSE
            goShort = FALSE
            closePos = FALSE

            # Model data --------------------------------------------------------------
            tmp.prices = tail(prices, configs$train.length)
            ml.prices = cbind(tail(AUDCAD, configs$ml.length),
                              vix.spreads = tail(vix$vix.spreads, configs$ml.length),
                              vix.change = tail(vix$vix.change, configs$ml.length))
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
            look.back = min(max(c(look.back, configs$min.lookback)), configs$max.lookback)

            # 3. Statistics Test ------------------------------------------------------
            # Stationary Tests
            # ADF
            adf.res = adf.test(spreads, k = 0)
            adf.res.lag1 = adf.test(spreads, k = 1) #price change
            adf.signal=adf.res$p.value
            adf.signal.lag1=adf.res.lag1$p.value
            pp.signal = pp.test(spreads)$p.value # Phillips-Perron Unit Root test
            # Hurst
            # hurst.res = hurstexp(spreads, d = 2, display = T)
            # hurst.signal=hurst.res$Hal
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
            # jc.signal= 0.1 / (jotest@teststat[1] / jotest@cval[1,1])


            # 4. Mean Reversion -------------------------------------------------------
            # hedge ratio NOT REQUIRED any more

            # ZScore Calculation
            spreads.MA <- MaRatio(spreads, look.back)
            spreads.SD <- Sd(spreads, look.back)
            Z.Score <- ZScore(spreads,spreads.MA,spreads.SD)

            # Position Sizing
            zScore=as.numeric(tail(Z.Score,1))
            numUnits=-tail(zScore,1)
            # Dollar neutrality
            sizing = abs(numUnits)/1
            posUnits = configs$initAssets * configs$leverage * configs$posiRate
            dollars = round(as.numeric(posUnits * sizing))
            # positions
            last.price = tail(tmp.prices,1)
            positions = as.data.frame(round(dollars / tail(last.price,1)))
            names(positions) = paste0('POS.AUDCAD')


            # 5. Momentum -------------------------------------------------------------
            mom.crossover = momentum.Crossover(tail(spreads, 100))
            macd.indicator = tail(MACD(spreads, maType = 'EMA'), 1)
            macd = as.numeric(macd.indicator$macd) # macd > signal bearish | macd < signal bullish
            macd.signal = as.numeric(macd.indicator$macd - macd.indicator$signal) # neg bearish | pos bullish
            ema.signal = as.numeric(tail(spreads - EMA(spreads, n = look.back), 1)) # pos bullish | neg bearish
            mom.hamming.dist = as.numeric(tail(mom.crossover$hamming.Dist, 1)) # 1 - 10 weak to strong momentum
            mom.spearman = as.numeric(tail(mom.crossover$spearman, 1))  # high means short and low means long (-1 to 1)
            mom.thickness = as.numeric(tail(mom.crossover$thickness, 1)) / mean(mom.crossover$thickness) # higher thickness means higher volatilities or stronger momentum
            mom.rsi = as.numeric(tail(momentum.RSI(spreads), 1)) # >70 over buy | <70 over sell
            # Directional
            macd = ifelse(macd > macd.signal, -1, ifelse(macd < macd.signal, 1, 0))
            macd.signal = ifelse(macd.signal < 0, -1, ifelse(macd.signal > 0, 1, 0))
            ema.signal = ifelse(ema.signal < 0, -1, ifelse(ema.signal > 0, 1, 0))
            mom.zScore = ifelse(zScore <= -2, -1, ifelse(zScore >= 2, 1, 0)) # > 2 | < -2 momentum
            mom.spearman = ifelse(mom.spearman > 0, -1, ifelse(mom.spearman < 0, 1, 0))
            mom.rsi = ifelse(mom.rsi >= 70, 1, ifelse(mom.rsi <= -70, -1, 0))
            mom.direction = mean(c(macd, macd.signal, ema.signal, mom.zScore, mom.spearman, mom.rsi)) # 0.6666667
            # mom.direction = sign(tail(diff(Z.Score),1))
            # Strongness
            mom.effect = mean(c(mom.hamming.dist/5, mom.thickness, hurst.signal/0.5)) # 1.1


            # 6. Machine learning -----------------------------------------------------
            if(!exists("xgbFitClass")){
                oa.ret = lag(ROC(Cl(ml.prices), n = configs$pred.n, type = 'discrete'), -configs$pred.n); names(oa.ret) = 'target'
                oa.ret = ifelse(oa.ret >= 0, 1,0)
                price.feat =  prepareMachinelearningFeatures(ml.prices)
                xgbFitClass1 = xgbBackTest(na.omit(merge(price.feat[1:800,], oa.ret)))
                xgbFitClass2 = xgbBackTest(na.omit(merge(price.feat[801:nrow(price.feat),], oa.ret)))
                price.feat$lastPred = c(predict(xgbFitClass1$xgbFit, data.matrix(price.feat[801:nrow(price.feat),])),
                                        predict(xgbFitClass2$xgbFit, data.matrix(price.feat[1:800,])))
                xgbFitClass = xgbBackTest(na.omit(merge(price.feat, oa.ret)))
                save(xgbFitClass1,xgbFitClass2,xgbFitClass, file = paste0(configs$modelPath,format(index(tail(tmp.prices,1)), "%Y%m%d%H%M"),".RData"))
            }
            price.feat =  tail(prepareMachinelearningFeatures(tail(ml.prices, 201)),1)
            price.feat$lastPred = (predict(xgbFitClass1$xgbFit, data.matrix(price.feat)) + predict(xgbFitClass2$xgbFit, data.matrix(price.feat)))/2
            aud.ml.ret = predict(xgbFitClass$xgbFit, data.matrix(price.feat))
            aud.ret.multiplier = sign(aud.ml.ret - configs$classifier.threshold) *
                (abs(aud.ml.ret - configs$classifier.threshold) + configs$classifier.threshold)/configs$classifier.threshold
            # aud.ml.sd = predict(audusdVolModel$xgbFitReg, data.matrix(price.feat.aud))
            # aud.sd.multiplier = configs$volatility.threshold / aud.ml.sd # low -> more | high -> less


            # 7. Risk Management ------------------------------------------------------
            AUD.sd = configs$sd.n * sd(tail(cur.spreads, look.back)) # mean(tail(cur.spreads, look.back)) +


            # 8. Mean-reversion & Momentum signals
            mean.reversion.signal = median(c(adf.signal, pp.signal, var.ratio))
            momentum.signal = as.numeric(mom.direction * mom.effect)


            # 8. Execution ------------------------------------------------------------
            # 8.1 Mean-reverting signals
            if(mean.reversion.signal <= configs$mean.threshold & abs(zScore) >= configs$threshold){
                if(zScore >= configs$threshold & aud.ml.ret <= configs$classifier.threshold & abs(tail(vix$vix,1)) <= configs$VIX.threshold){ # Added VIX
                    goLong = FALSE
                    goShort = TRUE
                    closePos = FALSE
                }else if(zScore <= -configs$threshold & aud.ml.ret >= configs$classifier.threshold & abs(tail(vix$vix,1)) <= configs$VIX.threshold){ # Added VIX
                    goLong = TRUE
                    goShort = FALSE
                    closePos = FALSE
                }else if((inLong & zScore>configs$threshold) | (inShort & zScore < -configs$threshold)){
                    goLong = FALSE
                    goShort = FALSE
                    closePos = TRUE # sell OR buy
                }
                tradeType = 'MeanReversion'
                # }else if(abs(momentum.signal) >= configs$moment.threshold){
                #   if(momentum.signal < 0){
                #     goLong = FALSE
                #     goShort = TRUE # buy
                #     closePos = FALSE
                #   }else if(momentum.signal > 0){
                #     goLong = TRUE # sell
                #     goShort = FALSE
                #     closePos = FALSE
                #   }else{
                #     goLong = FALSE
                #     goShort = FALSE
                #     closePos = TRUE # sell OR buy
                #   }
                #   tradeType = 'Momentum'
                # }else if(abs(momentum.signal) < configs$moment.threshold & tradeType == 'Momentum'){
                #   goLong = FALSE
                #   goShort = FALSE
                #   closePos = TRUE # sell OR buy
                #   tradeType = 'CloseMomentum'
            }else{
                tradeType = 'None'
            }


            # 9. Positions & Risk Management --------------------------------------------------
            POS.AUDUSD = 0
            aud.take.profit = 0
            aud.stop.loss = 0
            AUDUSD.exe = as.numeric(last.price)
            current.price = getLatestPrices(configs)
            bid.price = as.numeric(Cl(current.price$AUDCAD.bid))
            ask.price = as.numeric(Cl(current.price$AUDCAD.ask))

            if(goLong & (bid.price - AUDUSD.exe <= configs$exe.pip)){
                POS.AUDUSD = round(as.numeric(positions) * (aud.ret.multiplier^6))

                aud.take.profit = round(as.numeric(bid.price + sign(POS.AUDUSD) * AUD.sd), 5)
                aud.stop.loss = round(as.numeric(bid.price - sign(POS.AUDUSD) * AUD.sd), 5)

                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_CAD', UNITS = POS.AUDUSD,
                                                 ORDERTYPE = 'MARKET', PRICE = bid.price,
                                                 TAKEPROFIT = aud.take.profit,
                                                 # STOPLOSS = aud.stop.loss,
                                                 TRAILINGSTOP = AUD.sd,
                                                 TIMEINFORCE = "IOC")

                cat(paste0("\nGo Long | AUD Limit order at ", bid.price, " | Take Profit: ", aud.take.profit, " | Trailing Stop Loss: ", aud.stop.loss, " | Positions: ", POS.AUDUSD))

                inLong = TRUE
                inShort = FALSE

            }else if(goShort & (AUDUSD.exe - ask.price <= configs$exe.pip)){
                POS.AUDUSD = -round(as.numeric(positions) * (aud.ret.multiplier^6))

                aud.take.profit = round(as.numeric(ask.price + sign(POS.AUDUSD) * AUD.sd), 5)
                aud.stop.loss = round(as.numeric(ask.price - sign(POS.AUDUSD) * AUD.sd), 5)

                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_CAD', UNITS = POS.AUDUSD,
                                                 ORDERTYPE = 'MARKET', PRICE = ask.price,
                                                 TAKEPROFIT = aud.take.profit,
                                                 # STOPLOSS = aud.stop.loss,
                                                 TRAILINGSTOP = AUD.sd,
                                                 TIMEINFORCE = "IOC")

                cat(paste0("\nGo Short | AUD Limit order at ", ask.price, " | Take Profit: ", aud.take.profit, " | Trailing Stop Loss: ", aud.stop.loss, " | Positions: ", POS.AUDUSD))

                inLong = FALSE
                inShort = TRUE


            }else if(closePos & (inLong | inShort)){
                CurrentPositions = setDT(getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID)$positions)

                POS.AUDUSD = -sum(as.numeric(CurrentPositions[instrument == 'AUD_CAD', .(long.units,short.units)]))

                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_CAD', UNITS = POS.AUDUSD, TIMEINFORCE = "IOC")

                inLong = FALSE
                inShort = FALSE
            }
            cat(paste0('\nZScore: ', round(zScore, 2),
                       ' | Machine learning: ', round(aud.ml.ret, 5),
                       ' | Mean-reversion: ', round(mean.reversion.signal,2),
                       ' | Momentum: ', round(momentum.signal,2), '\n'))

            # Re-check positions
            CurrentPositions = setDT(getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID)$positions)
            POS.AUDCAD = sum(as.numeric(CurrentPositions[instrument == 'AUD_CAD', .(long.units,short.units)]))

            latest.aud.price = NA
            latest.aud.openTime = NA
            if(goShort | goLong){
                # latestTradeID = getLatestTradeID(configs)
                latestTrade = getLatestTrades(configs)
                latest.aud.price = as.numeric(latestTrade$audcad.trade$price)
                latest.aud.openTime = latestTrade$audcad.trade$openTime
            }


            # 10. Order book --------------------------------------------------------------
            # Update Portf Information
            OB.PRICES = last.price;
            unrealizedPnL = sum(as.numeric(CurrentPositions$unrealizedPL))
            PnL = sum(as.numeric(CurrentPositions$pl))
            # Create latest order record

            tot.pos = setDT(getOandaPositions(configs$ACCOUNT_TYPE, configs$ACCESS_TOKEN, configs$ACCOUNT_ID)$positions)
            long.pos = sum(as.numeric(tot.pos[instrument == 'AUD_CAD', .(long.units)]))
            short.pos = sum(as.numeric(tot.pos[instrument == 'AUD_CAD', .(short.units)]))
            long.exposure = long.pos * mid.price
            short.exposure = short.pos * mid.price
            orderBookFin = data.frame(as.character(index(OB.PRICES)),
                                      Sys.time(),
                                      bid.price,
                                      ask.price,
                                      as.numeric(Cl(current.price$AUDCAD)), # mid.price
                                      tradeType,
                                      latest.aud.price, # not right
                                      latest.aud.price, # not right
                                      POS.AUDUSD,
                                      POS.AUDUSD,
                                      long.exposure,
                                      short.exposure,
                                      ifelse(goLong, AUD.sd, 0), # longSL
                                      ifelse(goShort, AUD.sd, 0), # shortSL
                                      ifelse(goLong, aud.take.profit, 0), # longTgt
                                      ifelse(goShort, aud.take.profit, 0), # shortTgt
                                      unrealizedPnL,
                                      PnL,
                                      (abs(long.exposure) + abs(short.exposure)) / (configs$initAssets * configs$leverage), # percExposure
                                      PnL,
                                      0)
            names(orderBookFin) = c('Date', 'Exec Time', 'Bid Price', 'Ask Price', 'Mid Price', 'Order Type', 'Long Price', 'Short Price',
                                    'Long Qty', 'Short Qty', 'Long Exposure', 'Short Exposure', 'Long SL', 'Short SL', 'Long TGT', 'Short TGT',
                                    'P&L Booked Price', 'Profit & Loss', 'Exposure % of Portf', 'Total P&L', 'VAR')
            setDT(orderBookFin)
            if(file.exists(configs$order.book.file.fin)){
                Order.Book = fread(configs$order.book.file.fin)
                Order.Book[, names(orderBookFin)[!names(orderBookFin)%in%names(Order.Book)] := 0]
                Order.Book = Order.Book[, names(orderBookFin), with = F]
                Order.Book = rbind(Order.Book, orderBookFin)
                write.csv(Order.Book, file = configs$order.book.file.fin, row.names = FALSE)
            }else{
                write.csv(orderBookFin, file = configs$order.book.file.fin, row.names = FALSE)
            }


            orderBook = data.frame(as.character(index(OB.PRICES)), OB.PRICES, adf.signal, adf.signal.lag1, pp.signal,
                                   var.ratio, hurst.signal, pvmr.par, half.life.par, mean.reversion.signal,
                                   macd, macd.signal, ema.signal, mom.hamming.dist, mom.spearman, mom.thickness, mom.rsi, momentum.signal,
                                   zScore,
                                   aud.ml.ret, aud.ret.multiplier,
                                   # aud.ml.sd, aud.sd.multiplier,
                                   POS.AUDUSD,
                                   latest.aud.price,latest.aud.openTime,
                                   aud.take.profit, aud.stop.loss,
                                   inLong, inShort, goLong, goShort, closePos, tradeType, unrealizedPnL, PnL)
            names(orderBook) = c('DateTime', paste0('P.', names(OB.PRICES)), 'ADF', 'ADF.lag1', 'Phillips-Perron',
                                 'Variance.Ratio', 'Hurst', 'Partial.Auto.pvmr','Partial.Auto.HL', 'Mean-Reversion',
                                 'MACD','MACD.Signal', 'EMA.Signal', 'Hamming.Dist', 'Mom.Spearman', 'Mom.Thickness', 'RSI', 'Momentum',
                                 'ZScore',
                                 'AUD.ML.Returns', 'AUD.ML.Ret.Multiplier',
                                 # 'AUD.ML.Volatility', 'AUD.ML.SD.Multiplier',
                                 'POS.AUDUSD',
                                 'AUD.Exec.Price','AUD.Order.Time',
                                 'AUD.TakeProfit', 'AUD.StopLoss',
                                 'inLong', 'inShort', 'goLong', 'goShort', 'closePos', 'tradeType', 'unrealizedPnL', 'PnL')
            setDT(orderBook)

            if(file.exists(configs$order.book.file)){
                Order.Book = fread(configs$order.book.file)
                Order.Book[, names(orderBook)[!names(orderBook)%in%names(Order.Book)] := 0]
                Order.Book = Order.Book[, names(orderBook), with = F]
                Order.Book = rbind(Order.Book, orderBook)
                write.csv(Order.Book, file = configs$order.book.file, row.names = FALSE)
            }else{
                write.csv(orderBook, file = configs$order.book.file, row.names = FALSE)
            }

            # Save ZScore Plots -------------------------------------------------------
            ZScore.plot = tail(Z.Score,configs$train.length-look.back)
            lob <- shb <- clb <- xts(!as.logical(ZScore.plot[,1]),index(ZScore.plot))
            lob[as.Date(index(lob)) %in% as.Date(Order.Book[goLong==T,DateTime])] <- TRUE
            shb[as.Date(index(shb)) %in% as.Date(Order.Book[goShort==T,DateTime])] <- TRUE
            clb[as.Date(index(clb)) %in% as.Date(Order.Book[closePos==T,DateTime])] <- TRUE

            png(paste0(configs$plotPath,format(index(tail(tmp.prices,1)), "%Y%m%d%H%M"),".png"))
            print({
                chart_Series(ZScore.plot, theme = chart_theme(), name = "ZScore Chart - long(green)/short(red)/close(grey)")
                add_TA(lob, on = 1, col = "green", lty = 3)
                add_TA(shb, on = 1, col = "red", lty = 3)
                add_TA(clb, on = 1, col = "grey", lty = 3)
            })
            dev.off()

            # Train the machine learning model ----------------------------------------
            oa.ret = lag(ROC(Cl(ml.prices), n = configs$pred.n, type = 'discrete'), -configs$pred.n); names(oa.ret) = 'target'
            oa.ret = ifelse(oa.ret >= 0, 1,0)
            price.feat =  prepareMachinelearningFeatures(ml.prices)
            xgbFitClass1 = xgbBackTest(na.omit(merge(price.feat[1:800,], oa.ret)))
            xgbFitClass2 = xgbBackTest(na.omit(merge(price.feat[801:nrow(price.feat),], oa.ret)))
            price.feat$lastPred = c(predict(xgbFitClass1$xgbFit, data.matrix(price.feat[801:nrow(price.feat),])),
                                    predict(xgbFitClass2$xgbFit, data.matrix(price.feat[1:800,])))
            xgbFitClass = xgbBackTest(na.omit(merge(price.feat, oa.ret)))
            save(xgbFitClass1,xgbFitClass2,xgbFitClass, file = paste0(configs$modelPath,format(index(tail(tmp.prices,1)), "%Y%m%d%H%M"),".RData"))


            updateDirFilePermissions(configs$strategyPath)
        }
    },
    error = function(e){print(e)}
    )

    Sys.sleep(1)
}
sink(type="message")
sink()

