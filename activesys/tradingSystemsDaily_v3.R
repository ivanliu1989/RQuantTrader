rm(list=ls());gc()
loadQuantPackages()
library(xgboost)
Sys.setenv(TZ='GMT')

xgbBackTest = function(train){
    trainBC = as.data.frame(train[1:round(nrow(train)*4/5),])
    testBC = as.data.frame(train[round(nrow(train)*4/5):nrow(train),])
    predictors = names(trainBC)[!names(trainBC) %in% c('target')]
    response = 'target'
    dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
    dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
    watchlist <- list(train = dtrain, eval = dtest)
    param <- list(
        max_depth = 6,
        eta = 0.01,
        nthread = 6,
        objective = "binary:logistic",
        eval_metric = "rmse",
        booster = "gbtree",
        gamma = 0.0001,
        min_child_weight = 1,
        subsample = 1,
        colsample_bytree = 0.06
    )
    for(i in 1:15){
        xgbFitClass <- xgb.train(param,dtrain,nrounds = 1000,watchlist,
                                 early_stopping_rounds = 100,print_every_n = 1000)
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
# Required Functions ------------------------------------------------------
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

configs = list(
    order.book.file = '~/Common/audcad_intraday_d1/orderbook/ivanliu_AUDCAD_Pairs_d1.csv'
    ,log.file = file(paste0('~/Common/audcad_intraday_d1/log/ivanliu_AUDCAD_Pairs_d1_', as.Date(Sys.time()), '.Rout'), open = 'wt')
    ,plotPath = "~/Common/audcad_intraday_m15/plots/audcad_intraday_d1_"
    ,strategyPath = '~/Common/audcad_intraday_d1/'
    ,pair1 = c('AUD', 'CAD') # Later use
    ,initAssets = 1000000
    ,posiRate = 0.1
    ,threshold = 1
    ,mean.threshold = 0.49
    ,moment.threshold = 1.25 # effect * direction
    ,stoploss = -0.10
    ,ACCESS_TOKEN_PRICE = ' '
    ,ACCOUNT_TYPE_PRICE = 'real'
    ,ACCOUNT_ID = '101-011-4686012-003'
    ,train.length = 252
    ,ml.length = 1800
    ,backtest.n = 1800
    ,min.lookback = 21
    ,max.lookback = 63
    ,sd.n = 3
    ,freq = 'D'
    ,pred.n = 6
    ,AUD.pip = 0.00012
    ,CAD.pip = 0.0002
    ,volatility.threshold = 0.05
    ,classifier.threshold = 0.5
)
# load('~/Common/models/audcad_1d_models/audcad_1d_models.RData')


# Get Latest Prices
getLatestPrices = function(configs){
    AUDCAD = prepareForexOandaPrices(configs$ACCOUNT_TYPE_PRICE, configs$ACCESS_TOKEN_PRICE, oanda.count = 2, Cur1 = configs$pair1[1], Cur2 = configs$pair1[2], oanda.granularity = configs$freq)
    # if(configs$freq == 'D'){index(AUDCAD$OA.MID) = as.Date(index(AUDCAD$OA.MID))}

    audcad = Cl(AUDCAD$OA.MID);
    audcad.ret = log10(Cl(AUDCAD$OA.MID)/Op(AUDCAD$OA.MID))
    audcad.spreads = Hi(AUDCAD$OA.MID) - Lo(AUDCAD$OA.MID)

    cols = c('AUDCAD')
    portf.new = na.omit(audcad.ret); names(portf.new) = cols
    prices.new = na.omit(audcad); names(prices.new) = cols
    cur.spreads = na.omit(audcad.spreads); names(cur.spreads) = cols

    return(list(AUDCAD = tail(AUDCAD$OA.MID, 1),
                portf.new = tail(portf.new,1),
                prices.new = tail(prices.new,1),
                cur.spreads = tail(cur.spreads,1),
                update.date = index(tail(prices.new,1))))
}
# Get Latest Trade IDs
getLatestTradeID = function(configs){
    return(list(
        audcad.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = c('AUD_CAD'))$trades$id[1],
    ))
}
getLatestTrades = function(configs){
    return(list(
        audcad.trade = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = c('AUD_CAD'))$trades[1,]
    ))
}

######################################################
### OANDA MEAN REVERSION PORTFOLIO -- Hourly BASIS ###
######################################################
inLong = FALSE
inShort = FALSE
tradeType = 'None'

# Sunday approximately 5 p.m. to Friday 5 p.m
AUDCAD = prepareForexOandaPrices(configs$ACCOUNT_TYPE_PRICE, configs$ACCESS_TOKEN_PRICE,
                                 oanda.count = 2500, Cur1 = configs$pair1[1], Cur2 = configs$pair1[2],
                                 oanda.granularity = configs$freq)
# if(configs$freq == 'D'){index(AUDCAD$OA.MID) = as.Date(index(AUDCAD$OA.MID))}

audcad = Cl(AUDCAD$OA.MID);
audcad.ret = log10(Cl(AUDCAD$OA.MID)/Op(AUDCAD$OA.MID))
audcad.spreads = Hi(AUDCAD$OA.MID) - Lo(AUDCAD$OA.MID)

cols = c('AUDCAD')
portf = na.omit(audcad.ret); names(portf) = cols
prices = na.omit(audcad); names(prices) = cols
cur.spreads = na.omit(audcad.spreads); names(cur.spreads) = cols

AUDCAD = AUDCAD$OA.MID

sink(configs$log.file)
sink(configs$log.file, type="message")
while(TRUE){
    tryCatch({
        latestPrices = getLatestPrices(configs)
        lastDate = index(tail(prices, 1))
        # gtdtime = format(lastDate + 300, '%Y-%m-%dT%H:%M:%S.000000Z')

        if(latestPrices$update.date > lastDate){

            cat(paste0('\n\nNew Event Found! Start trading with ', lastDate, '............\n'))
            portf = rbind(portf, latestPrices$portf.new)
            AUDCAD = rbind(AUDCAD, latestPrices$AUDCAD)
            prices = rbind(prices, latestPrices$prices.new)
            cur.spreads = rbind(cur.spreads, latestPrices$cur.spreads)
            r = nrow(prices)
            goLong = FALSE
            goShort = FALSE
            closePos = FALSE

            # Model data --------------------------------------------------------------
            tmp.prices = tail(prices, configs$train.length)
            ml.prices = tail(AUDCAD, configs$ml.length)
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
            jc.signal= 0.1 / (jotest@teststat[1] / jotest@cval[1,1])


            # 4. Mean Reversion -------------------------------------------------------
            # hedge ratio
            spreads.MA <- MaRatio(spreads, look.back)
            spreads.SD <- Sd(spreads, look.back)
            Z.Score <- ZScore(spreads,spreads.MA,spreads.SD)

            # Position Sizing
            zScore=as.numeric(tail(Z.Score,1))
            numUnits=-tail(zScore,1)
            # Dollar neutrality
            sizing = abs(numUnits)/1
            posUnits = configs$initAssets * configs$posiRate
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
            oa.ret = lag(ROC(Cl(ml.prices), n = configs$pred.n, type = 'discrete'), -configs$pred.n); names(oa.ret) = 'target'
            oa.ret = ifelse(oa.ret >= 0, 1,0)
            price.feat =  prepareMachinelearningFeatures(ml.prices)
            xgbFitClass = xgbBackTest(na.omit(merge(price.feat, oa.ret)))

            # price.feat.aud =  tail(prepareMachinelearningFeatures(tail(ml.price.oa, 201)), 1)
            aud.ml.ret = predict(xgbFitClass, data.matrix(tail(price.feat,1)))
            # aud.ml.sd = predict(audusdVolModel$xgbFitReg, data.matrix(price.feat.aud))
            aud.ret.multiplier = sign(aud.ml.ret - configs$classifier.threshold) *
                (abs(aud.ml.ret - configs$classifier.threshold) + configs$classifier.threshold)/configs$classifier.threshold
            # aud.sd.multiplier = configs$volatility.threshold / aud.ml.sd # low -> more | high -> less


            # 7. Risk Management ------------------------------------------------------
            AUD.sd = max(tail(cur.spreads, look.back)) + configs$sd.n * sd(tail(cur.spreads, look.back))


            # 8. Mean-reversion & Momentum signals
            mean.reversion.signal = median(c(adf.signal, pp.signal, var.ratio))
            momentum.signal = as.numeric(mom.direction * mom.effect)


            # 8. Execution ------------------------------------------------------------
            # 8.1 Mean-reverting signals
            if(mean.reversion.signal <= configs$mean.threshold & abs(zScore) >= configs$threshold){
                if(zScore >= configs$threshold & aud.ml.ret <= configs$classifier.threshold){
                    goLong = FALSE
                    goShort = TRUE # buy
                    closePos = FALSE
                }else if(zScore <= -configs$threshold & aud.ml.ret >= configs$classifier.threshold){
                    goLong = TRUE # sell
                    goShort = FALSE
                    closePos = FALSE
                }else if((inLong & zScore>configs$threshold) | (inShort & zScore < -configs$threshold)){
                    # | unrealizedPnL <= stoploss
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


            # Positions & Risk Management --------------------------------------------------
            POS.USDCAD = 0
            aud.take.profit = 0
            aud.stop.loss = 0
            AUDUSD.exe = as.numeric(last.price)

            if(goLong){
                POS.AUDUSD = round(as.numeric(positions) * (aud.ret.multiplier^6))

                aud.take.profit = round(as.numeric(AUDUSD.exe + sign(POS.AUDUSD) * AUD.sd), 5)
                aud.stop.loss = round(as.numeric(AUDUSD.exe - sign(POS.AUDUSD) * AUD.sd), 5)
                # AUDUSD.exe = round(as.numeric(AUDUSD.exe + sign(POS.AUDUSD) * configs$AUD.pip), 5)
                # POS.AUDUSD = round(POS.AUDUSD * abs(aud.ret.multiplier)^2 * abs(cad.ret.multiplier)^2 * mean(c(aud.sd.multiplier, cad.sd.multiplier)))
                # if(sign(POS.AUDUSD) == sign(aud.ret.multiplier)){
                #   POS.AUDUSD = POS.AUDUSD * 2
                #   POS.USDCAD = POS.USDCAD * 2
                # }

                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_CAD', UNITS = POS.AUDUSD,
                                                 ORDERTYPE = 'MARKET', PRICE = AUDUSD.exe, STOPLOSS  = aud.stop.loss, TIMEINFORCE = "IOC")

                cat(paste0("\nGo Long | AUD Limit order at ", AUDUSD.exe, " | Take Profit: ", aud.take.profit, " | Trailing Stop Loss: ", aud.stop.loss, " | Positions: ", POS.AUDUSD))

                inLong = TRUE
                inShort = FALSE

            }else if(goShort){
                POS.AUDUSD = -round(as.numeric(positions) * (aud.ret.multiplier^6))

                aud.take.profit = round(as.numeric(AUDUSD.exe + sign(POS.AUDUSD) * AUD.sd), 5)
                aud.stop.loss = round(as.numeric(AUDUSD.exe - sign(POS.AUDUSD) * AUD.sd), 5)
                # AUDUSD.exe = round(as.numeric(AUDUSD.exe + sign(POS.AUDUSD) * configs$AUD.pip), 5)
                # POS.AUDUSD = round(POS.AUDUSD * abs(aud.ret.multiplier)^2 * abs(cad.ret.multiplier)^2 * mean(c(aud.sd.multiplier, cad.sd.multiplier)))
                # if(sign(POS.AUDUSD) == sign(aud.ret.multiplier)){
                #   POS.AUDUSD = POS.AUDUSD * 2
                #   POS.USDCAD = POS.USDCAD * 2
                # }

                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_CAD', UNITS = POS.AUDUSD,
                                                 ORDERTYPE = 'MARKET', PRICE = AUDUSD.exe, STOPLOSS = aud.stop.loss, TIMEINFORCE = "IOC")

                cat(paste0("\nGo Short | AUD Limit order at ", AUDUSD.exe, " | Take Profit: ", aud.take.profit, " | Trailing Stop Loss: ", aud.stop.loss, " | Positions: ", POS.AUDUSD))

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
            cat(paste0('\nZScore: ', round(zScore, 2), ' | Mean-reversion: ', round(mean.reversion.signal,2),' | Momentum: ', round(momentum.signal,2), '\n'))

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


            # Order book --------------------------------------------------------------
            # Update Portf Information
            OB.PRICES = last.price;
            unrealizedPnL = sum(as.numeric(CurrentPositions$unrealizedPL))
            PnL = sum(as.numeric(CurrentPositions$pl))
            # Create latest order record
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

            png(paste0(configs$plotPath,format(index(tail(tmp.prices,1)), "%Y%m%d%H%M"),".png"))
            par(mfcol = c(3,1))
            print(chart_Series(tail(zscores(spreads),252)))
            print(chart_Series(tail(Z.Score,252)))
            print(chart_Series(tail(zscores.ma(spreads, look.back, round(look.back/6)),252)))
            # print(chart_Series(tail(zscores.ma(spreads, 21, 8),252)))
            dev.off()


            updateDirFilePermissions(configs$strategyPath)
        }
    },
    error = function(e){print(e)}
    )

    Sys.sleep(1)
}
sink(type="message")
sink()

