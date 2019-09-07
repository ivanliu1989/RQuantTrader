rm(list=ls());
configs = list(
    order.book.file = '~/Common/audcad_intraday_m15/orderbook/ivanliu_AUDCAD_Pairs_M15_004.csv'
    ,log.file = file(paste0('~/Common/audcad_intraday_m15/log/ivanliu_AUDCAD_Pairs_M15_004_', as.Date(Sys.time()), '.Rout'), open = 'wt')
    ,plotPath = "~/Common/audcad_intraday_m15/plots/audcad_intraday_m15_"
    ,strategyPath = '~/Common/audcad_intraday_m15/'
    ,pair1 = c('AUD', 'USD') # Later use
    ,pair2 = c('USD', 'CAD') # Later use
    ,initAssets = 1000000
    ,posiRate = 0.1
    ,threshold = 2
    ,mean.threshold = 0.49
    ,moment.threshold = 1.25 # effect * direction
    ,stoploss = -0.10
    ,ACCOUNT_ID = '101-011-4686012-003'
    ,train.length = 480
    ,min.lookback = 48
    ,max.lookback = 240
    ,sd.n = 3
    ,freq = 'M15'
    ,AUD.pip = 0.00012
    ,CAD.pip = 0.0002
    ,volatility.threshold = 0.05
    ,classifier.threshold = 0.5
)
load('~/Common/models/audcad_15m_models/audcad_15m_models.RData')


######################################################
### OANDA MEAN REVERSION PORTFOLIO -- Hourly BASIS ###
######################################################
gc()
loadQuantPackages()
Sys.setenv(TZ='GMT')

inLong = FALSE
inShort = FALSE
tradeType = 'None'

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
# Get Latest Prices
getLatestPrices = function(configs){
    AUDUSD = prepareForexOandaPrices(oanda.count = 2, Cur1 = configs$pair1[1], Cur2 = configs$pair1[2], oanda.granularity = configs$freq)
    USDCAD = prepareForexOandaPrices(oanda.count = 2, Cur1 = configs$pair2[1], Cur2 = configs$pair2[2], oanda.granularity = configs$freq)

    aud = Cl(AUDUSD$OA.MID); if(configs$freq == 'D'){index(aud) = as.Date(index(aud))}
    cad = Cl(1/USDCAD$OA.MID); if(configs$freq == 'D'){index(cad) = as.Date(index(cad))}

    aud.ret = log10(Cl(AUDUSD$OA.MID)/Op(AUDUSD$OA.MID))
    cad.ret = log10(Cl(1/USDCAD$OA.MID)/Op(1/USDCAD$OA.MID))

    aud.spreads = Hi(AUDUSD$OA.MID) - Lo(AUDUSD$OA.MID)
    cad.spreads = Hi(USDCAD$OA.MID) - Lo(USDCAD$OA.MID)

    cols = c('AUDUSD', 'USDCAD')
    portf.new = na.omit(merge(aud.ret, cad.ret)); names(portf.new) = cols
    prices.new = na.omit(merge(aud, cad)); names(prices.new) = cols
    cur.spreads = na.omit(merge(aud.spreads, cad.spreads)); names(cur.spreads) = cols

    return(list(AUDUSD = tail(AUDUSD$OA.MID, 1),
                USDCAD = tail(USDCAD$OA.MID, 1),
                portf.new = tail(portf.new,1),
                prices.new = tail(prices.new,1),
                cur.spreads = tail(cur.spreads,1),
                update.date = index(tail(prices.new,1))))
}
# Get Latest Trade IDs
getLatestTradeID = function(configs){
    return(list(
        aud.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = c('AUD_USD'))$trades$id[1],
        cad.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = c('USD_CAD'))$trades$id[1]
    ))
}
getLatestTrades = function(configs){
    return(list(
        aud.trade = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = c('AUD_USD'))$trades[1,],
        cad.trade = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = c('USD_CAD'))$trades[1,]
    ))
}


# Sunday approximately 5 p.m. to Friday 5 p.m
AUDUSD = prepareForexOandaPrices(oanda.count = 2500, Cur1 = "AUD", Cur2 = "USD", oanda.granularity = configs$freq)
USDCAD = prepareForexOandaPrices(oanda.count = 2500, Cur1 = "USD", Cur2 = "CAD", oanda.granularity = configs$freq)

aud = Cl(AUDUSD$OA.MID); if(configs$freq == 'D'){index(aud) = as.Date(index(aud))}
cad = Cl(1/USDCAD$OA.MID); if(configs$freq == 'D'){index(cad) = as.Date(index(cad))}

aud.ret = log10(Cl(AUDUSD$OA.MID)/Op(AUDUSD$OA.MID))
cad.ret = log10(Cl(1/USDCAD$OA.MID)/Op(1/USDCAD$OA.MID))

aud.spreads = Hi(AUDUSD$OA.MID) - Lo(AUDUSD$OA.MID)
cad.spreads = Hi(USDCAD$OA.MID) - Lo(USDCAD$OA.MID)

cols = c('AUDUSD', 'USDCAD')
portf = na.omit(merge(aud.ret, cad.ret)); names(portf) = cols
prices = na.omit(merge(aud, cad)); names(prices) = cols
cur.spreads = na.omit(merge(aud.spreads, cad.spreads)); names(cur.spreads) = cols

AUDUSD = AUDUSD$OA.MID
USDCAD = USDCAD$OA.MID

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
            AUDUSD = rbind(AUDUSD, latestPrices$AUDUSD)
            USDCAD = rbind(USDCAD, latestPrices$USDCAD)
            prices = rbind(prices, latestPrices$prices.new)
            cur.spreads = rbind(cur.spreads, latestPrices$cur.spreads)
            r = nrow(prices)
            goLong = FALSE
            goShort = FALSE
            closePos = FALSE

            # Model data --------------------------------------------------------------
            tmp.prices = tail(prices[, cols], configs$train.length)
            ols.fit = lm(AUDUSD~. , log(tmp.prices))
            ols.r2 = summary(ols.fit)$r.squared
            ols.pvalues = summary(ols.fit)[[4]][,4]

            fit.hedge = as.numeric(c(1,-ols.fit$coefficients[2]))
            spreads = tmp.prices %*% fit.hedge
            spreads = as.xts(spreads, order.by = index(tmp.prices))

            adf.lm = adf.test(spreads, k = 1) # Augmented Dickey-Fuller test
            pp.lm = pp.test(spreads) # Phillips-Perron Unit Root test
            po.lm = po.test(tmp.prices * fit.hedge) # Phillips-Ouliaris Cointegration test


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


            # Correlation--------------------------------------------------------------
            cor.long = as.numeric(tail(runCor(x = tmp.prices[, 1], y = tmp.prices[, 2], n=configs$train.length), 1))
            cor.short = as.numeric(tail(runCor(x = tmp.prices[, 1], y = tmp.prices[, 2], n=look.back), 1))


            # 3. Statistics Test ------------------------------------------------------
            # stats tests
            # https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R
            jotest=ca.jo(log(tmp.prices), type="trace", K=2, ecdet="none", spec="longrun")
            # jotest=ca.jo(log(tmp.prices), type="trace", K=2, ecdet="const", spec="longrun")
            jc.res = summary(jotest)


            # Spreads
            hedgeRatio = as.numeric(jc.res@V[, 1])
            spreads = tmp.prices %*% hedgeRatio
            spreads = na.omit(as.xts(spreads, order.by = index(tmp.prices)))
            colnames(spreads) = 'Spreads'

            # Stationary Tests
            # ADF
            adf.res = adf.test(spreads, k = 0)
            adf.res.lag1 = adf.test(spreads, k = 1) #price change
            adf.signal=adf.res$p.value
            adf.signal.lag1=adf.res.lag1$p.value
            pp.signal = pp.test(spreads)$p.value # Phillips-Perron Unit Root test
            po.signal = po.test(tmp.prices * hedgeRatio)$p.value # Phillips-Ouliaris Cointegration test
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
            dollar.neut = hedgeRatio * dollars/abs(sum(hedgeRatio))
            # positions
            last.price = tail(tmp.prices,1)
            positions = as.data.frame(round(dollar.neut / tail(last.price,1)))
            names(positions) = paste0('POS.', cols)


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
            price.feat.aud =  tail(prepareMachinelearningFeatures(tail(AUDUSD, 201)), 1)
            aud.ml.ret = predict(audusdRetModel$xgbFitClass, data.matrix(price.feat.aud))
            aud.ml.sd = predict(audusdVolModel$xgbFitReg, data.matrix(price.feat.aud))

            price.feat.cad =  tail(prepareMachinelearningFeatures(tail(USDCAD, 201)), 1)
            cad.ml.ret = predict(usdcadRetModel$xgbFitClass, data.matrix(price.feat.cad))
            cad.ml.sd = predict(usdcadVolModel$xgbFitReg, data.matrix(price.feat.cad))

            aud.sd.multiplier = configs$volatility.threshold / aud.ml.sd # low -> more | high -> less
            cad.sd.multiplier = configs$volatility.threshold / cad.ml.sd # low -> more | high -> less
            aud.ret.multiplier = sign(aud.ml.ret - configs$classifier.threshold) *
                (abs(aud.ml.ret - configs$classifier.threshold) + configs$classifier.threshold)/configs$classifier.threshold
            cad.ret.multiplier = sign(cad.ml.ret - configs$classifier.threshold) *
                (abs(cad.ml.ret - configs$classifier.threshold) + configs$classifier.threshold)/configs$classifier.threshold


            # 7. Risk Management ------------------------------------------------------
            AUD.sd = max(tail(cur.spreads$AUDUSD, look.back)) + configs$sd.n * sd(tail(cur.spreads$AUDUSD, look.back))
            CAD.sd = max(tail(cur.spreads$USDCAD, look.back)) + configs$sd.n * sd(tail(cur.spreads$USDCAD, look.back))


            # 8. Mean-reversion & Momentum signals
            mean.reversion.signal = median(c(adf.signal, pp.signal, po.signal, var.ratio, jc.signal))
            momentum.signal = as.numeric(mom.direction * mom.effect)


            # 8. Execution ------------------------------------------------------------
            # 8.1 Mean-reverting signals
            if(mean.reversion.signal <= configs$mean.threshold & abs(zScore) >= configs$threshold){
                if(zScore >= configs$threshold){
                    goLong = FALSE
                    goShort = TRUE # buy
                    closePos = FALSE
                }else if(zScore <= -configs$threshold){
                    goLong = TRUE # sell
                    goShort = FALSE
                    closePos = FALSE
                }else if(abs(zScore) <= 0.1 | (inLong & zScore>0) | (inShort & zScore<0)){ # | unrealizedPnL <= stoploss
                    goLong = FALSE
                    goShort = FALSE
                    closePos = TRUE # sell OR buy
                }
                tradeType = 'MeanReversion'

            }else if(abs(momentum.signal) >= configs$moment.threshold){
                if(momentum.signal < 0){
                    goLong = FALSE
                    goShort = TRUE # buy
                    closePos = FALSE
                }else if(momentum.signal > 0){
                    goLong = TRUE # sell
                    goShort = FALSE
                    closePos = FALSE
                }else{
                    goLong = FALSE
                    goShort = FALSE
                    closePos = TRUE # sell OR buy
                }
                tradeType = 'Momentum'
            }else if(abs(momentum.signal) < configs$moment.threshold & tradeType == 'Momentum'){
                goLong = FALSE
                goShort = FALSE
                closePos = TRUE # sell OR buy
                tradeType = 'CloseMomentum'
            }else{
                tradeType = 'None'
            }


            # Positions & Risk Management --------------------------------------------------
            POS.USDCAD = 0
            POS.AUDUSD = 0
            aud.take.profit = 0
            aud.stop.loss = 0
            cad.take.profit = 0
            cad.stop.loss = 0
            AUDUSD.exe = as.numeric(last.price$AUDUSD)
            USDCAD.exe = as.numeric(1/last.price$USDCAD)

            if(goLong){
                POS.AUDUSD = positions$POS.AUDUSD
                POS.USDCAD = as.numeric(round(-positions$POS.USDCAD/(1/last.price$USDCAD)))

                if(sign(POS.AUDUSD) == sign(aud.ret.multiplier) & sign(POS.USDCAD) == sign(cad.ret.multiplier)){
                    aud.take.profit = round(as.numeric(AUDUSD.exe + sign(POS.AUDUSD) * AUD.sd), 5)
                    aud.stop.loss = round(as.numeric(AUDUSD.exe - sign(POS.AUDUSD) * AUD.sd), 5)
                    cad.take.profit = round(as.numeric(USDCAD.exe + sign(POS.USDCAD) * CAD.sd), 5)
                    cad.stop.loss = round(as.numeric(USDCAD.exe - sign(POS.USDCAD) * CAD.sd), 5)
                    AUDUSD.exe = round(as.numeric(AUDUSD.exe + sign(POS.AUDUSD) * configs$AUD.pip), 5)
                    USDCAD.exe = round(as.numeric(USDCAD.exe + sign(POS.USDCAD) * configs$CAD.pip), 5)

                    POS.AUDUSD = round(POS.AUDUSD * abs(aud.ret.multiplier)^2 * abs(cad.ret.multiplier)^2 * mean(c(aud.sd.multiplier, cad.sd.multiplier)))
                    POS.USDCAD = round(POS.USDCAD * abs(aud.ret.multiplier)^2 * abs(cad.ret.multiplier)^2 * mean(c(aud.sd.multiplier, cad.sd.multiplier)))

                    # Execution ---------------------------------------------------------------
                    aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD,
                                                     ORDERTYPE = 'MARKET', PRICE = AUDUSD.exe, TAKEPROFIT = aud.take.profit, TRAILINGSTOP = aud.stop.loss, TIMEINFORCE = "IOC")
                    cad.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'USD_CAD', UNITS = POS.USDCAD,
                                                     ORDERTYPE = 'MARKET', PRICE = USDCAD.exe, TAKEPROFIT = cad.take.profit, TRAILINGSTOP = cad.stop.loss, TIMEINFORCE = "IOC")

                    cat(paste0("\nGo Long | AUD Limit order at ", AUDUSD.exe, " | Take Profit: ", aud.take.profit, " | Trailing Stop Loss: ", aud.stop.loss, " | Positions: ", POS.AUDUSD))
                    cat(paste0("\nGo Long | CAD Limit order at ", USDCAD.exe, " | Take Profit: ", cad.take.profit, " | Trailing Stop Loss: ", cad.stop.loss, " | Positions: ", POS.USDCAD))

                    inLong = TRUE
                    inShort = FALSE
                }


            }else if(goShort){
                POS.AUDUSD = -positions$POS.AUDUSD
                POS.USDCAD = -as.numeric(round(-positions$POS.USDCAD/(1/last.price$USDCAD)))

                if(sign(POS.AUDUSD) == sign(aud.ret.multiplier) & sign(POS.USDCAD) == sign(cad.ret.multiplier)){
                    aud.take.profit = round(as.numeric(AUDUSD.exe + sign(POS.AUDUSD) * AUD.sd), 5)
                    aud.stop.loss = round(as.numeric(AUDUSD.exe - sign(POS.AUDUSD) * AUD.sd), 5)
                    cad.take.profit = round(as.numeric(USDCAD.exe + sign(POS.USDCAD) * CAD.sd), 5)
                    cad.stop.loss = round(as.numeric(USDCAD.exe - sign(POS.USDCAD) * CAD.sd), 5)
                    AUDUSD.exe = round(as.numeric(AUDUSD.exe + sign(POS.AUDUSD) * configs$AUD.pip), 5)
                    USDCAD.exe = round(as.numeric(USDCAD.exe + sign(POS.USDCAD) * configs$CAD.pip), 5)

                    POS.AUDUSD = round(POS.AUDUSD * abs(aud.ret.multiplier)^2 * abs(cad.ret.multiplier)^2 * mean(c(aud.sd.multiplier, cad.sd.multiplier)))
                    POS.USDCAD = round(POS.USDCAD * abs(aud.ret.multiplier)^2 * abs(cad.ret.multiplier)^2 * mean(c(aud.sd.multiplier, cad.sd.multiplier)))

                    # Execution ---------------------------------------------------------------
                    aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD,
                                                     ORDERTYPE = 'MARKET', PRICE = AUDUSD.exe, TAKEPROFIT = aud.take.profit, TRAILINGSTOP = aud.stop.loss, TIMEINFORCE = "IOC")
                    cad.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'USD_CAD', UNITS = POS.USDCAD,
                                                     ORDERTYPE = 'MARKET', PRICE = USDCAD.exe, TAKEPROFIT = cad.take.profit, TRAILINGSTOP = cad.stop.loss, TIMEINFORCE = "IOC")

                    cat(paste0("\nGo Short | AUD Limit order at ", AUDUSD.exe, " | Take Profit: ", aud.take.profit, " | Trailing Stop Loss: ", aud.stop.loss, " | Positions: ", POS.AUDUSD))
                    cat(paste0("\nGo Short | CAD Limit order at ", USDCAD.exe, " | Take Profit: ", cad.take.profit, " | Trailing Stop Loss: ", cad.stop.loss, " | Positions: ", POS.USDCAD))

                    inLong = FALSE
                    inShort = TRUE
                }


            }else if(closePos & (inLong | inShort)){

                CurrentPositions = setDT(getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID)$positions)

                POS.AUDUSD = -sum(as.numeric(CurrentPositions[instrument == 'AUD_USD', .(long.units,short.units)]))
                POS.USDCAD = -sum(as.numeric(CurrentPositions[instrument == 'USD_CAD', .(long.units,short.units)]))

                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD, TIMEINFORCE = "IOC")
                cad.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID, INSTRUMENTS = 'USD_CAD', UNITS = POS.USDCAD, TIMEINFORCE = "IOC")

                inLong = FALSE
                inShort = FALSE
            }
            cat(paste0('\nZScore: ', round(zScore, 2), ' | Mean-reversion: ', round(mean.reversion.signal,2),' | Momentum: ', round(momentum.signal,2), '\n'))



            # Re-check positions
            CurrentPositions = setDT(getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, configs$ACCOUNT_ID)$positions)
            POS.AUDUSD = sum(as.numeric(CurrentPositions[instrument == 'AUD_USD', .(long.units,short.units)]))
            POS.USDCAD = sum(as.numeric(CurrentPositions[instrument == 'USD_CAD', .(long.units,short.units)]))

            latest.aud.price = NA
            latest.aud.openTime = NA
            latest.cad.price = NA
            latest.cad.openTime = NA
            if(goShort | goLong){
                # latestTradeID = getLatestTradeID(configs)
                latestTrade = getLatestTrades(configs)
                latest.aud.price = as.numeric(latestTrade$aud.trade$price)
                latest.aud.openTime = latestTrade$aud.trade$openTime
                latest.cad.price = as.numeric(latestTrade$cad.trade$price)
                latest.cad.openTime = latestTrade$cad.trade$openTime
            }



            # Order book --------------------------------------------------------------
            # Update Portf Information
            OB.PRICES = last.price; OB.PRICES[, 2] = 1/OB.PRICES[, 2]
            names(ols.pvalues) = paste0('OLS.P.', names(ols.pvalues))
            unrealizedPnL = sum(as.numeric(CurrentPositions$unrealizedPL))
            PnL = sum(as.numeric(CurrentPositions$pl))
            # Create latest order record
            orderBook = data.frame(as.character(index(OB.PRICES)), OB.PRICES, adf.signal, adf.signal.lag1, pp.signal, po.signal,
                                   jc.signal, var.ratio, hurst.signal, pvmr.par, half.life.par, mean.reversion.signal,
                                   macd, macd.signal, ema.signal, mom.hamming.dist, mom.spearman, mom.thickness, mom.rsi, momentum.signal,
                                   cor.long, cor.short, zScore, ols.r2, t(ols.pvalues),
                                   aud.ml.ret, aud.ret.multiplier, cad.ml.ret, cad.ret.multiplier,
                                   aud.ml.sd, aud.sd.multiplier, cad.ml.sd, cad.sd.multiplier,
                                   POS.AUDUSD, POS.USDCAD, t(hedgeRatio),
                                   latest.aud.price,latest.aud.openTime,latest.cad.price,latest.cad.openTime,
                                   aud.take.profit, aud.stop.loss, cad.take.profit, cad.stop.loss,
                                   inLong, inShort, goLong, goShort, closePos, tradeType, unrealizedPnL, PnL)
            names(orderBook) = c('DateTime', paste0('P.', names(OB.PRICES)), 'ADF', 'ADF.lag1', 'Phillips-Perron', 'Phillips-Ouliaris',
                                 'Johansen', 'Variance.Ratio', 'Hurst', 'Partial.Auto.pvmr','Partial.Auto.HL', 'Mean-Reversion',
                                 'MACD','MACD.Signal', 'EMA.Signal', 'Hamming.Dist', 'Mom.Spearman', 'Mom.Thickness', 'RSI', 'Momentum',
                                 'Corr.long','Corr.short', 'ZScore', 'OLS.r2', names(ols.pvalues),
                                 'AUD.ML.Returns', 'AUD.ML.Ret.Multiplier', 'CAD.ML.Returns', 'CAD.ML.Ret.Multiplier',
                                 'AUD.ML.Volatility', 'AUD.ML.SD.Multiplier', 'CAD.ML.Volatility', 'CAD.ML.SD.Multiplier',
                                 'POS.AUDUSD', 'POS.USDCAD', 'Hedge.Ratio.AUD', 'Hedge.Ratio.CAD',
                                 'AUD.Exec.Price','AUD.Order.Time', 'CAD.Exec.Price', 'CAD.Order.Time',
                                 'AUD.TakeProfit', 'AUD.StopLoss', 'CAD.TakeProfit', 'CAD.StopLoss',
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
            dev.off()


            updateDirFilePermissions(configs$strategyPath)
        }
    },
    error = function(e){print(e)}
    )

    Sys.sleep(0.1)
}
sink(type="message")
sink()

