#####################################################
### OANDA MEAN REVERSION PORTFOLIO -- DAILY BASIS ###
#####################################################
rm(list=ls());gc()
loadQuantPackages()
Sys.setenv(TZ='GMT')

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
getLatestPrices = function(){
    AUDUSD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "AUD", Cur2 = "USD", oanda.granularity = 'D')
    USDCAD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "CAD", oanda.granularity = 'D')

    aud = Cl(AUDUSD$OA.MID); index(aud) = as.Date(index(aud))
    cad = Cl(1/USDCAD$OA.MID); index(cad) = as.Date(index(cad))

    aud.ret = log10(Cl(AUDUSD$OA.MID)/Op(AUDUSD$OA.MID))
    cad.ret = log10(Cl(1/USDCAD$OA.MID)/Op(1/USDCAD$OA.MID))

    aud.spreads = Hi(AUDUSD$OA.MID) - Lo(AUDUSD$OA.MID)
    cad.spreads = Hi(USDCAD$OA.MID) - Lo(USDCAD$OA.MID)

    cols = c('AUDUSD', 'USDCAD')
    portf.new = na.omit(merge(aud.ret, cad.ret)); names(portf.new) = cols
    prices.new = na.omit(merge(aud, cad)); names(prices.new) = cols
    cur.spreads = na.omit(merge(aud.spreads, cad.spreads)); names(cur.spreads) = cols

    return(list(portf.new = tail(portf.new,1),
                prices.new = tail(prices.new,1),
                cur.spreads = tail(cur.spreads,1),
                update.date = as.Date(index(tail(prices.new,1)))))
}
# Get Latest Trade IDs
getLatestTradeID = function(){
    return(list(
        aud.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('AUD_USD'))$trades$id[1],
        cad.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_CAD'))$trades$id[1]
    ))
}

getLatestTrades = function(){
    return(list(
        aud.trade = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('AUD_USD'))$trades[1,],
        cad.trade = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_CAD'))$trades[1,]
    ))
}

# Sunday approximately 5 p.m. to Friday 5 p.m
AUDUSD = prepareForexOandaPrices(oanda.count = 1000, Cur1 = "AUD", Cur2 = "USD", oanda.granularity = 'D')
USDCAD = prepareForexOandaPrices(oanda.count = 1000, Cur1 = "USD", Cur2 = "CAD", oanda.granularity = 'D')

aud = Cl(AUDUSD$OA.MID); index(aud) = as.Date(index(aud))
cad = Cl(1/USDCAD$OA.MID); index(cad) = as.Date(index(cad))

aud.ret = log10(Cl(AUDUSD$OA.MID)/Op(AUDUSD$OA.MID))
cad.ret = log10(Cl(1/USDCAD$OA.MID)/Op(1/USDCAD$OA.MID))

aud.spreads = Hi(AUDUSD$OA.MID) - Lo(AUDUSD$OA.MID)
cad.spreads = Hi(USDCAD$OA.MID) - Lo(USDCAD$OA.MID)

cols = c('AUDUSD', 'USDCAD')
portf = na.omit(merge(aud.ret, cad.ret)); names(portf) = cols
prices = na.omit(merge(aud, cad)); names(prices) = cols
cur.spreads = na.omit(merge(aud.spreads, cad.spreads)); names(cur.spreads) = cols


# 2. Configuration --------------------------------------------------------
order.book.file = '~/Common/audcad_interday_d1/orderbook/ivanliu_AUDCAD_Pairs_Daily_001.csv'
log.file = file(paste0('~/Common/audcad_interday_d1/log/ivanliu_AUDCAD_Pairs_Daily_001_', as.Date(Sys.time()), '.Rout'), open = 'wt')
inLong = FALSE
inShort = FALSE
initAssets = 1000000
posiRate = 0.05
nfx = 2
threshold = 1.5
mean.threshold = 0.2
moment.threshold = 1.05
stoploss = -0.10
ACCOUNT_ID = '101-011-4686012-001'
train.length = 504
min.lookback = 21
max.lookback = 63
sd.n = 3

sink(log.file)
sink(log.file, type="message")
while(TRUE){

    tryCatch({
        latestPrices = getLatestPrices()
        lastDate = as.Date(index(tail(prices, 1)))

        if(latestPrices$update.date > lastDate){

            cat(paste0('\n\nNew Event Found! Start trading with ', lastDate, '............\n'))
            portf = rbind(portf, latestPrices$portf.new)
            prices = rbind(prices, latestPrices$prices.new)
            cur.spreads = rbind(cur.spreads, latestPrices$cur.spreads)
            r = nrow(prices)
            goLong = FALSE
            goShort = FALSE
            closePos = FALSE

            # Model data --------------------------------------------------------------
            tmp.prices = tail(prices[, cols], train.length)
            ols.fit = lm(AUDUSD~. , log(tmp.prices))
            ols.r2 = summary(ols.fit)$r.squared
            ols.pvalues = summary(ols.fit)[[4]][,4]

            fit.hedge = as.numeric(c(1,-ols.fit$coefficients[2]))
            spreads = tmp.prices %*% fit.hedge
            spreads = as.xts(spreads, order.by = index(tmp.prices))

            adf.lm = adf.test(spreads, k = 1) # Augmented Dickey-Fuller test
            pp.lm = pp.test(spreads) # Phillips-Perron Unit Root test
            po.lm = po.test(tmp.prices * fit.hedge) # Phillips-Ouliaris Cointegration test
            # chart_Series(spreads)

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


            # Correlation--------------------------------------------------------------
            cor.long = as.numeric(tail(runCor(x = tmp.prices[, 1], y = tmp.prices[, 2], n=train.length), 1))
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
            png(paste0("~/Common/audcad_interday_d1/plots/audcad_interday_d1_",format(index(tail(tmp.prices,1)), "%Y%m%d%H%M"),".png"))
            par(mfcol = c(3,1))
            print(chart_Series(tail(zscores(spreads),300)))
            print(chart_Series(tail(Z.Score,300)))
            print(chart_Series(tail(zscores.ma(spreads, look.back, round(look.back/6)),300)))
            dev.off()

            # Position Sizing
            zScore=tail(Z.Score,1)
            numUnits=-tail(zScore,1)
            # Dollar neutrality
            sizing = abs(numUnits)/1
            posUnits = initAssets * posiRate
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
            # macd = ifelse(macd > macd.signal, -1, ifelse(macd < macd.signal, 1, 0))
            # macd.signal = ifelse(macd.signal < 0, -1, ifelse(macd.signal > 0, 1, 0))
            # ema.signal = ifelse(ema.signal < 0, -1, ifelse(ema.signal > 0, 1, 0))
            # mom.zScore = ifelse(zScore <= -2, -1, ifelse(zScore >= 2, 1, 0)) # > 2 | < -2 momentum
            # mom.spearman = ifelse(mom.spearman > 0, -1, ifelse(mom.spearman < 0, 1, 0))
            # mom.rsi = ifelse(mom.rsi >= 70, 1, ifelse(mom.rsi <= -70, -1, 0))
            # mom.direction = mean(c(macd, macd.signal, ema.signal, mom.zScore, mom.spearman, mom.rsi))
            mom.direction = sign(tail(diff(Z.Score),1))
            # Strongness
            mom.effect = mean(c(mom.hamming.dist/5, mom.thickness, hurst.signal/0.5))


            # 6. Risk Management ------------------------------------------------------
            AUD.sd = sd(tail(cur.spreads$AUDUSD, look.back))
            CAD.sd = sd(tail(cur.spreads$USDCAD, look.back))


            # 7. Mean-reversion & Momentum signals
            mean.reversion.signal = mean(c(adf.signal, pp.signal, po.signal, var.ratio, jc.signal))
            momentum.signal = mom.direction * mom.effect

            # 8. Execution ------------------------------------------------------------
            if(mean.reversion.signal <= mean.threshold & abs(zScore) >= threshold){
                if(zScore >= threshold){
                    goLong = FALSE
                    goShort = TRUE # buy
                    closePos = FALSE
                }else if(zScore <= -threshold){
                    goLong = TRUE # sell
                    goShort = FALSE
                    closePos = FALSE
                }else if(abs(zScore) < 0.1){ # | unrealizedPnL <= stoploss
                    goLong = FALSE
                    goShort = FALSE
                    closePos = TRUE # sell OR buy
                }
                tradeType = 'MeanReversion'

            }else if(abs(momentum.signal) >= moment.threshold){
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
            }else if(abs(momentum.signal) < moment.threshold & tradeType == 'Momentum'){
                goLong = FALSE
                goShort = FALSE
                closePos = TRUE # sell OR buy
                tradeType = 'CloseMomentum'
            }else{
                tradeType = 'None'
            }

            # Positions ---------------------------------------------------------------
            POS.AUDUSD = 0
            POS.USDCAD = 0
            if(goLong){
                POS.AUDUSD = positions$POS.AUDUSD
                POS.USDCAD = as.numeric(round(-positions$POS.USDCAD/(1/last.price$USDCAD)))

                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD)
                cad.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CAD', UNITS = POS.USDCAD)

                inLong = TRUE
                inShort = FALSE

            }else if(goShort){
                POS.AUDUSD = -positions$POS.AUDUSD
                POS.USDCAD = -as.numeric(round(-positions$POS.USDCAD/(1/last.price$USDCAD)))

                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD)
                cad.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CAD', UNITS = POS.USDCAD)

                inLong = FALSE
                inShort = TRUE

            }else if(closePos & (inLong | inShort)){

                CurrentPositions = setDT(getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID)$positions)

                POS.AUDUSD = -sum(as.numeric(CurrentPositions[instrument == 'AUD_USD', .(long.units,short.units)]))
                POS.USDCAD = -sum(as.numeric(CurrentPositions[instrument == 'USD_CAD', .(long.units,short.units)]))

                # Execution ---------------------------------------------------------------
                aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD)
                cad.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CAD', UNITS = POS.USDCAD)

                inLong = FALSE
                inShort = FALSE
            }
            cat(paste0('ZScore: ', round(zScore, 2), ' | Mean-reversion: ', round(mean.reversion.signal,2),' | Momentum: ',
                       round(momentum.signal,2), ' | CAD Position: ', POS.USDCAD, ' | AUD Position: ', POS.AUDUSD, '\n'))

            # Risk Management ---------------------------------------------------------
            aud.take.profit = 0
            aud.stop.loss = 0
            cad.take.profit = 0
            cad.stop.loss = 0
            latest.aud.price = NA
            latest.aud.openTime = NA
            latest.cad.price = NA
            latest.cad.openTime = NA
            if(goShort | goLong){
                latestTradeID = getLatestTradeID()
                latestTrade = getLatestTrades()
                latest.aud.price = as.numeric(latestTrade$aud.trade$price)
                latest.aud.openTime = latestTrade$aud.trade$openTime
                latest.cad.price = as.numeric(latestTrade$cad.trade$price)
                latest.cad.openTime = latestTrade$cad.trade$openTime

                aud.take.profit = as.numeric(latest.aud.price + sd.n * sign(POS.AUDUSD) * AUD.sd)
                aud.stop.loss = as.numeric(latest.aud.price - sd.n * sign(POS.AUDUSD) * AUD.sd)
                aud.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$aud.tradeid,
                                                  PRICE = aud.take.profit, ORDERTYPE = 'TAKE_PROFIT')
                aud.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$aud.tradeid,
                                                  PRICE = aud.stop.loss, ORDERTYPE = 'STOP_LOSS')

                cad.take.profit = as.numeric(latest.cad.price + sd.n * sign(POS.USDCAD) * CAD.sd)
                cad.stop.loss = as.numeric(latest.cad.price - sd.n * sign(POS.USDCAD) * CAD.sd)
                cad.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$cad.tradeid,
                                                  PRICE = cad.take.profit, ORDERTYPE = 'TAKE_PROFIT')
                cad.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$cad.tradeid,
                                                  PRICE = cad.stop.loss, ORDERTYPE = 'STOP_LOSS')
            }

            # Re-check positions
            CurrentPositions = setDT(getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID)$positions)
            POS.AUDUSD = sum(as.numeric(CurrentPositions[instrument == 'AUD_USD', .(long.units,short.units)]))
            POS.USDCAD = sum(as.numeric(CurrentPositions[instrument == 'USD_CAD', .(long.units,short.units)]))


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
                                   POS.AUDUSD, POS.USDCAD, t(hedgeRatio),
                                   latest.aud.price,latest.aud.openTime,latest.cad.price,latest.cad.openTime,
                                   aud.take.profit, aud.stop.loss, cad.take.profit, cad.stop.loss,
                                   inLong, inShort, goLong, goShort, closePos, tradeType, unrealizedPnL, PnL)
            names(orderBook) = c('DateTime', paste0('P.', names(OB.PRICES)), 'ADF', 'ADF.lag1', 'Phillips-Perron', 'Phillips-Ouliaris',
                                 'Johansen', 'Variance.Ratio', 'Hurst', 'Partial.Auto.pvmr','Partial.Auto.HL', 'Mean-Reversion',
                                 'MACD','MACD.Signal', 'EMA.Signal', 'Hamming.Dist', 'Mom.Spearman', 'Mom.Thickness', 'RSI', 'Momentum',
                                 'Corr.long','Corr.short', 'ZScore', 'OLS.r2', names(ols.pvalues),
                                 'POS.AUDUSD', 'POS.USDCAD', 'Hedge.Ratio.AUD', 'Hedge.Ratio.CAD',
                                 'AUD.Exec.Price','AUD.Order.Time', 'CAD.Exec.Price', 'CAD.Order.Time',
                                 'AUD.TakeProfit', 'AUD.StopLoss', 'CAD.TakeProfit', 'CAD.StopLoss',
                                 'inLong', 'inShort', 'goLong', 'goShort', 'closePos', 'tradeType', 'unrealizedPnL', 'PnL')
            setDT(orderBook)

            if(file.exists(order.book.file)){
                Order.Book = fread(order.book.file)
                Order.Book = rbind(Order.Book, orderBook)
                colnames(orderBook) = colnames(Order.Book)
                write.csv(Order.Book, file = order.book.file, row.names = FALSE)
            }else{
                write.csv(orderBook, file = order.book.file, row.names = FALSE)
            }

            updateDirFilePermissions('~/Common/audcad_interday_d1/')
        }

    },
    error = function(e){print(e)}
    )

    Sys.sleep(1)
}
sink(type="message")
sink()

