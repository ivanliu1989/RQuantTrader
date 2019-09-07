rm(list=ls());gc()
loadQuantPackages()
source('tests/AutomatedSystems/IBPaperTradingIndicators.R')
source('tests/h2o/h2oDeeplearningMain.R')
source('tests/caret/caretMachineLearningEnsemble.R')
Sys.setenv(TZ='US/Eastern')

# Sunday approximately 5 p.m. to Friday 5 p.m
AUDUSD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "AUD", Cur2 = "USD", oanda.granularity = 'D')
USDSEK = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "SEK", oanda.granularity = 'D')
USDCZK = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "CZK", oanda.granularity = 'D')
USDNOK = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "NOK", oanda.granularity = 'D')
USDSGD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "SGD", oanda.granularity = 'D')
USDTRY = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "TRY", oanda.granularity = 'D')
USDJPY = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "JPY", oanda.granularity = 'D')
EURUSD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "EUR", Cur2 = "USD", oanda.granularity = 'D')
GBPUSD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "GBP", Cur2 = "USD", oanda.granularity = 'D')
NZDUSD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "NZD", Cur2 = "USD", oanda.granularity = 'D')


# 1. Price model ----------------------------------------------------------
aud = Cl(AUDUSD$OA.MID); #index(aud) = as.Date(index(aud))
sek = Cl(1/USDSEK$OA.MID); #index(huf) = as.Date(index(huf))
czk = Cl(1/USDCZK$OA.MID); #index(huf) = as.Date(index(huf))
nok = Cl(1/USDNOK$OA.MID); #index(huf) = as.Date(index(huf))
sgd = Cl(1/USDSGD$OA.MID); #index(huf) = as.Date(index(huf))
try = Cl(1/USDTRY$OA.MID); #index(huf) = as.Date(index(huf))
jpy = Cl(1/USDJPY$OA.MID); #index(huf) = as.Date(index(huf))
eur = Cl(EURUSD$OA.MID); #index(huf) = as.Date(index(huf))
gbp = Cl(GBPUSD$OA.MID); #index(huf) = as.Date(index(huf))
nzd = Cl(NZDUSD$OA.MID); #index(huf) = as.Date(index(huf))

aud.ret = log10(Cl(AUDUSD$OA.MID)/Op(AUDUSD$OA.MID))
sek.ret = log10(Cl(1/USDSEK$OA.MID)/Op(1/USDSEK$OA.MID))
czk.ret = log10(Cl(1/USDCZK$OA.MID)/Op(1/USDCZK$OA.MID))
nok.ret = log10(Cl(1/USDNOK$OA.MID)/Op(1/USDNOK$OA.MID))
sgd.ret = log10(Cl(1/USDSGD$OA.MID)/Op(1/USDSGD$OA.MID))
try.ret = log10(Cl(1/USDTRY$OA.MID)/Op(1/USDTRY$OA.MID))
jpy.ret = log10(Cl(1/USDJPY$OA.MID)/Op(1/USDJPY$OA.MID))
eur.ret = log10(Cl(EURUSD$OA.MID)/Op(EURUSD$OA.MID))
gbp.ret = log10(Cl(GBPUSD$OA.MID)/Op(GBPUSD$OA.MID))
nzd.ret = log10(Cl(NZDUSD$OA.MID)/Op(NZDUSD$OA.MID))

# Iteration 1 -------------------------------------------------------------
cols = c('AUDUSD', 'USDSEK', 'USDCZK', 'USDNOK', 'USDSGD', 'USDTRY', 'USDJPY', 'EURUSD', 'GBPUSD', 'NZDUSD')
portf = na.omit(merge(aud.ret, sek.ret, czk.ret, nok.ret, sgd.ret, try.ret, jpy.ret
                      , eur.ret, gbp.ret, nzd.ret)); names(portf) = cols
prices = na.omit(merge(aud, sek, czk, nok, sgd, try, jpy, eur, gbp, nzd)); names(prices) = cols



# 2. Configurations -------------------------------------------------------
for(jl in c(65:85,335:350)){
    inLong = FALSE
    inShort = FALSE
    initAssets = 1000000
    posiRate = 0.05
    johansen.lookback = 340
    ols.lookback = 340
    hurst.lookback = 21
    nfx = 10
    threshold = 1.0
    adf.threshold = 0.7
    jc.threshold = 0.7
    hurst.threshold = 0.8
    stoploss = -0.50
    obs = nrow(portf)

    rnd = 0
    initPortf = c(rep(0,36), rep(FALSE, 5), 0, initAssets, 0, initAssets)
    initPortf = data.frame(t(initPortf))
    unrealizedPnL = 0
    for(r in (johansen.lookback+1):obs){

        goLong = FALSE
        goShort = FALSE
        closePos = FALSE

        # tmp.ret = portf[(r-johansen.lookback):r, cols]
        # tmp.ols = portf[(r-ols.lookback):r, cols]
        tmp.ret = prices[(r-johansen.lookback):r, cols]
        tmp.ols = prices[(r-ols.lookback):r, cols]
        tmp.prices = prices[(r-johansen.lookback):r, cols]

        # 3. Statistics Test ------------------------------------------------------
        # stats tests
        jc.res=JohansenCointegrationTest(tmp.ret, type = "eigen", ecdet = 'const', K = 2)
        ols.fit = lm(AUDUSD~. , tmp.ols)
        ols.r2 = summary(ols.fit)$r.squared

        # Spreads
        # hedgeRatio = c(1, -ols.fit$coefficients[-1])
        hedgeRatio = jc.res$jc.test@V[1:(nrow(jc.res$jc.test@V)-1), 1]
        spreads = tmp.ret %*% t(data.frame(hedgeRatio))[1:nfx]
        spreads = as.xts(spreads, order.by = index(tmp.ret))
        # chart_Series(spreads)
        spreads = na.omit(spreads)

        # stats tests 2
        adf.res=adf.test(spreads) #0.06999
        hurst.res=as.numeric(tail(HurstExponentTest(spreads, hurst.lookback)$hurstKY, 1)) # > 0.5 trending | < 0.5 mean reverting
        half.life <- HalfLifeMeanReversion(spreads)$half.life.round

        # Technical indicators
        # bbands = na.omit(BollingerBands(spreads, 20, 2))
        # rsi = RSI(spreads)
        # stochOsc = stoch(spreads)

        # mean reversion signals
        adf.signal=adf.res$p.value
        hurst.signal=hurst.res
        jc.signal=as.numeric(((jc.res$p.value[1]-jc.res$r.1)/((jc.res$p.value[3]-jc.res$p.value[1])/10)+10)/100)
        # bb.signal = ifelse(bbands$spread<=bbands$BBlow, -1, ifelse(bbands$spread>=bbands$BBhigh, 1, 0))
        # rsi.signal = ifelse(rsi<=30, -1, ifelse(rsi>=70, 1, 0))
        # stock.signal = stochOsc$fastK - stochOsc$slowD
        # stock.signal = diff(sign(stock.signal))
        # stock.signal = ifelse(stock.signal == -2, 'sell', ifelse(stock.signal == 2, 'buy', 'na'))
        # momentum signals
        # macd.signal = MACD(spreads)


        # 4. Mean Reversion -------------------------------------------------------
        # !!! Normalisation
        # hedge ratio
        zScore=tail(zscores(spreads),1)
        # chart_Series(zscores(spreads))
        numUnits=-tail(zScore,1)
        hedgeRatio = t(data.frame(hedgeRatio))
        # hedgeRatio[2:4] = -1/hedgeRatio[2:4]
        # dollar neutrality
        sizing = abs(numUnits)/1
        posUnits = initAssets * posiRate
        dollars = round(as.numeric(posUnits * sizing))
        dollar.neut = hedgeRatio * dollars/abs(sum(hedgeRatio))
        # positions
        last.price = tail(tmp.prices,1)
        # last.price[,2:4] = 1/last.price[,2:4]
        positions = as.data.frame(round(dollar.neut / tail(last.price,1)))
        names(positions) = paste0('POS.', cols)
        # positions[2:4] = ceiling(- positions[2:4] / as.numeric(1/last.price[,2:4]))

        # 5. Execution ------------------------------------------------------------
        # stats arbitragy triggers
        if(zScore >= threshold & (adf.signal <= adf.threshold | jc.signal <= jc.threshold | hurst.signal <= hurst.threshold)){
            goLong = FALSE
            goShort = TRUE # buy
            closePos = FALSE
        }else if(zScore <= -threshold & (adf.signal <= adf.threshold | jc.signal <= jc.threshold | hurst.signal <= hurst.threshold)){
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
            lstPos = lstRcd[17:26]
        }else{
            lstRcd = Order.Book[r-johansen.lookback,]
            lstPos = lstRcd[17:26]
        }

        if(goLong){
            pos.fnl = positions + lstPos
            mkt.fnl = pos.fnl * data.frame(last.price)
            inLong = TRUE
            inShort = FALSE
        }else if(goShort){
            pos.fnl = -positions + lstPos
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
        # mkt.fnl[2:4] = mkt.fnl[2:4] * as.numeric(last.price[,2:4])
        names(mkt.fnl) = paste0('MKT.', cols)
        sumMktValue = sum(mkt.fnl)
        actualValue = lstRcd[25] - sumMktValue

        unrealizedPnL = sum((data.frame(diff(tail(tmp.prices,2))[2,]) * lstPos)) / sum((data.frame(tail(tmp.prices,2)[1,]) * abs(lstPos)))
        unrealizedPnL = round(ifelse(is.nan(unrealizedPnL), 0, unrealizedPnL), 5)


        # PnL = sumMktValue + actualValue + unrealizedPnL
        PnL = round(sum((data.frame(diff(tail(tmp.prices,2))[2,]))) / sum((data.frame(tail(tmp.prices,2)[1,]))),5)

        orderBook = data.frame(last.price, adf.signal, jc.signal, hurst.signal, half.life, zScore, ols.r2,
                               pos.fnl, mkt.fnl, inLong, inShort, goLong, goShort, closePos,
                               sumMktValue, actualValue, unrealizedPnL, PnL)
        names(orderBook) = c(paste0('P.', names(tmp.prices)), 'ADF', 'Johansen', 'Hurst', 'H.L', 'ZScore', 'ols.r2',
                             paste0('Pos.', names(tmp.prices)), paste0('Mkt.', names(tmp.prices)),
                             'inLong', 'inShort', 'goLong', 'goShort', 'closePos',
                             'sumMktValue', 'actualValue', 'unrealizedPnL', 'PnL')
        if(r == (johansen.lookback+1)){
            names(initPortf) = names(orderBook)
            Order.Book = rbind(initPortf, orderBook)
        }else{
            Order.Book = rbind(Order.Book,orderBook)
        }

        cat(paste0('\n', unrealizedPnL, ' | ', PnL))
    }

    Order.Book = as.xts(Order.Book[-1,], order.by = as.Date(rownames(Order.Book[-1,])))
    # Order.Book = tail(Order.Book, 500)
    ret = Order.Book[,c('unrealizedPnL', 'PnL', 'ADF')]
    # hist(ret[,1], 100);
    # hist(ret[,2], 100)
    # quantile(ret[,1],probs = seq(0, 1, 0.25))
    cat(paste0('\nJC lookback: ', johansen.lookback, ' Score: ',sum(ret[,1]), '|', sum(ret[,2])))
    charts.PerformanceSummary(ret[, c(1,2)])
    # chart_Series(ret[,3])
    # sharpe
    # (mean(ret[,1]) * 252 - mean(ret[,2]) * 252)/ sd(ret[,1])
    # (mean(ret[ret[,1]!=0, 1]) * 252 * 24 - 0.02) / sd(ret[ret[,1]!=0,1])
    # (mean(ret[,1]) * 252 - 0.02) / sd(ret[,1])
}



# 12.3% (eigvector 63)
# 13.49% (eigvector 126)
# -9.21% (eigvector 252)
# -0.65% (eigvector 31)
# 2.13% (eigvector 84)
# -8.1% (ols 5)
# -29.0% (ols 14)
# View(Order.Book)



# 170:167 (50.45%)
# 0.813:-0.69
# 661:693 (48.82%)
# 2.526:-2.801
# 26 % exec

# 220:199 (52.5%)
# 1.32412:-1.32245
# 661:693 (48.82%)
# 2.526:-2.801

table(Order.Book$unrealizedPnL > 0)
table(Order.Book$unrealizedPnL < 0)
sum(Order.Book$unrealizedPnL[Order.Book$unrealizedPnL > 0])
sum(Order.Book$unrealizedPnL[Order.Book$unrealizedPnL < 0])

table(Order.Book$PnL > 0)
table(Order.Book$PnL < 0)
sum(Order.Book$PnL[Order.Book$PnL > 0])
sum(Order.Book$PnL[Order.Book$PnL < 0])

# hurst0.7: 1.7 124
