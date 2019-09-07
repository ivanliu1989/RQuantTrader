#####################################################
### OANDA MEAN REVERSION PORTFOLIO -- DAILY BASIS ###
#####################################################
# Lookback: 340
rm(list=ls());gc()
loadQuantPackages()
Sys.setenv(TZ='US/Eastern')

getLatestPrices = function(){
    AUDUSD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "AUD", Cur2 = "USD", oanda.granularity = 'D')
    USDSEK = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "SEK", oanda.granularity = 'D')
    USDCZK = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "CZK", oanda.granularity = 'D')
    USDNOK = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "NOK", oanda.granularity = 'D')
    USDSGD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "SGD", oanda.granularity = 'D')
    USDTRY = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "TRY", oanda.granularity = 'D')
    USDJPY = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "JPY", oanda.granularity = 'D')
    EURUSD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "EUR", Cur2 = "USD", oanda.granularity = 'D')
    GBPUSD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "GBP", Cur2 = "USD", oanda.granularity = 'D')
    NZDUSD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "NZD", Cur2 = "USD", oanda.granularity = 'D')

    aud = Cl(AUDUSD$OA.MID); index(aud) = as.Date(index(aud))
    sek = Cl(1/USDSEK$OA.MID); index(sek) = as.Date(index(sek))
    czk = Cl(1/USDCZK$OA.MID); index(czk) = as.Date(index(czk))
    nok = Cl(1/USDNOK$OA.MID); index(nok) = as.Date(index(nok))
    sgd = Cl(1/USDSGD$OA.MID); index(sgd) = as.Date(index(sgd))
    try = Cl(1/USDTRY$OA.MID); index(try) = as.Date(index(try))
    jpy = Cl(1/USDJPY$OA.MID); index(jpy) = as.Date(index(jpy))
    eur = Cl(EURUSD$OA.MID); index(eur) = as.Date(index(eur))
    gbp = Cl(GBPUSD$OA.MID); index(gbp) = as.Date(index(gbp))
    nzd = Cl(NZDUSD$OA.MID); index(nzd) = as.Date(index(nzd))

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

    aud.spreads = Hi(AUDUSD$OA.MID) - Lo(AUDUSD$OA.MID)
    sek.spreads = Hi(USDSEK$OA.MID) - Lo(USDSEK$OA.MID)
    czk.spreads = Hi(USDCZK$OA.MID) - Lo(USDCZK$OA.MID)
    nok.spreads = Hi(USDNOK$OA.MID) - Lo(USDNOK$OA.MID)
    sgd.spreads = Hi(USDSGD$OA.MID) - Lo(USDSGD$OA.MID)
    try.spreads = Hi(USDTRY$OA.MID) - Lo(USDTRY$OA.MID)
    jpy.spreads = Hi(USDJPY$OA.MID) - Lo(USDJPY$OA.MID)
    eur.spreads = Hi(EURUSD$OA.MID) - Lo(EURUSD$OA.MID)
    gbp.spreads = Hi(GBPUSD$OA.MID) - Lo(GBPUSD$OA.MID)
    nzd.spreads = Hi(NZDUSD$OA.MID) - Lo(NZDUSD$OA.MID)

    cols = c('AUDUSD', 'USDSEK', 'USDCZK', 'USDNOK', 'USDSGD', 'USDTRY', 'USDJPY', 'EURUSD', 'GBPUSD', 'NZDUSD')
    portf.new = na.omit(merge(aud.ret, sek.ret, czk.ret, nok.ret, sgd.ret, try.ret, jpy.ret
                              , eur.ret, gbp.ret, nzd.ret)); names(portf) = cols
    prices.new = na.omit(merge(aud, sek, czk, nok, sgd, try, jpy, eur, gbp, nzd)); names(prices) = cols
    cur.spreads = na.omit(merge(aud.spreads, sek.spreads, czk.spreads, nok.spreads, sgd.spreads, try.spreads, jpy.spreads
                                , eur.spreads, gbp.spreads, nzd.spreads)); names(cur.spreads) = cols

    return(list(portf.new = tail(portf.new,1),
                prices.new = tail(prices.new,1),
                cur.spreads = tail(cur.spreads,1),
                update.date = as.Date(index(tail(prices.new,1)))))
}


getLatestTradeID = function(){

    return(list(
        aud.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('AUD_USD'))$trades$id[1],
        sek.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_SEK'))$trades$id[1],
        czk.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_CZK'))$trades$id[1],
        nok.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_NOK'))$trades$id[1],
        sgd.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_SGD'))$trades$id[1],
        try.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_TRY'))$trades$id[1],
        jpy.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_JPY'))$trades$id[1],
        eur.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('EUR_USD'))$trades$id[1],
        gbp.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('GBP_USD'))$trades$id[1],
        nzd.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('NZD_USD'))$trades$id[1]
    ))
}


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
aud = Cl(AUDUSD$OA.MID); index(aud) = as.Date(index(aud))
sek = Cl(1/USDSEK$OA.MID); index(sek) = as.Date(index(sek))
czk = Cl(1/USDCZK$OA.MID); index(czk) = as.Date(index(czk))
nok = Cl(1/USDNOK$OA.MID); index(nok) = as.Date(index(nok))
sgd = Cl(1/USDSGD$OA.MID); index(sgd) = as.Date(index(sgd))
try = Cl(1/USDTRY$OA.MID); index(try) = as.Date(index(try))
jpy = Cl(1/USDJPY$OA.MID); index(jpy) = as.Date(index(jpy))
eur = Cl(EURUSD$OA.MID); index(eur) = as.Date(index(eur))
gbp = Cl(GBPUSD$OA.MID); index(gbp) = as.Date(index(gbp))
nzd = Cl(NZDUSD$OA.MID); index(nzd) = as.Date(index(nzd))

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

aud.spreads = Hi(AUDUSD$OA.MID) - Lo(AUDUSD$OA.MID)
sek.spreads = Hi(USDSEK$OA.MID) - Lo(USDSEK$OA.MID)
czk.spreads = Hi(USDCZK$OA.MID) - Lo(USDCZK$OA.MID)
nok.spreads = Hi(USDNOK$OA.MID) - Lo(USDNOK$OA.MID)
sgd.spreads = Hi(USDSGD$OA.MID) - Lo(USDSGD$OA.MID)
try.spreads = Hi(USDTRY$OA.MID) - Lo(USDTRY$OA.MID)
jpy.spreads = Hi(USDJPY$OA.MID) - Lo(USDJPY$OA.MID)
eur.spreads = Hi(EURUSD$OA.MID) - Lo(EURUSD$OA.MID)
gbp.spreads = Hi(GBPUSD$OA.MID) - Lo(GBPUSD$OA.MID)
nzd.spreads = Hi(NZDUSD$OA.MID) - Lo(NZDUSD$OA.MID)

cols = c('AUDUSD', 'USDSEK', 'USDCZK', 'USDNOK', 'USDSGD', 'USDTRY', 'USDJPY', 'EURUSD', 'GBPUSD', 'NZDUSD')
portf = na.omit(merge(aud.ret, sek.ret, czk.ret, nok.ret, sgd.ret, try.ret, jpy.ret
                      , eur.ret, gbp.ret, nzd.ret)); names(portf) = cols
prices = na.omit(merge(aud, sek, czk, nok, sgd, try, jpy, eur, gbp, nzd)); names(prices) = cols
cur.spreads = na.omit(merge(aud.spreads, sek.spreads, czk.spreads, nok.spreads, sgd.spreads, try.spreads, jpy.spreads
                            , eur.spreads, gbp.spreads, nzd.spreads)); names(cur.spreads) = cols


# 2. Configuration --------------------------------------------------------
order.book.file = '~/ivanliu2_dollar_neutral_001.csv'
log.file = paste0('~/ivanliu2_dollar_neutral_', as.Date(Sys.time()), '.txt')
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
stoploss = -0.10
sd.lookback = 21
ACCOUNT_ID = '101-011-4686012-001'

sink(file=log.file)
while(TRUE){

    latestPrices = getLatestPrices()
    lastDate = as.Date(index(tail(prices, 1)))

    if(latestPrices$update.date > lastDate){

        cat(paste0('\nNew Event Found! Start trading with ', lastDate, '............'))
        portf = rbind(portf, latestPrices$portf.new)
        prices = rbind(prices, latestPrices$prices.new)
        cur.spreads = rbind(cur.spreads, latestPrices$cur.spreads)

        r = nrow(prices)
        goLong = FALSE
        goShort = FALSE
        closePos = FALSE

        # Model data
        tmp.prices = prices[(r-johansen.lookback):r, cols]


        # 3. Statistics Test ------------------------------------------------------
        # stats tests
        jc.res=JohansenCointegrationTest(tmp.prices, type = "eigen", ecdet = 'const', K = 2)
        ols.fit = lm(AUDUSD~. , tmp.prices)
        ols.r2 = summary(ols.fit)$r.squared
        ols.pvalues = summary(ols.fit)[[4]][,4]

        # Spreads
        hedgeRatio = jc.res$jc.test@V[1:(nrow(jc.res$jc.test@V)-1), 1]
        spreads = tmp.prices %*% t(data.frame(hedgeRatio))[1:nfx]
        spreads = as.xts(spreads, order.by = index(tmp.prices))
        spreads = na.omit(spreads)

        # stats tests 2
        adf.res=adf.test(spreads) #0.06999
        hurst.res=as.numeric(tail(HurstExponentTest(spreads, hurst.lookback)$hurstKY, 1)) # > 0.5 trending | < 0.5 mean reverting
        half.life <- HalfLifeMeanReversion(spreads)$half.life.round

        # mean reversion signals
        adf.signal=adf.res$p.value
        hurst.signal=hurst.res
        jc.signal=as.numeric(((jc.res$p.value[1]-jc.res$r.1)/((jc.res$p.value[3]-jc.res$p.value[1])/10)+10)/100)


        # 4. Mean Reversion -------------------------------------------------------
        # !!! Normalisation
        # hedge ratio
        zScore=tail(zscores(spreads),1)
        numUnits=-tail(zScore,1)
        hedgeRatio = t(data.frame(hedgeRatio))
        # dollar neutrality
        sizing = abs(numUnits)/1
        posUnits = initAssets * posiRate
        dollars = round(as.numeric(posUnits * sizing))
        dollar.neut = hedgeRatio * dollars/abs(sum(hedgeRatio))
        # positions
        last.price = tail(tmp.prices,1)
        positions = as.data.frame(round(dollar.neut / tail(last.price,1)))
        names(positions) = paste0('POS.', cols)


        # 5. Risk Management ------------------------------------------------------
        AUD.sd = sd(tail(cur.spreads$AUDUSD, sd.lookback))
        SEK.sd = sd(tail(cur.spreads$USDSEK, sd.lookback))
        CZK.sd = sd(tail(cur.spreads$USDCZK, sd.lookback))
        NOK.sd = sd(tail(cur.spreads$USDNOK, sd.lookback))
        SGD.sd = sd(tail(cur.spreads$USDSGD, sd.lookback))
        TRY.sd = sd(tail(cur.spreads$USDTRY, sd.lookback))
        JPY.sd = sd(tail(cur.spreads$USDJPY, sd.lookback))
        EUR.sd = sd(tail(cur.spreads$EURUSD, sd.lookback))
        GBP.sd = sd(tail(cur.spreads$GBPUSD, sd.lookback))
        NZD.sd = sd(tail(cur.spreads$NZDUSD, sd.lookback))


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
        }else if(abs(zScore) < 0.1 | adf.signal < adf.threshold){ # | unrealizedPnL <= stoploss
            goLong = FALSE
            goShort = FALSE
            closePos = TRUE # sell OR buy
        }

        # Positions ---------------------------------------------------------------
        if(goLong){
            POS.AUDUSD = positions$POS.AUDUSD
            POS.USDSEK = round(-positions$POS.USDSEK/(1/last.price$USDSEK))
            POS.USDCZK = round(-positions$POS.USDCZK/(1/last.price$USDCZK))
            POS.USDNOK = round(-positions$POS.USDNOK/(1/last.price$USDNOK))
            POS.USDSGD = round(-positions$POS.USDSGD/(1/last.price$USDSGD))
            POS.USDTRY = round(-positions$POS.USDTRY/(1/last.price$USDTRY))
            POS.USDJPY = round(-positions$POS.USDJPY/(1/last.price$USDJPY))
            POS.EURUSD = positions$POS.EURUSD
            POS.GBPUSD = positions$POS.GBPUSD
            POS.NZDUSD = positions$POS.NZDUSD

            # Execution ---------------------------------------------------------------
            aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD)
            sek.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SEK', UNITS = POS.USDSEK)
            czk.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CZK', UNITS = POS.USDCZK)
            nok.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_NOK', UNITS = POS.USDNOK)
            sgd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SGD', UNITS = POS.USDSGD)
            try.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_TRY', UNITS = POS.USDTRY)
            jpy.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_JPY', UNITS = POS.USDJPY)
            eur.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'EUR_USD', UNITS = POS.EURUSD)
            gbp.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'GBP_USD', UNITS = POS.GBPUSD)
            nzd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'NZD_USD', UNITS = POS.NZDUSD)

            inLong = TRUE
            inShort = FALSE

        }else if(goShort){
            POS.AUDUSD = -positions$POS.AUDUSD
            POS.USDSEK = -round(-positions$POS.USDSEK/(1/last.price$USDSEK))
            POS.USDCZK = -round(-positions$POS.USDCZK/(1/last.price$USDCZK))
            POS.USDNOK = -round(-positions$POS.USDNOK/(1/last.price$USDNOK))
            POS.USDSGD = -round(-positions$POS.USDSGD/(1/last.price$USDSGD))
            POS.USDTRY = -round(-positions$POS.USDTRY/(1/last.price$USDTRY))
            POS.USDJPY = -round(-positions$POS.USDJPY/(1/last.price$USDJPY))
            POS.EURUSD = -positions$POS.EURUSD
            POS.GBPUSD = -positions$POS.GBPUSD
            POS.NZDUSD = -positions$POS.NZDUSD

            # Execution ---------------------------------------------------------------
            aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD)
            sek.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SEK', UNITS = POS.USDSEK)
            czk.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CZK', UNITS = POS.USDCZK)
            nok.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_NOK', UNITS = POS.USDNOK)
            sgd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SGD', UNITS = POS.USDSGD)
            try.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_TRY', UNITS = POS.USDTRY)
            jpy.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_JPY', UNITS = POS.USDJPY)
            eur.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'EUR_USD', UNITS = POS.EURUSD)
            gbp.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'GBP_USD', UNITS = POS.GBPUSD)
            nzd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'NZD_USD', UNITS = POS.NZDUSD)

            latestTradeID = getLatestTradeID()

            inLong = FALSE
            inShort = TRUE

        }else if(closePos & (inLong | inShort)){

            CurrentPositions = setDT(getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID)$positions)

            POS.AUDUSD = -sum(as.numeric(CurrentPositions[instrument == 'AUD_USD', .(long.units,short.units)]))
            POS.USDSEK = -sum(as.numeric(CurrentPositions[instrument == 'USD_SEK', .(long.units,short.units)]))
            POS.USDCZK = -sum(as.numeric(CurrentPositions[instrument == 'USD_CZK', .(long.units,short.units)]))
            POS.USDNOK = -sum(as.numeric(CurrentPositions[instrument == 'USD_NOK', .(long.units,short.units)]))
            POS.USDSGD = -sum(as.numeric(CurrentPositions[instrument == 'USD_SGD', .(long.units,short.units)]))
            POS.USDTRY = -sum(as.numeric(CurrentPositions[instrument == 'USD_TRY', .(long.units,short.units)]))
            POS.USDJPY = -sum(as.numeric(CurrentPositions[instrument == 'USD_JPY', .(long.units,short.units)]))
            POS.EURUSD = -sum(as.numeric(CurrentPositions[instrument == 'EUR_USD', .(long.units,short.units)]))
            POS.GBPUSD = -sum(as.numeric(CurrentPositions[instrument == 'GBP_USD', .(long.units,short.units)]))
            POS.NZDUSD = -sum(as.numeric(CurrentPositions[instrument == 'NZD_USD', .(long.units,short.units)]))

            # Execution ---------------------------------------------------------------
            aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD)
            sek.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SEK', UNITS = POS.USDSEK)
            czk.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CZK', UNITS = POS.USDCZK)
            nok.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_NOK', UNITS = POS.USDNOK)
            sgd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SGD', UNITS = POS.USDSGD)
            try.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_TRY', UNITS = POS.USDTRY)
            jpy.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_JPY', UNITS = POS.USDJPY)
            eur.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'EUR_USD', UNITS = POS.EURUSD)
            gbp.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'GBP_USD', UNITS = POS.GBPUSD)
            nzd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'NZD_USD', UNITS = POS.NZDUSD)


            inLong = FALSE
            inShort = FALSE
        }

        # Risk Management ---------------------------------------------------------
        if(goShort | goLong){
            latestTradeID = getLatestTradeID()
            aud.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$aud.tradeid,
                                              PRICE = as.numeric(last.price$AUDUSD + 1.5 * sign(POS.AUDUSD) * AUD.sd), ORDERTYPE = 'TAKE_PROFIT')
            aud.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$aud.tradeid,
                                              PRICE = as.numeric(last.price$AUDUSD - 1.5 * sign(POS.AUDUSD) * AUD.sd), ORDERTYPE = 'STOP_LOSS')
            sek.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$sek.tradeid,
                                              PRICE = as.numeric(1/last.price$USDSEK + 1.5 * sign(POS.USDSEK) * SEK.sd), ORDERTYPE = 'TAKE_PROFIT')
            sek.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$sek.tradeid,
                                              PRICE = as.numeric(1/last.price$USDSEK - 1.5 * sign(POS.USDSEK) * SEK.sd), ORDERTYPE = 'STOP_LOSS')
            czk.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$czk.tradeid,
                                              PRICE = as.numeric(1/last.price$USDCZK + 1.5 * sign(POS.USDCZK) * CZK.sd), ORDERTYPE = 'TAKE_PROFIT')
            czk.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$czk.tradeid,
                                              PRICE = as.numeric(1/last.price$USDCZK - 1.5 * sign(POS.USDCZK) * CZK.sd), ORDERTYPE = 'STOP_LOSS')
            nok.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$nok.tradeid,
                                              PRICE = as.numeric(1/last.price$USDNOK + 1.5 * sign(POS.USDNOK) * NOK.sd), ORDERTYPE = 'TAKE_PROFIT')
            nok.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$nok.tradeid,
                                              PRICE = as.numeric(1/last.price$USDNOK - 1.5 * sign(POS.USDNOK) * NOK.sd), ORDERTYPE = 'STOP_LOSS')
            sgd.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$sgd.tradeid,
                                              PRICE = as.numeric(1/last.price$USDSGD + 1.5 * sign(POS.USDSGD) * SGD.sd), ORDERTYPE = 'TAKE_PROFIT')
            sgd.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$sgd.tradeid,
                                              PRICE = as.numeric(1/last.price$USDSGD - 1.5 * sign(POS.USDSGD) * SGD.sd), ORDERTYPE = 'STOP_LOSS')
            try.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$try.tradeid,
                                              PRICE = as.numeric(1/last.price$USDTRY + 1.5 * sign(POS.USDTRY) * TRY.sd), ORDERTYPE = 'TAKE_PROFIT')
            try.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$try.tradeid,
                                              PRICE = as.numeric(1/last.price$USDTRY - 1.5 * sign(POS.USDTRY) * TRY.sd), ORDERTYPE = 'STOP_LOSS')
            jpy.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$jpy.tradeid,
                                              PRICE = as.numeric(1/last.price$USDJPY + 1.5 * sign(POS.USDJPY) * JPY.sd), ORDERTYPE = 'TAKE_PROFIT')
            jpy.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$jpy.tradeid,
                                              PRICE = as.numeric(1/last.price$USDJPY - 1.5 * sign(POS.USDJPY) * JPY.sd), ORDERTYPE = 'STOP_LOSS')
            eur.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$eur.tradeid,
                                              PRICE = as.numeric(last.price$EURUSD + 1.5 * sign(POS.EURUSD) * EUR.sd), ORDERTYPE = 'TAKE_PROFIT')
            eur.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$eur.tradeid,
                                              PRICE = as.numeric(last.price$EURUSD - 1.5 * sign(POS.EURUSD) * EUR.sd), ORDERTYPE = 'STOP_LOSS')
            gbp.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$gbp.tradeid,
                                              PRICE = as.numeric(last.price$GBPUSD + 1.5 * sign(POS.GBPUSD) * GBP.sd), ORDERTYPE = 'TAKE_PROFIT')
            gbp.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$gbp.tradeid,
                                              PRICE = as.numeric(last.price$GBPUSD - 1.5 * sign(POS.GBPUSD) * GBP.sd), ORDERTYPE = 'STOP_LOSS')
            nzd.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$nzd.tradeid,
                                              PRICE = as.numeric(last.price$NZDUSD + 1.5 * sign(POS.NZDUSD) * NZD.sd), ORDERTYPE = 'TAKE_PROFIT')
            nzd.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$nzd.tradeid,
                                              PRICE = as.numeric(last.price$NZDUSD - 1.5 * sign(POS.NZDUSD) * NZD.sd), ORDERTYPE = 'STOP_LOSS')
        }

        # Re-check positions
        CurrentPositions = setDT(getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID)$positions)
        POS.AUDUSD = sum(as.numeric(CurrentPositions[instrument == 'AUD_USD', .(long.units,short.units)]))
        POS.USDSEK = sum(as.numeric(CurrentPositions[instrument == 'USD_SEK', .(long.units,short.units)]))
        POS.USDCZK = sum(as.numeric(CurrentPositions[instrument == 'USD_CZK', .(long.units,short.units)]))
        POS.USDNOK = sum(as.numeric(CurrentPositions[instrument == 'USD_NOK', .(long.units,short.units)]))
        POS.USDSGD = sum(as.numeric(CurrentPositions[instrument == 'USD_SGD', .(long.units,short.units)]))
        POS.USDTRY = sum(as.numeric(CurrentPositions[instrument == 'USD_TRY', .(long.units,short.units)]))
        POS.USDJPY = sum(as.numeric(CurrentPositions[instrument == 'USD_JPY', .(long.units,short.units)]))
        POS.EURUSD = sum(as.numeric(CurrentPositions[instrument == 'EUR_USD', .(long.units,short.units)]))
        POS.GBPUSD = sum(as.numeric(CurrentPositions[instrument == 'GBP_USD', .(long.units,short.units)]))
        POS.NZDUSD = sum(as.numeric(CurrentPositions[instrument == 'NZD_USD', .(long.units,short.units)]))


        # Order book --------------------------------------------------------------
        OB.PRICES = last.price; OB.PRICES[, 2:7] = 1/OB.PRICES[, 2:7]
        names(ols.pvalues) = paste0('OLS.P.', names(ols.pvalues))
        unrealizedPnL = sum(as.numeric(CurrentPositions$unrealizedPL))
        PnL = sum(as.numeric(CurrentPositions$pl))

        orderBook = data.frame(as.character(index(OB.PRICES)), OB.PRICES, adf.signal, jc.signal, hurst.signal, half.life, zScore, ols.r2, t(ols.pvalues),
                               POS.AUDUSD, POS.USDSEK, POS.USDCZK, POS.USDNOK, POS.USDSGD, POS.USDTRY, POS.USDJPY, POS.EURUSD, POS.GBPUSD, POS.NZDUSD,
                               inLong, inShort, goLong, goShort, closePos, unrealizedPnL, PnL)
        names(orderBook) = c('DateTime', paste0('P.', names(OB.PRICES)), 'ADF', 'Johansen', 'Hurst', 'H.L', 'ZScore', 'ols.r2', names(ols.pvalues),
                             'POS.AUDUSD', 'POS.USDSEK', 'POS.USDCZK', 'POS.USDNOK', 'POS.USDSGD', 'POS.USDTRY', 'POS.USDJPY', 'POS.EURUSD', 'POS.GBPUSD', 'POS.NZDUSD',
                             'inLong', 'inShort', 'goLong', 'goShort', 'closePos', 'unrealizedPnL', 'PnL')
        setDT(orderBook)

        if(file.exists(order.book.file)){
            Order.Book = fread(order.book.file)
            Order.Book = rbind(Order.Book, orderBook)
            colnames(orderBook) = colnames(Order.Book)
            write.csv(Order.Book, file = order.book.file, row.names = FALSE)
        }else{
            write.csv(orderBook, file = order.book.file, row.names = FALSE)
        }

    }

    Sys.sleep(1)
}
sink()
