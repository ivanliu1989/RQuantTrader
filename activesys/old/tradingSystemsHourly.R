######################################################
### OANDA MEAN REVERSION PORTFOLIO -- HOURLY BASIS ###
######################################################
# Lookback: 340
rm(list=ls());gc()
loadQuantPackages()
Sys.setenv(TZ='US/Eastern')

getLatestPrices = function(){
    # Sunday approximately 5 p.m. to Friday 5 p.m
    AUDUSD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "AUD", Cur2 = "USD", oanda.granularity = 'H1')
    USDMXN = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "MXN", oanda.granularity = 'H1')
    USDSEK = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "SEK", oanda.granularity = 'H1')
    USDTHB = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "THB", oanda.granularity = 'H1')
    USDZAR = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "ZAR", oanda.granularity = 'H1')
    USDHKD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "HKD", oanda.granularity = 'H1')
    USDSGD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "SGD", oanda.granularity = 'H1')
    USDJPY = prepareForexOandaPrices(oanda.count = 2, Cur1 = "USD", Cur2 = "JPY", oanda.granularity = 'H1')
    GBPUSD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "GBP", Cur2 = "USD", oanda.granularity = 'H1')
    NZDUSD = prepareForexOandaPrices(oanda.count = 2, Cur1 = "NZD", Cur2 = "USD", oanda.granularity = 'H1')

    # 1. Price model ----------------------------------------------------------
    aud = Cl(AUDUSD$OA.MID); #index(aud) = as.Date(index(aud))
    mxn = Cl(1/USDMXN$OA.MID); #index(huf) = as.Date(index(huf))
    sek = Cl(1/USDSEK$OA.MID); #index(huf) = as.Date(index(huf))
    thb = Cl(1/USDTHB$OA.MID); #index(huf) = as.Date(index(huf))
    zar = Cl(1/USDZAR$OA.MID); #index(huf) = as.Date(index(huf))
    hkd = Cl(1/USDHKD$OA.MID); #index(huf) = as.Date(index(huf))
    sgd = Cl(1/USDSGD$OA.MID); #index(huf) = as.Date(index(huf))
    jpy = Cl(1/USDJPY$OA.MID); #index(huf) = as.Date(index(huf))
    gbp = Cl(GBPUSD$OA.MID); #index(huf) = as.Date(index(huf))
    nzd = Cl(NZDUSD$OA.MID); #index(huf) = as.Date(index(huf))

    aud.ret = log10(Cl(AUDUSD$OA.MID)/Op(AUDUSD$OA.MID))
    mxn.ret = log10(Cl(1/USDMXN$OA.MID)/Op(1/USDMXN$OA.MID))
    sek.ret = log10(Cl(1/USDSEK$OA.MID)/Op(1/USDSEK$OA.MID))
    thb.ret = log10(Cl(1/USDTHB$OA.MID)/Op(1/USDTHB$OA.MID))
    zar.ret = log10(Cl(1/USDZAR$OA.MID)/Op(1/USDZAR$OA.MID))
    hkd.ret = log10(Cl(1/USDHKD$OA.MID)/Op(1/USDHKD$OA.MID))
    sgd.ret = log10(Cl(1/USDSGD$OA.MID)/Op(1/USDSGD$OA.MID))
    jpy.ret = log10(Cl(1/USDJPY$OA.MID)/Op(1/USDJPY$OA.MID))
    gbp.ret = log10(Cl(GBPUSD$OA.MID)/Op(GBPUSD$OA.MID))
    nzd.ret = log10(Cl(NZDUSD$OA.MID)/Op(NZDUSD$OA.MID))

    aud.spreads = Hi(AUDUSD$OA.MID) - Lo(AUDUSD$OA.MID)
    mxn.spreads = Hi(USDMXN$OA.MID) - Lo(USDMXN$OA.MID)
    sek.spreads = Hi(USDSEK$OA.MID) - Lo(USDSEK$OA.MID)
    thb.spreads = Hi(USDTHB$OA.MID) - Lo(USDTHB$OA.MID)
    zar.spreads = Hi(USDZAR$OA.MID) - Lo(USDZAR$OA.MID)
    hkd.spreads = Hi(USDHKD$OA.MID) - Lo(USDHKD$OA.MID)
    sgd.spreads = Hi(USDSGD$OA.MID) - Lo(USDSGD$OA.MID)
    jpy.spreads = Hi(USDJPY$OA.MID) - Lo(USDJPY$OA.MID)
    gbp.spreads = Hi(GBPUSD$OA.MID) - Lo(GBPUSD$OA.MID)
    nzd.spreads = Hi(NZDUSD$OA.MID) - Lo(NZDUSD$OA.MID)

    cols = c('AUDUSD', 'USDMXN', 'USDSEK', 'USDTHB', 'USDZAR', 'USDHKD', 'USDSGD', 'USDJPY', 'GBPUSD', 'NZDUSD')
    portf = na.omit(merge(aud.ret, mxn.ret, sek.ret, thb.ret, zar.ret
                          , hkd.ret, sgd.ret, jpy.ret, gbp.ret, nzd.ret)); names(portf) = cols
    prices = na.omit(merge(aud, mxn, sek, thb, zar, hkd, sgd, jpy, gbp, nzd)); names(prices) = cols
    cur.spreads = na.omit(merge(aud.spreads, mxn.spreads, sek.spreads, thb.spreads, zar.spreads
                                , hkd.spreads, sgd.spreads, jpy.spreads, gbp.spreads, nzd.spreads)); names(cur.spreads) = cols

    return(list(portf.new = tail(portf,1),
                prices.new = tail(prices,1),
                cur.spreads = tail(cur.spreads,1),
                update.date = (index(tail(prices,1)))))
}

getLatestTradeID = function(){

    return(list(
        aud.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('AUD_USD'))$trades$id[1],
        mxn.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_MXN'))$trades$id[1],
        sek.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_SEK'))$trades$id[1],
        thb.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_THB'))$trades$id[1],
        zar.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_ZAR'))$trades$id[1],
        hkd.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_HKD'))$trades$id[1],
        sgd.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_SGD'))$trades$id[1],
        jpy.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('USD_JPY'))$trades$id[1],
        gbp.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('GBP_USD'))$trades$id[1],
        nzd.tradeid = getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('NZD_USD'))$trades$id[1]
    ))
}

# Sunday approximately 5 p.m. to Friday 5 p.m
AUDUSD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "AUD", Cur2 = "USD", oanda.granularity = 'H1')
USDMXN = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "MXN", oanda.granularity = 'H1')
USDSEK = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "SEK", oanda.granularity = 'H1')
USDTHB = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "THB", oanda.granularity = 'H1')
USDZAR = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "ZAR", oanda.granularity = 'H1')
USDHKD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "HKD", oanda.granularity = 'H1')
USDSGD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "SGD", oanda.granularity = 'H1')
USDJPY = prepareForexOandaPrices(oanda.count = 500, Cur1 = "USD", Cur2 = "JPY", oanda.granularity = 'H1')
GBPUSD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "GBP", Cur2 = "USD", oanda.granularity = 'H1')
NZDUSD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "NZD", Cur2 = "USD", oanda.granularity = 'H1')

# 1. Price model ----------------------------------------------------------
aud = Cl(AUDUSD$OA.MID); #index(aud) = as.Date(index(aud))
mxn = Cl(1/USDMXN$OA.MID); #index(huf) = as.Date(index(huf))
sek = Cl(1/USDSEK$OA.MID); #index(huf) = as.Date(index(huf))
thb = Cl(1/USDTHB$OA.MID); #index(huf) = as.Date(index(huf))
zar = Cl(1/USDZAR$OA.MID); #index(huf) = as.Date(index(huf))
hkd = Cl(1/USDHKD$OA.MID); #index(huf) = as.Date(index(huf))
sgd = Cl(1/USDSGD$OA.MID); #index(huf) = as.Date(index(huf))
jpy = Cl(1/USDJPY$OA.MID); #index(huf) = as.Date(index(huf))
gbp = Cl(GBPUSD$OA.MID); #index(huf) = as.Date(index(huf))
nzd = Cl(NZDUSD$OA.MID); #index(huf) = as.Date(index(huf))

aud.ret = log10(Cl(AUDUSD$OA.MID)/Op(AUDUSD$OA.MID))
mxn.ret = log10(Cl(1/USDMXN$OA.MID)/Op(1/USDMXN$OA.MID))
sek.ret = log10(Cl(1/USDSEK$OA.MID)/Op(1/USDSEK$OA.MID))
thb.ret = log10(Cl(1/USDTHB$OA.MID)/Op(1/USDTHB$OA.MID))
zar.ret = log10(Cl(1/USDZAR$OA.MID)/Op(1/USDZAR$OA.MID))
hkd.ret = log10(Cl(1/USDHKD$OA.MID)/Op(1/USDHKD$OA.MID))
sgd.ret = log10(Cl(1/USDSGD$OA.MID)/Op(1/USDSGD$OA.MID))
jpy.ret = log10(Cl(1/USDJPY$OA.MID)/Op(1/USDJPY$OA.MID))
gbp.ret = log10(Cl(GBPUSD$OA.MID)/Op(GBPUSD$OA.MID))
nzd.ret = log10(Cl(NZDUSD$OA.MID)/Op(NZDUSD$OA.MID))

aud.spreads = Hi(AUDUSD$OA.MID) - Lo(AUDUSD$OA.MID)
mxn.spreads = Hi(USDMXN$OA.MID) - Lo(USDMXN$OA.MID)
sek.spreads = Hi(USDSEK$OA.MID) - Lo(USDSEK$OA.MID)
thb.spreads = Hi(USDTHB$OA.MID) - Lo(USDTHB$OA.MID)
zar.spreads = Hi(USDZAR$OA.MID) - Lo(USDZAR$OA.MID)
hkd.spreads = Hi(USDHKD$OA.MID) - Lo(USDHKD$OA.MID)
sgd.spreads = Hi(USDSGD$OA.MID) - Lo(USDSGD$OA.MID)
jpy.spreads = Hi(USDJPY$OA.MID) - Lo(USDJPY$OA.MID)
gbp.spreads = Hi(GBPUSD$OA.MID) - Lo(GBPUSD$OA.MID)
nzd.spreads = Hi(NZDUSD$OA.MID) - Lo(NZDUSD$OA.MID)

cols = c('AUDUSD', 'USDMXN', 'USDSEK', 'USDTHB', 'USDZAR', 'USDHKD', 'USDSGD', 'USDJPY', 'GBPUSD', 'NZDUSD')
portf = na.omit(merge(aud.ret, mxn.ret, sek.ret, thb.ret, zar.ret
                      , hkd.ret, sgd.ret, jpy.ret, gbp.ret, nzd.ret)); names(portf) = cols
prices = na.omit(merge(aud, mxn, sek, thb, zar, hkd, sgd, jpy, gbp, nzd)); names(prices) = cols
cur.spreads = na.omit(merge(aud.spreads, mxn.spreads, sek.spreads, thb.spreads, zar.spreads
                            , hkd.spreads, sgd.spreads, jpy.spreads, gbp.spreads, nzd.spreads)); names(cur.spreads) = cols


# 2. Configuration --------------------------------------------------------
order.book.file = '~/ivanliu2_dollar_neutral_hourly_002.csv'
log.file = paste0('~/ivanliu2_dollar_neutral_hourly', as.Date(Sys.time()), '.txt')
inLong = FALSE
inShort = FALSE
initAssets = 1000000
posiRate = 0.05
johansen.lookback = 95
ols.lookback = 95
hurst.lookback = 21
nfx = 10
threshold = 1.0
adf.threshold = 0.7
jc.threshold = 0.7
hurst.threshold = 0.8
stoploss = -0.10
sd.lookback = 48
ACCOUNT_ID = '101-011-4686012-003'
sl_tp.threshold = 2

sink(file=log.file)
while(TRUE){

    latestPrices = getLatestPrices()
    lastDate = (index(tail(prices, 1)))

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
        MXN.sd = sd(tail(cur.spreads$USDMXN, sd.lookback))
        SEK.sd = sd(tail(cur.spreads$USDSEK, sd.lookback))
        THB.sd = sd(tail(cur.spreads$USDTHB, sd.lookback))
        ZAR.sd = sd(tail(cur.spreads$USDZAR, sd.lookback))
        HKD.sd = sd(tail(cur.spreads$USDHKD, sd.lookback))
        SGD.sd = sd(tail(cur.spreads$USDSGD, sd.lookback))
        JPY.sd = sd(tail(cur.spreads$USDJPY, sd.lookback))
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
            POS.USDMXN = round(-positions$POS.USDMXN/(1/last.price$USDMXN))
            POS.USDSEK = round(-positions$POS.USDSEK/(1/last.price$USDSEK))
            POS.USDTHB = round(-positions$POS.USDTHB/(1/last.price$USDTHB))
            POS.USDZAR = round(-positions$POS.USDZAR/(1/last.price$USDZAR))
            POS.USDHKD = round(-positions$POS.USDHKD/(1/last.price$USDHKD))
            POS.USDSGD = round(-positions$POS.USDSGD/(1/last.price$USDSGD))
            POS.USDJPY = round(-positions$POS.USDJPY/(1/last.price$USDJPY))
            POS.GBPUSD = positions$POS.GBPUSD
            POS.NZDUSD = positions$POS.NZDUSD

            # Execution ---------------------------------------------------------------
            aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD)
            mxn.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_MXN', UNITS = POS.USDMXN)
            sek.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SEK', UNITS = POS.USDSEK)
            thb.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_THB', UNITS = POS.USDTHB)
            zar.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_ZAR', UNITS = POS.USDZAR)
            hkd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_HKD', UNITS = POS.USDHKD)
            sgd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SGD', UNITS = POS.USDSGD)
            jpy.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_JPY', UNITS = POS.USDJPY)
            gbp.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'GBP_USD', UNITS = POS.GBPUSD)
            nzd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'NZD_USD', UNITS = POS.NZDUSD)

            inLong = TRUE
            inShort = FALSE
            cat('\nGo Long! AUDUSD Raw Position: ', positions$POS.AUDUSD, ' | AUDUSD New Position: ', POS.AUDUSD)

        }else if(goShort){

            POS.AUDUSD = -positions$POS.AUDUSD
            POS.USDMXN = -round(-positions$POS.USDMXN/(1/last.price$USDMXN))
            POS.USDSEK = -round(-positions$POS.USDSEK/(1/last.price$USDSEK))
            POS.USDTHB = -round(-positions$POS.USDTHB/(1/last.price$USDTHB))
            POS.USDZAR = -round(-positions$POS.USDZAR/(1/last.price$USDZAR))
            POS.USDHKD = -round(-positions$POS.USDHKD/(1/last.price$USDHKD))
            POS.USDSGD = -round(-positions$POS.USDSGD/(1/last.price$USDSGD))
            POS.USDJPY = -round(-positions$POS.USDJPY/(1/last.price$USDJPY))
            POS.GBPUSD = -positions$POS.GBPUSD
            POS.NZDUSD = -positions$POS.NZDUSD

            # Execution ---------------------------------------------------------------
            aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD)
            mxn.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_MXN', UNITS = POS.USDMXN)
            sek.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SEK', UNITS = POS.USDSEK)
            thb.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_THB', UNITS = POS.USDTHB)
            zar.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_ZAR', UNITS = POS.USDZAR)
            hkd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_HKD', UNITS = POS.USDHKD)
            sgd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SGD', UNITS = POS.USDSGD)
            jpy.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_JPY', UNITS = POS.USDJPY)
            gbp.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'GBP_USD', UNITS = POS.GBPUSD)
            nzd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'NZD_USD', UNITS = POS.NZDUSD)

            inLong = FALSE
            inShort = TRUE
            cat('\nGo Short! AUDUSD Raw Position: ', positions$POS.AUDUSD, ' | AUDUSD New Position: ', POS.AUDUSD)

        }else if(closePos & (inLong | inShort)){

            CurrentPositions = setDT(getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID)$positions)

            POS.AUDUSD = -sum(as.numeric(CurrentPositions[instrument == 'AUD_USD', .(long.units,short.units)]))
            POS.USDMXN = -sum(as.numeric(CurrentPositions[instrument == 'USD_MXN', .(long.units,short.units)]))
            POS.USDSEK = -sum(as.numeric(CurrentPositions[instrument == 'USD_SEK', .(long.units,short.units)]))
            POS.USDTHB = -sum(as.numeric(CurrentPositions[instrument == 'USD_THB', .(long.units,short.units)]))
            POS.USDZAR = -sum(as.numeric(CurrentPositions[instrument == 'USD_ZAR', .(long.units,short.units)]))
            POS.USDHKD = -sum(as.numeric(CurrentPositions[instrument == 'USD_HKD', .(long.units,short.units)]))
            POS.USDSGD = -sum(as.numeric(CurrentPositions[instrument == 'USD_SGD', .(long.units,short.units)]))
            POS.USDJPY = -sum(as.numeric(CurrentPositions[instrument == 'USD_JPY', .(long.units,short.units)]))
            POS.GBPUSD = -sum(as.numeric(CurrentPositions[instrument == 'GBP_USD', .(long.units,short.units)]))
            POS.NZDUSD = -sum(as.numeric(CurrentPositions[instrument == 'NZD_USD', .(long.units,short.units)]))

            # Execution ---------------------------------------------------------------
            aud.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = POS.AUDUSD)
            mxn.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_MXN', UNITS = POS.USDMXN)
            sek.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SEK', UNITS = POS.USDSEK)
            thb.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_THB', UNITS = POS.USDTHB)
            zar.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_ZAR', UNITS = POS.USDZAR)
            hkd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_HKD', UNITS = POS.USDHKD)
            sgd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_SGD', UNITS = POS.USDSGD)
            jpy.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_JPY', UNITS = POS.USDJPY)
            gbp.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'GBP_USD', UNITS = POS.GBPUSD)
            nzd.order.msg = createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'NZD_USD', UNITS = POS.NZDUSD)

            inLong = FALSE
            inShort = FALSE
            cat('\nClose Position! AUDUSD Raw Position: ', positions$POS.AUDUSD, ' | AUDUSD New Position: ', POS.AUDUSD)
        }


        if(goShort | goLong){
            latestTradeID = getLatestTradeID()
            aud.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$aud.tradeid,
                                              PRICE = as.numeric(last.price$AUDUSD + sl_tp.threshold * sign(POS.AUDUSD) * AUD.sd), ORDERTYPE = 'TAKE_PROFIT')
            aud.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$aud.tradeid,
                                              PRICE = as.numeric(last.price$AUDUSD - sl_tp.threshold * sign(POS.AUDUSD) * AUD.sd), ORDERTYPE = 'STOP_LOSS')
            mxn.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$mxn.tradeid,
                                              PRICE = as.numeric(1/last.price$USDMXN + sl_tp.threshold * sign(POS.USDMXN) * MXN.sd), ORDERTYPE = 'TAKE_PROFIT')
            mxn.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$mxn.tradeid,
                                              PRICE = as.numeric(1/last.price$USDMXN - sl_tp.threshold * sign(POS.USDMXN) * MXN.sd), ORDERTYPE = 'STOP_LOSS')
            sek.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$sek.tradeid,
                                              PRICE = as.numeric(1/last.price$USDSEK + sl_tp.threshold * sign(POS.USDSEK) * SEK.sd), ORDERTYPE = 'TAKE_PROFIT')
            sek.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$sek.tradeid,
                                              PRICE = as.numeric(1/last.price$USDSEK - sl_tp.threshold * sign(POS.USDSEK) * SEK.sd), ORDERTYPE = 'STOP_LOSS')
            thb.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$thb.tradeid,
                                              PRICE = as.numeric(1/last.price$USDTHB + sl_tp.threshold * sign(POS.USDTHB) * THB.sd), ORDERTYPE = 'TAKE_PROFIT')
            thb.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$thb.tradeid,
                                              PRICE = as.numeric(1/last.price$USDTHB - sl_tp.threshold * sign(POS.USDTHB) * THB.sd), ORDERTYPE = 'STOP_LOSS')
            zar.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$zar.tradeid,
                                              PRICE = as.numeric(1/last.price$USDZAR + sl_tp.threshold * sign(POS.USDZAR) * ZAR.sd), ORDERTYPE = 'TAKE_PROFIT')
            zar.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$zar.tradeid,
                                              PRICE = as.numeric(1/last.price$USDZAR - sl_tp.threshold * sign(POS.USDZAR) * ZAR.sd), ORDERTYPE = 'STOP_LOSS')
            hkd.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$hkd.tradeid,
                                              PRICE = as.numeric(1/last.price$USDHKD + sl_tp.threshold * sign(POS.USDHKD) * HKD.sd), ORDERTYPE = 'TAKE_PROFIT')
            hkd.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$hkd.tradeid,
                                              PRICE = as.numeric(1/last.price$USDHKD - sl_tp.threshold * sign(POS.USDHKD) * HKD.sd), ORDERTYPE = 'STOP_LOSS')
            sgd.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$sgd.tradeid,
                                              PRICE = as.numeric(1/last.price$USDSGD + sl_tp.threshold * sign(POS.USDSGD) * SGD.sd), ORDERTYPE = 'TAKE_PROFIT')
            sgd.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$sgd.tradeid,
                                              PRICE = as.numeric(1/last.price$USDSGD - sl_tp.threshold * sign(POS.USDSGD) * SGD.sd), ORDERTYPE = 'STOP_LOSS')
            jpy.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$jpy.tradeid,
                                              PRICE = as.numeric(1/last.price$USDJPY + sl_tp.threshold * sign(POS.USDJPY) * JPY.sd), ORDERTYPE = 'TAKE_PROFIT')
            jpy.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$jpy.tradeid,
                                              PRICE = as.numeric(1/last.price$USDJPY - sl_tp.threshold * sign(POS.USDJPY) * JPY.sd), ORDERTYPE = 'STOP_LOSS')
            gbp.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$gbp.tradeid,
                                              PRICE = as.numeric(last.price$GBPUSD + sl_tp.threshold * sign(POS.GBPUSD) * GBP.sd), ORDERTYPE = 'TAKE_PROFIT')
            gbp.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$gbp.tradeid,
                                              PRICE = as.numeric(last.price$GBPUSD - sl_tp.threshold * sign(POS.GBPUSD) * GBP.sd), ORDERTYPE = 'STOP_LOSS')
            nzd.tp.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$nzd.tradeid,
                                              PRICE = as.numeric(last.price$NZDUSD + sl_tp.threshold * sign(POS.NZDUSD) * NZD.sd), ORDERTYPE = 'TAKE_PROFIT')
            nzd.sl.msg = createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID, TRADEID = latestTradeID$nzd.tradeid,
                                              PRICE = as.numeric(last.price$NZDUSD - sl_tp.threshold * sign(POS.NZDUSD) * NZD.sd), ORDERTYPE = 'STOP_LOSS')
        }


        # Re-check positions
        CurrentPositions = setDT(getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, ACCOUNT_ID)$positions)
        POS.AUDUSD = sum(as.numeric(CurrentPositions[instrument == 'AUD_USD', .(long.units,short.units)]))
        POS.USDMXN = sum(as.numeric(CurrentPositions[instrument == 'USD_MXN', .(long.units,short.units)]))
        POS.USDSEK = sum(as.numeric(CurrentPositions[instrument == 'USD_SEK', .(long.units,short.units)]))
        POS.USDTHB = sum(as.numeric(CurrentPositions[instrument == 'USD_THB', .(long.units,short.units)]))
        POS.USDZAR = sum(as.numeric(CurrentPositions[instrument == 'USD_ZAR', .(long.units,short.units)]))
        POS.USDHKD = sum(as.numeric(CurrentPositions[instrument == 'USD_HKD', .(long.units,short.units)]))
        POS.USDSGD = sum(as.numeric(CurrentPositions[instrument == 'USD_SGD', .(long.units,short.units)]))
        POS.USDJPY = sum(as.numeric(CurrentPositions[instrument == 'USD_JPY', .(long.units,short.units)]))
        POS.GBPUSD = sum(as.numeric(CurrentPositions[instrument == 'GBP_USD', .(long.units,short.units)]))
        POS.NZDUSD = sum(as.numeric(CurrentPositions[instrument == 'NZD_USD', .(long.units,short.units)]))

        # Order book --------------------------------------------------------------
        OB.PRICES = last.price; OB.PRICES[, 2:7] = 1/OB.PRICES[, 2:7]
        names(ols.pvalues) = paste0('OLS.P.', names(ols.pvalues))
        unrealizedPnL = sum(as.numeric(CurrentPositions$unrealizedPL))
        PnL = sum(as.numeric(CurrentPositions$pl))

        orderBook = data.frame(as.character(index(OB.PRICES)), OB.PRICES, adf.signal, jc.signal, hurst.signal, half.life, zScore, ols.r2, t(ols.pvalues),
                               POS.AUDUSD, POS.USDMXN, POS.USDSEK, POS.USDTHB, POS.USDZAR, POS.USDHKD, POS.USDSGD, POS.USDJPY, POS.GBPUSD, POS.NZDUSD,
                               inLong, inShort, goLong, goShort, closePos, unrealizedPnL, PnL)
        names(orderBook) = c('DateTime', paste0('P.', names(OB.PRICES)), 'ADF', 'Johansen', 'Hurst', 'H.L', 'ZScore', 'ols.r2', names(ols.pvalues),
                             'POS.AUDUSD', 'POS.USDMXN', 'POS.USDSEK', 'POS.USDTHB', 'POS.USDZAR', 'POS.USDHKD', 'POS.USDSGD', 'POS.USDJPY', 'POS.GBPUSD', 'POS.NZDUSD',
                             'inLong', 'inShort', 'goLong', 'goShort', 'closePos', 'unrealizedPnL', 'PnL')
        setDT(orderBook)

        if(file.exists(order.book.file)){
            Order.Book = fread(order.book.file)
            colnames(orderBook) = colnames(Order.Book)
            Order.Book = rbind(Order.Book, orderBook)
            write.csv(Order.Book, file = order.book.file, row.names = FALSE)
        }else{
            write.csv(orderBook, file = order.book.file, row.names = FALSE)
        }

    }

    Sys.sleep(1)
}
sink()
