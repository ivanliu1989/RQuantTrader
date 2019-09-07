rm(list=ls());gc()
loadQuantPackages()
source('tests/AutomatedSystems/IBPaperTradingIndicators.R')
source('tests/h2o/h2oDeeplearningMain.R')
source('tests/caret/caretMachineLearningEnsemble.R')
Sys.setenv(TZ='US/Eastern')

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

AUDUSD = prepareForexOandaPrices(oanda.count = 2500, Cur1 = "AUD", Cur2 = "USD", oanda.granularity = 'D') # buy
USDCAD = prepareForexOandaPrices(oanda.count = 2500, Cur1 = "USD", Cur2 = "CAD", oanda.granularity = 'D') # sell
# EURUSD = prepareForexOandaPrices(oanda.count = 500, Cur1 = "EUR", Cur2 = "USD", oanda.granularity = 'D') # buy

aud = Cl(AUDUSD$OA.MID); index(aud) = as.Date(index(aud))
cad = Cl(1/USDCAD$OA.MID); index(cad) = as.Date(index(cad))
# eur = Cl(EURUSD$OA.MID); index(eur) = as.Date(index(eur))

aud.ret = log10(Cl(AUDUSD$OA.MID)/Op(AUDUSD$OA.MID))
cad.ret = log10(Cl(1/USDCAD$OA.MID)/Op(1/USDCAD$OA.MID))

aud.spreads = Hi(AUDUSD$OA.MID) - Lo(AUDUSD$OA.MID)
cad.spreads = Hi(USDCAD$OA.MID) - Lo(USDCAD$OA.MID)

cols = c('AUDUSD', 'USDCAD')
portf = na.omit(merge(aud.ret, cad.ret)); names(portf) = cols
prices = na.omit(merge(aud, cad)); names(prices) = cols
cur.spreads = na.omit(merge(aud.spreads, cad.spreads)); names(cur.spreads) = cols


# 2. Configurations -------------------------------------------------------
for(jl in c(65:85,335:350)){
    inLong = FALSE
    inShort = FALSE
    initAssets = 1000000
    posiRate = 0.05
    nfx = 2
    threshold = 1
    adf.threshold = 0.35
    jc.threshold = 0.35
    hurst.threshold = 0.8
    stoploss = -0.50
    obs = nrow(portf)
    train.length = 126
    backtest.n = 1800
    min.lookback = 21
    max.lookback = 63

    rnd = 0
    initPortf = c(rep(0,12), rep(FALSE, 5), 0, initAssets, 0, initAssets)
    initPortf = data.frame(t(initPortf))
    unrealizedPnL = 0

    for(r in (backtest.n+1):obs){

        goLong = FALSE
        goShort = FALSE
        closePos = FALSE

        # Model data --------------------------------------------------------------
        tmp.prices = prices[(r-train.length):r, cols]
        # spreads = tmp.prices[, 1] - tmp.prices[, 2]

        # stats tests
        # https://www.quantstart.com/articles/Johansen-Test-for-Cointegrating-Time-Series-Analysis-in-R
        jotest=ca.jo(log(tmp.prices), type="trace", K=2, ecdet="none", spec="longrun")
        jc.res = summary(jotest)
        ols.fit = lm(AUDUSD~. , log(tmp.prices))
        ols.r2 = summary(ols.fit)$r.squared
        ols.pvalues = summary(ols.fit)[[4]][,4]

        # Spreads
        hedgeRatio = as.numeric(jc.res@V[, 1])
        spreads = tmp.prices %*% hedgeRatio
        spreads = na.omit(as.xts(spreads, order.by = index(tmp.prices)))
        colnames(spreads) = 'Spreads'


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
        jc.signal= 0.1 / (jotest@teststat[1] / jotest@cval[1,1])


        # 4. Mean Reversion -------------------------------------------------------
        # hedge ratio
        spreads.MA <- MaRatio(spreads, look.back)
        spreads.SD <- Sd(spreads, look.back)
        Z.Score <- ZScore(spreads,spreads.MA,spreads.SD)
        # Z.Score <- zscores(spreads)
        # Z.Score <- zscores.ma(spreads, look.back, round(look.back/6))
        # par(mfcol = c(3,1))
        # chart_Series(zscores(spreads))
        # chart_Series(Z.Score)
        # chart_Series(zscores.ma(spreads, look.back, round(look.back/6)))

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


        # 5. Risk Management ------------------------------------------------------
        AUD.sd = sd(tail(cur.spreads$AUDUSD, look.back))
        CAD.sd = sd(tail(cur.spreads$USDCAD, look.back))


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
        if(r == (backtest.n+1)){
            lstRcd = initPortf
            lstPos = lstRcd[,9:10]
        }else{
            lstRcd = Order.Book[r-backtest.n,]
            lstPos = lstRcd[,9:10]
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
        actualValue = lstRcd[18] - sumMktValue

        unrealizedPnL = sum((data.frame(diff(tail(tmp.prices,2))[2,]) * lstPos)) / sum((data.frame(tail(tmp.prices,2)[1,]) * abs(lstPos)))
        unrealizedPnL = round(ifelse(is.nan(unrealizedPnL), 0, unrealizedPnL), 5)


        # PnL = sumMktValue + actualValue + unrealizedPnL
        PnL = round(sum((data.frame(diff(tail(tmp.prices,2))[2,]))) / sum((data.frame(tail(tmp.prices,2)[1,]))),5)

        orderBook = data.frame(last.price, adf.signal, jc.signal, hurst.signal, half.life.par, zScore, ols.r2,
                               pos.fnl, mkt.fnl, inLong, inShort, goLong, goShort, closePos,
                               sumMktValue, actualValue, unrealizedPnL, PnL)
        names(orderBook) = c(paste0('P.', names(tmp.prices)), 'ADF', 'Johansen', 'Hurst', 'H.L', 'ZScore', 'ols.r2',
                             paste0('Pos.', names(tmp.prices)), paste0('Mkt.', names(tmp.prices)),
                             'inLong', 'inShort', 'goLong', 'goShort', 'closePos',
                             'sumMktValue', 'actualValue', 'unrealizedPnL', 'PnL')
        if(r == (backtest.n+1)){
            names(initPortf) = names(orderBook)
            Order.Book = rbind(initPortf, orderBook)
        }else{
            Order.Book = rbind(Order.Book,orderBook)
        }

        cat(paste0('\n', unrealizedPnL * 100, ' | ', PnL * 100))
    }

    Order.Book = as.xts(Order.Book[-1,], order.by = as.Date(rownames(Order.Book[-1,])))
    # Order.Book = tail(Order.Book, 500)
    ret = Order.Book[,c('unrealizedPnL', 'PnL', 'ADF')]
    # hist(ret[,1], 100);
    # hist(ret[,2], 100)
    # quantile(ret[,1],probs = seq(0, 1, 0.25))
    cat(paste0('\nJC lookback: ', look.back, ' Score: ',sum(ret[,1]), '|', sum(ret[,2])))
    charts.PerformanceSummary(ret[, c(1,2)])
    # chart_Series(ret[,1])
    # sharpe
    # (mean(ret[,1]) * 252 - mean(ret[,2]) * 252)/ sd(ret[,1])
    # (mean(ret[ret[,1]!=0, 1]) * 252 - 0.02) / sd(ret[ret[,1]!=0,1])
    # (mean(ret[,1]) * 252 - 0.02) / sd(ret[,1])
}

# 246:233
# 0.57:0.565
# 0.6

table(Order.Book$unrealizedPnL > 0)
table(Order.Book$unrealizedPnL < 0)
sum(Order.Book$unrealizedPnL[Order.Book$unrealizedPnL > 0])
sum(Order.Book$unrealizedPnL[Order.Book$unrealizedPnL < 0])

table(Order.Book$PnL > 0)
table(Order.Book$PnL < 0)
sum(Order.Book$PnL[Order.Book$PnL > 0])
sum(Order.Book$PnL[Order.Book$PnL < 0])

# hurst0.7: 1.7 124

MainTable = na.omit(merge(aud, cad, ret[,1]))

par(mfcol = c(2,1))
chart_Series(tail(MainTable[,1:2], 252))
chart_Series(tail(MainTable[,3],252))


names(Order.Book)
cordata = Order.Book[Order.Book$unrealizedPnL!=0,]
cor(cordata$unrealizedPnL, cordata$ADF)
plot(x = cordata$unrealizedPnL, y = cordata$ADF)
