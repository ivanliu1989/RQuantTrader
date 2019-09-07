#' Generate model data based on prices
#'
#' Generate model data based on prices
#'
#' @param dat raw data of prices
#' @param test.p lookback periods
#'
#' @export
modelPricingFeatures = function(dat, test.p = 63){
    finIndicators = finIndicatorHLC(dat)

    target <- lag(diff(Cl(dat)), -1)
    colnames(target) = 'target'
    all = na.omit(merge(finIndicators, target))

    test = all[(nrow(all)-test.p):nrow(all), ]
    training = all[1:(nrow(all)-(test.p+1)), ]
    preProcValues <- preProcess(as.data.frame(all[, -ncol(all)]), method = c("scale"))
    trainBC <- predict(preProcValues, as.data.frame(training))
    testBC <- predict(preProcValues, as.data.frame(test))
    allBC <- predict(preProcValues, as.data.frame(all))
    # Reg and BC
    trainREG = trainBC
    testREG = testBC
    allREG = allBC
    trainBC$target = as.factor(ifelse(trainBC$target >= 0, 1, 0))
    testBC$target = as.factor(ifelse(testBC$target >= 0, 1, 0))
    allBC$target = as.factor(ifelse(allBC$target >= 0, 1, 0))
    response <- "target"
    predictors <- setdiff(names(trainBC), response)

    return(list(all=all,
                allBC=allBC,
                allREG=allREG,
                trainBC=trainBC,
                testBC=testBC,
                trainREG=trainREG,
                testREG=testREG,
                response=response,
                predictors=predictors))
}


#' Consecutive Candle Analysis
#'
#' Consecutive Candle Analysis
#'
#' @param data raw data of prices
#'
#' @export
modelCandleFeatures = function(data){
    data$bull = 1
    data$bull = ifelse(Cl(data) >= Op(data), 1, 0)

    # count consecutive bullish / bearish candles
    data$ConsecWins <- 1
    for(i in 2:nrow(data)){
        if(as.numeric(data[i,]$bull) == as.numeric(data[i-1,]$bull))
            data[i,]$ConsecWins = as.numeric(data[i-1,]$ConsecWins + 1)
    }

    # example 1: prob. of trend change
    data$nextcount = shift(data$ConsecWins, 1)

    mytable = table(data$ConsecWins, data$nextcount)
    # mytable

    percent.table <- prop.table(mytable, 1)
    percent.table[percent.table==0] <- NA
    # percent.table

    return(data)
}

#' Create Bonds and Interest Rates Features
#'
#' Create Bonds and Interest Rates Features
#'
#' @param bond1 main bonds
#' @param bond2 secondary bonds
#'
#' @export
modelBondIRFeatures = function(bond1, bond2){

    names(bond1) <- paste0('Bond1.', c(2,3,5,10),'Y')
    names(bond2) <- paste0('Bond2.', c(2,3,5,10),'Y')
    all.bonds <- na.omit(merge(bond1, bond2))

    # 1. Uncovered Interest Rate Parity ---------------------------------------
    all.bonds$IR.2 = (all.bonds$Bond1.2Y+100)/(all.bonds$Bond2.2Y+100)
    all.bonds$IR.3 = (all.bonds$Bond1.3Y+100)/(all.bonds$Bond2.3Y+100)
    all.bonds$IR.5 = (all.bonds$Bond1.5Y+100)/(all.bonds$Bond2.5Y+100)
    all.bonds$IR.10 = (all.bonds$Bond1.10Y+100)/(all.bonds$Bond2.10Y+100)

    # 2. Calculate Forward Rate -----------------------------------------------
    all.bonds$IR.10.2 = (all.bonds$Bond1.10Y+100)/(all.bonds$Bond1.2Y+100)
    all.bonds$IR.10.3 = (all.bonds$Bond1.10Y+100)/(all.bonds$Bond1.3Y+100)
    all.bonds$IR.10.5 = (all.bonds$Bond1.10Y+100)/(all.bonds$Bond1.5Y+100)

    return(all.bonds)
}


#' For loop for generating Univariate Features
#'
#' For loop for generating Univariate Features
#'
#' @param dat multivariate time-series
#'
#' @export
modelMultivariableFeatures = function(dat){
    for(x in 1:ncol(dat)){
        finInd = finIndicatorCommon(dat[,x])
        names(finInd) = paste(names(dat)[x], names(finInd))
        if(x==1){
            dat.fnl = finInd
        }else{
            dat.fnl = na.omit(merge(dat.fnl, finInd))
        }
    }
    return(dat.fnl)
}




#' Generate Financial Indicator (Common)
#'
#' Generate Financial Indicator (Common)
#'
#' @param x prices data
#'
#' @export
finIndicatorCommon = function(x){

    # Common Indicators -------------------------------------------------------
    # EMA
    EMA12 = TTR::EMA(x, 12) # 12, 26, 50 to 100
    EMA26 = TTR::EMA(x, 26)
    EMA50 = TTR::EMA(x, 50)
    EMA100 = TTR::EMA(x, 100)
    EMA12v100 = EMA12 / EMA100
    EMA26v100 = EMA26 / EMA100
    EMA50v100 = EMA50 / EMA100

    EMA.Feat = merge(EMA12v100, EMA26v100, EMA50v100)
    colnames(EMA.Feat) = c('EMA12v100', 'EMA26v100', 'EMA50v100')

    # Cross over
    mo.xover = momentum.Crossover(x)
    mo.xover = mo.xover[, c('hamming.Dist', 'spearman', 'thickness')]

    # MACD Oscillator
    MACD.Feat = TTR::MACD(x)
    MACD.Feat$signal.diff = MACD.Feat$macd - MACD.Feat$signal
    colnames(MACD.Feat) = c('MACD', 'MACD.Signal', 'Signal.Diff')

    # Rate of Change
    ROC5 = TTR::ROC(x, n = 5) # 5, 21, 125
    ROC21 = TTR::ROC(x, n = 21)
    ROC125 = TTR::ROC(x, n = 125)

    ROC.Feat = merge(ROC5, ROC21, ROC125)
    colnames(ROC.Feat) = c('ROC5', 'ROC21', 'ROC125')

    # Volatility
    Volatility5 = rollapply(x, width = 5, FUN = sd)
    Volatility21 = rollapply(x, width = 21, FUN = sd)
    Volatility125 = rollapply(x, width = 125, FUN = sd)

    Volatility.Feat = merge(Volatility5, Volatility21, Volatility125)
    colnames(Volatility.Feat) = c('Volatility5', 'Volatility21', 'Volatility125')

    # Mean Reversion
    mr.bbands = BollingerBands(x)
    mr.bbands$BBhigh.diff = mr.bbands$spread - mr.bbands$BBhigh
    mr.bbands$BBlow.diff = mr.bbands$spread - mr.bbands$BBlow
    mr.bbands = mr.bbands[, c('BBhigh.diff', 'BBlow.diff')]
    mr.bbands = as.xts(mr.bbands, order.by = index(x))

    mr.zscore = zscores.ma(x)
    colnames(mr.zscore) = 'ZScores'

    # SMA
    SMA100v200 = TTR::SMA(x, n = 100)/TTR::SMA(x, n = 200) # 100 to 200
    colnames(SMA100v200) = 'SMA100v200'

    # RSI
    RSI14 = TTR::RSI(x, n = 14) # 14
    colnames(RSI14) = 'RSI14'



    # Integration
    finIndicators = merge(EMA.Feat, mo.xover, MACD.Feat, ROC.Feat, Volatility.Feat,
                          mr.bbands, mr.zscore, SMA100v200, RSI14)

    return(finIndicators)
}


#' Generate Financial Indicator (HLC)
#'
#' Generate Financial Indicator (HLC)
#'
#' @param x prices data
#'
#' @export
finIndicatorHLC = function(x){

    # Prices Only Indicators --------------------------------------------------
    # ATR (HLC Prices)
    ATR14 = TTR::ATR(HLC(x), n = 14)[, c(1,2)] # 14 & 45
    ATR45 = TTR::ATR(HLC(x), n = 45)[, c(1,2)]

    ATR.Feat = merge(ATR14, ATR45)
    colnames(ATR.Feat) = c('ATR14.TR', 'ATR14.ATR', 'ATR45.TR', 'ATR45.ATR')

    # stoch (HLC Prices)
    STOCK.Feat = TTR::stoch(HLC(x)) # 14, 45, 125
    colnames(STOCK.Feat) = c('STOCK.fastK', 'STOCK.fastD', 'STOCK.slowD')

    # Common Indicators
    comIndicators = finIndicatorCommon(Cl(x))

    fi.pSAR = Cl(x) - SAR(merge(Hi(x), Lo(x)))
    colnames(fi.pSAR) = 'pSAR'

    fi.ADX = ADX(HLC(x))
    fi.ADX$DI.diff = fi.ADX$DIp - fi.ADX$DIn
    colnames(fi.ADX) = c('DIp', 'DIn', 'DX', 'ADX', 'DI.diff')

    fi.CCI = CCI(HLC(x))
    colnames(fi.CCI) = 'CCI'

    fi.williams = williamsAD(HLC(x))
    colnames(fi.williams) = 'Williams'

    fi.ultiOscillator = ultimateOscillator(HLC(x))
    colnames(fi.ultiOscillator) = 'ultiOscillator'

    # Consec wins
    co.cwins = diff(Cl(x))
    co.cwins$bull = ifelse(Cl(co.cwins) >= 0, 1, -1)
    # count consecutive bullish / bearish candles
    co.cwins$count <- 1
    co.cwins = na.omit(co.cwins)
    for(i in 2:nrow(co.cwins)){
        if(as.numeric(co.cwins[i,]$bull) == as.numeric(co.cwins[i-1,]$bull))
            co.cwins[i,]$count = as.numeric(co.cwins[i-1,]$count + 1)
    }
    co.cwins$count = co.cwins$count * co.cwins$bull
    co.cwins = co.cwins$count
    colnames(co.cwins) = 'ConsecCnt'

    # Jump price
    x$JumpOpen = Op(x) - lag(Cl(x))

    # Integration
    finIndicators = merge(comIndicators, ATR.Feat, STOCK.Feat, fi.pSAR, fi.ADX, fi.CCI
                          , fi.williams, fi.ultiOscillator, co.cwins, x$JumpOpen)

    return(finIndicators)

}

#' Generate Financial Indicator (HLC + Vol)
#'
#' Generate Financial Indicator (HLC + Vol)
#'
#' @param x prices data
#' @param v volume
#'
#' @export
finIndicatorHLCVol = function(x, v){

    # On Balance Volume (Prices & Volume)
    dat = merge(Cl(x), v)
    OBV.fun = function(x) {
        OBV.Feat = TTR::OBV(x[, 1], x[,2])
        return(OBV.Feat[nrow(OBV.Feat)])
    }
    OBV5 = rollapply(dat, width = 5, by.column = F, align = 'right', FUN = OBV.fun) # 5, 15, 60
    OBV15 = rollapply(dat, width = 15, by.column = F, align = 'right', FUN = OBV.fun)
    OBV60 = rollapply(dat, width = 60, by.column = F, align = 'right', FUN = OBV.fun)

    OBV.Feat = merge(OBV5, OBV15, OBV60)
    colnames(OBV.Feat) = c('OBV5', 'OBV15', 'OBV60')

    # Common Indicators
    comIndicators = finIndicatorHLC(x)

    # Integration
    finIndicators = merge(comIndicators, OBV.Feat)

    return(finIndicators)

}
