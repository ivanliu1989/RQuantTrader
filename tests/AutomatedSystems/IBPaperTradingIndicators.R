#
# # Prices Only Indicators --------------------------------------------------
# # ATR (HLC Prices)
# TTR::ATR() # 14 & 45
#
# # On Balance Volume (Prices & Volume)
# TTR::OBV() # 5, 15, 60
#
# # stoch (HLC Prices)
# TTR::stoch() # 14, 45, 125
#
#
# # Common Indicators -------------------------------------------------------
# # EMA
# TTR::EMA() # 12, 26, 50 to 100
#
# # Detrended Price Oscillator
# TTR::DPO()
#
# # MACD Oscillator
# TTR::MACD()
#
# # Rate of Change
# TTR::ROC() # 5, 21, 125
#
# # SMA
# TTR::SMA() # 100 to 200
#
# # RSI
# TTR::RSI() # 14



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
