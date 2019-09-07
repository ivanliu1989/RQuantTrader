#' Generate Japanese Candlesticks Patterns
#'
#' Generate Japanese Candlesticks Patterns
#'
#' @param x TS
#'
#' @examples
#' load('~/Common/audcad_intraday_m15/model/Data_AUDUSD_1487485108.RData')
#' generateCandleStickPatterns(prices.ib)
#'
#' @export
generateCandleStickPatterns = function(x){
    library(candlesticks)
    cdlBodyLen = candlesticks::CandleBodyLength(x)
    cdlLen = candlesticks::CandleLength(x)
    darkCloudCover = candlesticks::CSPDarkCloudCover(x)
    dojiPat = candlesticks::CSPDoji(x)
    engulfingPat = candlesticks::CSPEngulfing(x)
    gapPat = candlesticks::CSPGap(x)
    hammerPat = candlesticks::CSPHammer(x)
    haramiPat = candlesticks::CSPHarami(x)
    inSideDay = candlesticks::CSPInsideDay(x)
    outSideDay = candlesticks::CSPOutsideDay(x)
    invHammerPat = candlesticks::CSPInvertedHammer(x)
    kickingPat = candlesticks::CSPKicking(x)
    longCandle = candlesticks::CSPLongCandle(x)
    longCandleBody = candlesticks::CSPLongCandleBody(x)
    shortCandle = candlesticks::CSPShortCandle(x)
    shortCandleBody = candlesticks::CSPShortCandleBody(x)
    marubozuPat = candlesticks::CSPMarubozu(x)
    consecHigh2 = candlesticks::CSPNHigherClose(x, 2)
    consecLow2 = candlesticks::CSPNLowerClose(x, 2)
    consecHigh3 = candlesticks::CSPNHigherClose(x, 3)
    consecLow3 = candlesticks::CSPNLowerClose(x, 3)
    consecHigh4 = candlesticks::CSPNHigherClose(x, 4)
    consecLow4 = candlesticks::CSPNLowerClose(x, 4)
    piercingPat = candlesticks::CSPPiercingPattern(x)
    # starPat = candlesticks::CSPStar(x)
    stomachPat = candlesticks::CSPStomach(x)
    # tasukiGap = candlesticks::CSPTasukiGap(x)
    # threeInsidePat = candlesticks::CSPThreeInside(x)
    threeMethod = candlesticks::CSPThreeMethods(x)
    threeOutsidePat = candlesticks::CSPThreeOutside(x)
    donchianChannel = candlesticks::DonchianChannel2(x); names(donchianChannel) = paste0('donchian.', names(donchianChannel))
    trendDetect = candlesticks::TrendDetectionChannel(x)[,4]; names(trendDetect) = paste0('tndDetect.', names(trendDetect))
    trendSMA = candlesticks::TrendDetectionSMA(x)[,4]; names(trendSMA) = paste0('tndSMA.', names(trendSMA))
    nxtCandlPos = lag(candlesticks::nextCandlePosition(x))

    candleStickPattersDat = merge(cdlBodyLen, cdlLen, darkCloudCover, dojiPat, engulfingPat, gapPat, hammerPat, haramiPat, inSideDay,
                                  outSideDay, invHammerPat, kickingPat, longCandle, longCandleBody, shortCandle, shortCandleBody,
                                  marubozuPat, consecHigh2, consecLow2, consecHigh3, consecLow3, consecHigh4, consecLow4,
                                  piercingPat, stomachPat, threeMethod, threeOutsidePat,
                                  donchianChannel, trendDetect, trendSMA, nxtCandlPos)[-c(1:20), ]
    # starPat
    # threeWhiteSoldier = candlesticks::CSPThreeWhiteSoldiers(x)
    # candlesticks::CSPThreeBlackCrows(x)
    # longBlackCdl = candlesticks::CSPNLongBlackCandleBodies(TS = x, N = 2, n = 20, threshold = 1)
    # candlesticks::CSPNLongBlackCandles(x)
    # candlesticks::CSPNLongWhiteCandleBodies(x)
    # candlesticks::CSPNLongWhiteCandles(x)


    candleStickPattersDat[is.na(candleStickPattersDat)] = -1

    return(candleStickPattersDat)
}
