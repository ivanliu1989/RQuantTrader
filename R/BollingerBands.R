#' Calculate Bollinger Bands
#'
#' Bollinger bands are calculated based on moving averages and moving standard deviations of time-series.
#'
#' @param y Vector to be tested for a unit root.
#' @param lookback number of perioed to look back for testing
#' @param std number of standard deviations to calculate BB bands
#'
#' @return A \code{data.frame} with raw time-series and BB bands
#'
#' @examples
#' getFX("AUD/USD")
#' getFX("CAD/USD")
#' BollingerBands(AUDUSD/CADUSD, lookback = 20, std = 2)
#'
#' @export
BollingerBands <- function(y, lookback = 20, std = 2){

    maverage = SMA(y, lookback)
    mstd = rollapplyr(y, lookback, sd)
    mlowBB = maverage - std * mstd
    mhighBB = maverage + std * mstd

    BBbands = data.frame(y = y,
                         ma = maverage,
                         mstd = mstd,
                         BBhigh = mhighBB,
                         BBlow = mlowBB)
    colnames(BBbands) <- c("spread", "ma", "mstd", "BBhigh", "BBlow")
    return(BBbands)
}
