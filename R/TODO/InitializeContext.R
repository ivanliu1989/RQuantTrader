#' Initialize Trading Context
#'
#' Initialze and configure trading context
#'
#' @param Symbol.Y Ticker symbol for series Y (long)
#' @param Symbol.X Ticker symbol for series X (short)
#' @param capital Initial capital for investment
#' @param window used for zscore calculation, must be <= \code{lookback}
#' @param lookback used for regression
#' @param brokerage Commission fees in percentage
#' @param stoploss Max drawdown for stop loss in percentage
#'
#' @return A \code{list} of attributes about current trading context
#'
#' @examples
#' getFX("AUD/USD")
#' getFX("CAD/USD")
#' context <- InitializeContext(AUDUSD, CADUSD, capital = 1e5, window = 20, lookback = 250, brokerage = 0.02, stoploss = 0.1)
#' context
#'
#' @export
InitializeContext <- function(Symbol.Y, Symbol.X, capital = 1e5, window = 20, lookback = 250, brokerage = 0.02, stoploss = 0.1, half.life){
  lookback <- min(lookback, length(Symbol.Y))
  context <- list(
    y = Symbol.Y[(length(Symbol.Y)-lookback+1):length(Symbol.Y)],
    x = Symbol.X[(length(Symbol.X)-lookback+1):length(Symbol.X)],
    pair = merge(Symbol.Y[(length(Symbol.Y)-lookback+1):length(Symbol.Y)],
                 Symbol.X[(length(Symbol.X)-lookback+1):length(Symbol.X)]),
    capital = capital,
    window = window,
    lookback = lookback,
    brokerage = brokerage,
    stoploss = stoploss,
    half.life = half.life
  )
  return(context)
}


