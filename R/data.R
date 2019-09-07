#' Prices of AUD/USD FX.
#'
#' A time-series containing the 499 prices points of AUD/USD FX.
#'
#' @format A xts, zoo with 499 rows and 1 variable:
#' \describe{
#'   \item{AUD.USD}{price, in US dollars}
#'   ...
#' }
#' @source \url{https://finance.yahoo.com/quote/AUDUSD=X?p=AUDUSD=X}
"AUDUSD"

#' Prices of CAD/USD FX.
#'
#' A time-series containing the 499 prices points of CAD/USD FX.
#'
#' @format A xts, zoo with 499 rows and 1 variable:
#' \describe{
#'   \item{CAD.USD}{price, in US dollars}
#'   ...
#' }
#' @source \url{https://finance.yahoo.com/quote/CADUSD=X?p=CADUSD=X}
"CADUSD"

#' Prices of S&P 500.
#'
#' Prices of S&P 500 from 2010-01-01 to 2014-01-31.
#'
#' @format A xts, zoo matrix with 500 stocks in S&P from 2010-01-01 to 2014-01-31:
#' \describe{
#'   \item{AAPL}{price, in US dollars}
#'   ...
#' }
"sp500"

#' Australian Bonds.
#'
#' 2, 3, 5 and 10 years Australian bonds.
#'
#' @format A xts, zoo matrix with 5460 points of Australian Bonds from 1995-01-03 to 2016-08-17:
#' \describe{
#'   \item{AUS2Y}{2 years AUS bonds}
#'   ...
#' }
"AUSYC"

#' Canadian Bonds.
#'
#' 2, 3, 5 and 10 years Canadian bonds.
#'
#' @format A xts, zoo matrix with 3002 points of Canadian Bonds from 2004-08-20 to 2016-08-18:
#' \describe{
#'   \item{CAN2Y}{2 years CAN bonds}
#'   ...
#' }
"CANYC"

#' Canadian Bonds.
#'
#' 2, 3, 5 and 10 years USA bonds.
#'
#' @format A xts, zoo matrix with 6653 points of USA Bonds from 1990-1-02 to 2016-08-02:
#' \describe{
#'   \item{USA2Y}{2 years USA bonds}
#'   ...
#' }
"USAYC"


#' Sample Data for Modeling
#'
#' All required features including AUDUSD, CADUSD and Bonds Ratios for modeling
#'
#' @format A xts, zoo matrix with 618 points from 2014-1-02 to 2016-07-22:
#' \describe{
#'   \item{price.ratio}{price ratios between pairs}
#'   ...
#' }
"SampleUniverse"

