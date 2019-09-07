#' Zscore
#'
#' Normalize time series
#'
#' @param t a time-series to be normalized
#'
#' @return A normalized time-series
#'
#' @examples
#' library(quantmod)
#' getFX("AUD/USD")
#' zscores(AUDUSD)
#'
#' @export
zscores <- function(t){
    tz <- (t-mean(t))/sd(t)
    return(tz)
}

#' Zscore based Moving Average
#'
#' Using moving averages to compute the z-score of the ratio at each given time.
#' This will tell us how extreme the ratio is and whether it's a good idea to enter a position at this time.
#'
#' @param t a time-series to be normalized
#' @param long days of long-term moving averages
#' @param short days of short-term moving averages
#'
#' @return A normalized time-series
#'
#' @examples
#' library(quantmod)
#' getFX("AUD/USD")
#' zscores.ma(AUDUSD, 60, 10)
#'
#' @export
zscores.ma <- function(t, long = 60, short = 10){
    tz <- (SMA(t, short) - SMA(t, long)) / rollapply(t, long, sd)
    return(tz)
}

#' Fill Missing Data
#'
#' Fill missing data according to the last value
#'
#' @param v a vector of values
#'
#' @return A \code{vector} of filled values
#'
#' @examples
#' v = c(1, rep(NA, 10), 2)
#' fillMissingData(v)
#'
#' @export
fillMissingData <- function(v){
    for(i in 1:length(v)){
        v[i]<-ifelse(is.na(v[i]), v[i-1], v[i])
    }
    return(v)
}

#' Compute the Holdings Percentages
#'
#' Compute the Holdings Percentages based on long/short shares and prices
#'
#' @param y.shares y shares
#' @param x.shares x shares
#' @param y.price y price
#' @param x.price x price
#'
#' @return A \code{vector} of percentages of x and y to be targeted
#'
#' @export
computeHoldingsPct <- function(y.shares, x.shares, y.price, x.price){
    y.dollars = y.shares * y.price
    x.dollars = x.shares * x.price
    notional.dollars = abs(y.dollars) + abs(x.dollars)
    y.target.pct = y.dollars / notional.dollars
    x.target.pct = x.dollars / notional.dollars
    return(c(y.target.pct, x.target.pct))
}


#' Make Orders based on Portfolio Percent
#'
#' Make orders based on portfolio percentages
#'
#' @param initial.capital total capital to invest
#' @param capital current capital in hand
#' @param holdings current holdings in dollar values for symbol
#' @param prices current prices of symbol
#' @param current.shares current number of units of symbol
#' @param pct target percent of total portfolio to invest
#' @param brokerage brokerage fees in percentage
#'
#' @return A \code{list} of updated values for current status of the symbol
#'
#' @export
make_order_pct <- function(initial.capital, capital, holdings, prices, current.shares, pct, brokerage = 0.001){
    initial.capital = as.numeric(initial.capital)
    capital = as.numeric(capital)
    holdings = as.numeric(holdings)
    prices = as.numeric(prices)
    pct = as.numeric(pct)
    current.shares = as.numeric(current.shares)

    order_dollars = initial.capital * pct
    order_num = round(order_dollars / prices / 100) * 100 - current.shares
    brokerage = abs(order_num * brokerage)
    # 1. update capital
    capital = capital - prices * order_num - brokerage
    # 2. update holding
    holdings = holdings + prices * order_num
    # 3. shares
    shares = current.shares + order_num

    res = list(
        capital = capital,
        holdings = holdings,
        prices = prices,
        shares = shares,
        brokerage = brokerage
    )
    return(res)
}


#' Create a Summary Sheet for Backtesting
#'
#' Create a Summary Sheet for Backtesting to record all trading activities across the time
#'
#' @param y y series
#' @param x x series
#' @param initial.capital initial capital
#'
#' @return A \code{data.frame}
#'
#' @export
createSummarySheet <- function(prices, initial.capital){
    dt.summary <- as.data.frame(prices)
    dt.summary$in_short = FALSE
    dt.summary$in_long = FALSE
    dt.summary$capital = initial.capital
    dt.summary$y.holdings = 0
    dt.summary$x.holdings = 0
    dt.summary$y.shares = 0
    dt.summary$x.shares = 0
    dt.summary$hedgeRatio = 0
    dt.summary$alpha = 0
    dt.summary$beta = 0
    dt.summary$long.pos = 0
    dt.summary$short.pos = 0
    dt.summary$brokerage = 0
    dt.summary$real.capital = 0
    dt.summary$trade = "NO Trade"

    return(dt.summary)
}


#' Convert Summary sheet into data.table
#'
#' Convert Summary sheet into data.table
#'
#' @param dt summary trading sheet
#'
#' @return A \code{data.table}
#'
#' @export
setDataTable <- function(dt){
    library(data.table)
    dates <- rownames(dt)
    dt <- setDT(dt)
    dt[, Dates := as.IDate(dates)]

    return(dt)
}


#' Get Price Ratio / Spreads between a Pair of Prices
#'
#' Get Price Ratio / Spreads between a Pair of Prices
#'
#' @param y y series
#' @param x x series
#' @param log log scaled or not
#' @param spread return spread instead of price ratio
#'
#' @return A \code{vector} of price ratio OR spreads
#'
#' @export
getPriceRatio <- function(y, x, log = TRUE, spread = FALSE){
    if(!class(y) == "numeric"){
        p = na.omit(merge(y, x))
        y = p[,1]
        x = p[,2]
    }
    if(spread){
        p_ratio <- y-x
    }else{
        p_ratio <- (y/x)
        p_ratio[is.infinite(p_ratio)] <- NA
        p_ratio <- na.omit(p_ratio)
        if(log) p_ratio <- log(p_ratio)
    }
    tryCatch({
        colnames(p_ratio) <- "price.ratio"
    })
    return(p_ratio)
}


#' Get an Index of Current T-series
#'
#' Divide each column by the first non-NA value to get an index of 1 for input time-series
#'
#' @param x time series
#'
#' @return A \code{vector} of indexed time-series
#'
#' @export
indexation <- function(x) {
    # Divide each column by the first non-NA value
    # (There may already be a function to do that.)
    coredata(x) <- t(t(coredata(x)) / apply(coredata(x),2,function(u){ c(u[!is.na(u)&u!=0],NA)[1] }))
    return(x)
}


#' Calculate Forward FX
#'
#' Calculate forward FX by using Covered Interest Rate Parity formula
#'
#' @param If foreign Interest Rates
#' @param Id domestic Interest Rates
#' @param SFX Spot FX prices
#'
#' @return A \code{list} of forward FX and their growth rates
#'
#' @export
CoInterestRateParity <- function(If, Id, SFX){
    datasets <- merge(If, Id, SFX)
    FFX = datasets[,3] * ((1 + datasets[,1]) / (1 + datasets[,2]))
    Gwt = ((1 + datasets[,1]) / (1 + datasets[,2]))
    return(list(
        FFX = FFX,
        Gwt = Gwt
    ))
}


#' Get the slope of a vector of numbers
#'
#' Get the slope of a vector of numbers
#'
#' @param y a vector of numbers
#'
#' @return A \code{numer} of slope value
#'
#' @export
getSlope = function(y){
    dt = data.frame(y = as.vector(y), x = 1:length(y))
    fit <- lm(y~., dt)
    slope = as.numeric(coef(fit)[2])
    return(slope)
}




#' Load all required quant libraries in one go
#'
#' Load all required quant libraries in one go
#'
#' @export
loadQuantPackages = function(){
    library(quantmod)
    library(tseries)
    library(TTR)
    library(data.table)
    library(xts)
    library(lattice)
    library(timeSeries)
    library(PerformanceAnalytics)
    library(zoo)
    library(IBrokers)
    library(fBasics)
    library(partialAR)
    library(pracma)
    library(vrtest)
    library(urca)
    library(xgboost)
}
