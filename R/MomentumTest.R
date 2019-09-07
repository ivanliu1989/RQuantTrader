#' Use RSI to calculate a momentum indicator
#'
#' Use RSI (Relative Strength Index) to calculate a momentum indicator
#'
#' @param y Price series that is coercible to xts or matrix
#' @param n Number of periods for moving averages.
#'
#' @details The RSI calculation is RSI = 100 - 100 / ( 1 + RS ), where RS is the smoothed ratio of 'average' gains over 'average' losses. The 'averages' aren't true averages, since they're divided by the value of n and not the number of periods in which there are gains/losses.
#' @details RSI > 70 means the asset is overvalued and overbought (go short)
#' @details RSI < 30 means the asset is undervalued and underbought (go long)
#'
#' @examples
#' y = AUDUSD
#' momRSI <- momentum.RSI(y)
#'
#' @seealso \link{momentum.MACD}
#' @seealso \link{momentum.Crossover}
#'
#' @export
momentum.RSI <- function(y, n){
    library(TTR)
    momRSI <- RSI(y, 14) # 30 Undervalue & 70 Overvalue
    # if RSI < 30 then go long
    # if RIS > 70 then go short
    return(momRSI)
}

#' Use MACD to create a momentum indicator
#'
#' Use MACD (moving average convergence divergence) to calculate a momentum indicator.
#'
#' @param y Price series that is coercible to xts or matrix
#' @param nFast Number of periods for fast moving average.
#' @param nSlow Number of periods for slow moving average.
#' @param nSig Number of periods for signal moving average.
#' @param nEMA Number of periods for exponentially-weighted moving average.
#'
#' @details When price is going over EMA(20) and MACD is going over 0 then go long
#' @details When price is going below EMA(20) and MACD is going below 0 then go short
#'
#' @examples
#' y = AUDUSD
#' momMACD <- momentum.MACD(y)
#'
#' @seealso \link{momentum.RSI}
#' @seealso \link{momentum.Crossover}
#'
#' @export
#' @import TTR
momentum.MACD <- function(y, nFast = 12, nSlow = 26, nSig = 9, nEMA = 20){
    library(TTR)
    momentum.MACD <- MACD(y, nFast = nFast, nSlow = nSlow, nSig = nSig)
    momentum.EMA <- EMA(y, n = nEMA)
    momentum.MACD <- merge(momentum.MACD, momentum.EMA, y)

    long.cond <- momentum.MACD$EMA < momentum.MACD[,4] & momentum.MACD$macd > 0
    short.cond <- momentum.MACD$EMA > momentum.MACD[,4] & momentum.MACD$macd < 0
    momentum.MACD$longshort <- ifelse(long.cond, 1, ifelse(short.cond, -1, 0))
    return(momentum.MACD)
}

#' Use MA Crossovers to create different momentum indicators
#'
#' Use 10 moving averages (short to long) to calculate momentum indicators.
#'
#' @param y Price series that is coercible to xts or matrix.
#'
#' @details Hamming distance indicator: rankings from 1 to 10 to indicate the strength of a momentum
#' @details Spearman cor indicator: a number from -1 to 1 to tell the direction of current momentum. (high means short and low means long)
#' @details Thickness: thickness/difference of 10 moving averages, higher thickness means higher volatilities or stronger momentum
#'
#' @examples
#' y = AUDUSD
#' momCrossover <- momentum.Crossover(y)
#'
#' @seealso \link{momentum.RSI}
#' @seealso \link{momentum.MACD}
#'
#' @export
#' @import TTR
#' @import e1071
momentum.Crossover <- function(y){

    library(e1071)
    library(TTR)

    # Generate Moving Averages
    y1 = y
    for(i in c(1:10)){
        X = SMA(y, i*10)
        y1 = merge(y1, X)
    }
    colnames(y1) <- c(colnames(y), paste0("SMA", seq(10, 100, 10)))
    y1 = na.omit(y1)

    # Hamming distance 1-10 ranking distance
    y1$hamming.Dist = 0
    for(r in 1:nrow(y1)){
        y1[r, "hamming.Dist"] = hamming.distance(rank(as.vector(y1[r, 2:11])), c(1:10))
    }

    # Spearman relation ship -1 to 1 distance
    y1$spearman = 0
    for(r in 1:nrow(y1)){
        y1[r, "spearman"] = cor(rank(as.vector(y1[r, 2:11])), c(1:10), method = "spearman")
    }

    # Thickness
    y1$thickness = 0
    for(r in 1:nrow(y1)){
        y1[r, "thickness"] = max(as.vector(y1[r, 2:11]), na.rm = T) - min(as.vector(y1[r, 2:11]), na.rm = T)
    }

    momCrossover <- y1[, c(1, 12:14)]
    return(momCrossover)
}
