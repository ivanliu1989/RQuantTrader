#' Correlation Tests
#'
#' Tests if two series are correlated.
#'
#' @param x, y numeric vectors of data values.
#' @param method a character string naming which test should be applied. ("pearson","kendall","spearman")
#'
#' @return A \code{list} of test results and diagram
#'
#' @examples
#' getFX("AUD/USD")
#' getFX("CAD/USD")
#' cor = CorrelationTest(AUDUSD, CADUSD)
#' cor
#'
#' @export
CorrelationTest <- function(x, y, method = "pearson"){
    library(fBasics)
    library(PerformanceAnalytics)
    cor <- correlationTest(x, y, method)
    plot <- chart.Correlation(merge(x, y))
    return(list(cor=cor, plot=plot))
}


#' Find Most Correlated Lags
#'
#' Search the number of lags with highest correlations between x and y
#'
#' @param x main series
#' @param y secondary series which will be applied to get lags
#' @param mlag maximum lags to test
#'
#' @return A \code{list} of all lags and their correlations
#'
#' @export
findCorLags <- function(x, y, mlag = 60){
    datasets = merge(x, y)
    nlag <- matrix(0, mlag, 2)
    for(l in 1:mlag){
        data <- datasets
        data[,2] = lag(data[,2], l)
        data <- na.omit(data)
        cor <- cor(data[,1], data[,2])

        nlag[l, 1] = l
        nlag[l, 2] = cor
    }
    return(list(
        all = nlag,
        best = nlag[abs(nlag[,2]) == max(abs(nlag[,2])),]))
}
