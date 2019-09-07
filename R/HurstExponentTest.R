#' Hurst Exponent Test
#'
#' Effectively this returns a value between 0 and 1 that tells you whether a time-series is trending or mean-reverting.
#' The closer the value is to 0.5 means the more "random" the time-series has behaved historically.
#' Values below 0.5 imply the time-series is mean-reverting, and above 0.5 imply trending.
#' The closer the value is to 0 implies greater levels of mean-reversion.
#'
#' @param y Vector to be tested for a unit root.
#' @param lookback number of perioed to look back for testing
#'
#' @return A \code{list} of hurst test results
#'
#' @examples
#' getFX("AUD/USD")
#' getFX("CAD/USD")
#' HurstExponentTest(AUDUSD/CADUSD, 20)
#'
#' @export
#' @import FGN
HurstExponentTest <- function(y, lookback){
    library(FGN)
    retY <- ROC(y, n=1, type="discrete")
    retY[is.na(retY)] <- 0
    hurstKY <- apply.rolling(retY, FUN="HurstK", width = lookback)

    serialcorr <- runCor(cbind(coredata(hurstKY)),cbind(index(hurstKY)),n=lookback)
    serialcorr <- as.xts(serialcorr,order.by=index(hurstKY))
    autoreg <- runCor(hurstKY,lag(hurstKY,k=1),n=lookback)
    colnames(serialcorr) <- "SerialCorrelation"
    colnames(autoreg) <- "AutoRegression"


    res <- list(
        hurstKY = hurstKY,
        serialcorr = serialcorr,
        autoreg = autoreg
    )
    return(res)
}
