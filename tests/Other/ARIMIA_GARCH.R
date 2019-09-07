library(RQuantAPI)


# This part should be done in python --------------------------------------
AUD_USD = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = 'AUD_USD', price = 'M', granularity = 'D', count = 300)
AUD_USD$complete <- NULL
names(AUD_USD) <- c("time", "Volume", "Close", "High", "Low", "Open")
AUD_USD[, 2:6] <- sapply(AUD_USD[, 2:6], as.numeric)
AUD_USD = xts(AUD_USD[,-1],as.POSIXct(as.Date(AUD_USD$time)))

spReturns = diff(log(Cl(AUD_USD)))
spReturns[as.character(head(index(Cl(AUD_USD)),1))] = 0
# -------------------------------------------------------------------------


# Function in R, should be called from Python -----------------------------
ARIMA_GARCH_IN_R = function(spReturns){
    # Import the necessary libraries
    library(quantmod)
    library(lattice)
    library(timeSeries)
    library(rugarch)

    # Create the forecasts vector to store the predictions
    windowLength = 250
    foreLength = length(spReturns) - windowLength
    forecasts <- data.frame(date = index(spReturns)[(windowLength+1):length(spReturns)],
                            actual = spReturns[(windowLength+1):length(spReturns)],
                            predictions = rep(0, foreLength))
    for (d in 0:(foreLength-1)) {
        # Obtain the rolling window for this day
        spReturnsOffset = spReturns[(1+d):(windowLength+d)]
        # Fit the ARIMA model
        final.aic <- Inf
        final.order <- c(0,0,0)
        for (p in 0:5) for (q in 0:5) {
            if ( p == 0 && q == 0) {
                next
            }
            arimaFit = tryCatch( arima(spReturnsOffset, order=c(p, 0, q)),
                                 error=function( err ) FALSE,
                                 warning=function( err ) FALSE )
            if( !is.logical( arimaFit ) ) {
                current.aic <- AIC(arimaFit)
                if (current.aic < final.aic) {
                    final.aic <- current.aic
                    final.order <- c(p, 0, q)
                    final.arima <- arima(spReturnsOffset, order=final.order)
                }
            } else {
                next
            }
        }
        # Specify and fit the GARCH model
        spec = ugarchspec(
            variance.model=list(garchOrder=c(1,1)),
            mean.model=list(armaOrder=c(
                final.order[1], final.order[3]
            ), include.mean=T),
            distribution.model="sged"
        )
        fit = tryCatch(
            ugarchfit(
                spec, spReturnsOffset, solver = 'hybrid'
            ), error=function(e) e, warning=function(w) w
        )
        # If the GARCH model does not converge, set the direction to "long" else
        # choose the correct forecast direction based on the returns prediction
        if(is(fit, "warning")) {
            forecasts[d+1,3] = 0
            print(
                paste(
                    index(spReturnsOffset[windowLength]), 0, sep=","
                )
            )
        } else {
            fore = ugarchforecast(fit, n.ahead=1)
            ind = fore@forecast$seriesFor
            forecasts[d+1,3] = ind[1]

            print(paste(colnames(ind), ind[1], sep=","))
        }
    }

    return(forecasts)
}
# -------------------------------------------------------------------------


# This results should be sent by to python environment --------------------
res = ARIMA_GARCH_IN_R(spReturns)
# -------------------------------------------------------------------------

