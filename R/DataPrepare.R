#' Prepare all required datasets for FX daily Modeling
#'
#' Prepare all required datasets for FX daily Modeling.
#'
#' @param ib.duration length of data to retreive from Interactive Broker
#' @param ib.barsize the barsize of IB data
#' @param Cur1 currency one
#' @param Cur2 currency two
#' @param oanda.granularity barsize of Oanda data
#' @param QuandlSymbol1 currency one for Quandl
#' @param QuandlSymbol2 currency two for Quandl
#' @param ibAcc account type of Interactive Brokers (live or paper)
#'
#' @return A \code{list} of \code{xts}
#'
#' @examples
#' res = prepareForexData(ib.duration = "1 Y", ib.barsize = "1 day", Cur1 = "USD", Cur2 = "CAD",
#' oanda.granularity = 'D', QuandlSymbol1 = 'CAN', QuandlSymbol2 = 'USA')
#'
#' @export
prepareForexData <- function(ib.duration = "1 Y", ib.barsize = "1 day", Cur1 = "USD", Cur2 = "CAD",
                             oanda.granularity = 'D', QuandlSymbol1 = 'CAN', QuandlSymbol2 = 'USA', ibAcc = 'paper'){

    library(IBrokers)
    library(RQuantAPI)

    convertToXts <- function(cm, tzone = ""){
        dts <- structure(as.numeric(as.POSIXlt(gsub("(\\d{4})(\\d{2})(\\d{2})",
                                                    "\\1-\\2-\\3", cm[, 1], perl = TRUE))),
                         class = c("POSIXct","POSIXt"), tzone = tzone)
        if(ncol(cm) > 2){
            x <- xts(as.matrix(apply(cm[, -1], 2, as.numeric)), order.by = dts, tzone = tzone)
        }else{
            x <- xts(as.numeric(cm[, -1]), order.by = dts, tzone = tzone)
        }

        return(x)
    }

    instrument = paste0(Cur1, '_', Cur2)
    if(ibAcc == 'live'){
        tws <- twsConnect(port = 7496, clientId = 999)
    }else{
        tws <- twsConnect(port = 7497, clientId = 998)
    }

    # IB OHLC -----------------------------------------------------------------
    cat('\nLoading Interactive Brokers OHLC...\n')
    ccy <- reqContractDetails(tws, twsCurrency(Cur1, Cur2))[[1]]$contract
    cols = c('Open', 'High', 'Low', 'Close')
    # Bid Ask
    IB.BIDASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                                duration = ib.duration, useRTH = "1", whatToShow='BID_ASK')
    IB.BIDASK = IB.BIDASK[, 1:4]
    IB.BIDASK = IB.BIDASK[!duplicated(index(IB.BIDASK)), ]
    colnames(IB.BIDASK) = paste0('IB.BA.', cols)

    # Mid
    IB.MID <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                             duration = ib.duration, useRTH = "1", whatToShow='MIDPOINT')
    IB.MID = IB.MID[, 1:4]
    IB.MID = IB.MID[!duplicated(index(IB.MID)), ]
    colnames(IB.MID) = paste0('IB.M.', cols)

    # Bid
    IB.BID <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                             duration = ib.duration, useRTH = "1", whatToShow='BID')
    IB.BID = IB.BID[, 1:4]
    IB.BID = IB.BID[!duplicated(index(IB.BID)), ]
    colnames(IB.BID) = paste0('IB.B.', cols)

    # Ask
    IB.ASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                             duration = ib.duration, useRTH = "1", whatToShow='ASK')
    IB.ASK = IB.ASK[, 1:4]
    IB.ASK = IB.ASK[!duplicated(index(IB.ASK)), ]
    colnames(IB.ASK) = paste0('IB.A.', cols)


    # IB Spreads --------------------------------------------------------------
    cat('\nLoading Interactive Brokers Spreads...\n')
    IB.SPREADS = IB.ASK - IB.BID
    colnames(IB.SPREADS) = paste0('IB.SP.', cols)
    IB.SPREADS.Perc = (IB.SPREADS / IB.MID) * 100
    colnames(IB.SPREADS.Perc) = paste0('IB.SPP.', cols)


    # IB Volatility -----------------------------------------------------------
    cat('\nLoading Interactive Brokers Volatility...\n')
    IB.VOLATILITY <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                                       duration = ib.duration, useRTH = "1", whatToShow='HISTORICAL_VOLATILITY')
    IB.VOLATILITY = IB.VOLATILITY[,6]
    IB.VOLATILITY = IB.VOLATILITY[!duplicated(index(IB.VOLATILITY)), ]
    colnames(IB.VOLATILITY) = paste0('IB.Volatility.',Cur1, '.', Cur2)


    # Oanda OHLC --------------------------------------------------------------
    cat('\nLoading Oanda OHLC...\n')
    oanda.count = nrow(IB.BIDASK) + 1
    OA.ALL = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = instrument,
                                          price = 'BA', granularity = oanda.granularity, count = oanda.count)
    OA.ALL = convertToXts(OA.ALL[, -1])
    colnames(OA.ALL) = c('OA.Volume', 'OA.Ask.Close', 'OA.Ask.High', 'OA.Ask.Low', 'OA.Ask.Open'
                            , 'OA.Bid.Close', 'OA.Bid.High', 'OA.Bid.Low', 'OA.Bid.Open')
    OA.BIDASK = OA.ALL[, -1]


    # Oanda Spreads -----------------------------------------------------------
    # OA.SPREADS = SpreadsOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID,
    #                           INSTRUMENTS = instrument, PERIOD = '1 year')
    cat('\nLoading Oanda Spreads...\n')
    OA.SPREADS = OA.ALL[, 2:5] - OA.ALL[, 6:9]
    OA.SPREADS.Perc = ((OA.ALL[, 2:5] - OA.ALL[, 6:9])/((OA.ALL[, 2:5] + OA.ALL[, 6:9])/2)) * 100


    # Oanda Volume ------------------------------------------------------------
    cat('\nLoading Oanda Volume...\n')
    OA.VOLUME = OA.ALL[, 1]


    # Oanda Net Positions -----------------------------------------------------
    cat('\nLoading Oanda New Positions...\n')
    OA.HistPos <- HistoricalPositionOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID,
                                       INSTRUMENTS = instrument, PERIOD = '1 year')
    OA.HistPos = convertToXts(as.data.frame(OA.HistPos)[, -1])
    colnames(OA.HistPos) = c('OA.LongPosRatio', 'OA.ExRate')
    OA.HistPos$OA.PosCorr = rollapply(OA.HistPos, by.column = F, width = 20, function(x) cor(x)[1,2])


    # Oanda CBOT --------------------------------------------------------------
    cat('\nLoading Oanda / Quandl CBOT...\n')
    OA.CBOT = COT.Oanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = instrument)
    OA.CBOT = convertToXts(as.data.frame(OA.CBOT)[, -6])
    # calNet <- function(x,y){
    #     net <- (x-y)/(x+y)
    #     return(net)
    # }
    # CFTC.AUD <- Quandl("CFTC/TIFF_CME_AD_ALL")
    # CFTC.AUD <- as.xts(calNet(CFTC.AUD$`Lev Money Long Positions`,CFTC.AUD$`Lev Money Short Positions`),CFTC.AUD$Date)


    # Oanda Autochartist ------------------------------------------------------
    cat('\nLoading Oanda / Autochartist Signals...\n')
    Oanda.Autochartist = Autochartist.Oanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN,
                                            INSTRUMENTS = instrument, PERIOD = '1 week')


    # Quandl Indexes ----------------------------------------------------------
    cat('\nLoading Quandl Equity Indexes...\n')
    QuandlConnect()
    # AU
    EI.ASX <- Quandl("YAHOO/INDEX_AXJO")
    # Canada
    EI.TSX <- Quandl("YAHOO/INDEX_GSPTSE")
    # USA
    EI.USA <- Quandl("YAHOO/INDEX_GSPC")
    # China
    EI.SSEC <- Quandl("YAHOO/INDEX_SSEC")
    # Mexico
    EI.MXX <- Quandl("YAHOO/INDEX_MXX")
    # Germany
    EI.GDAXI <- Quandl("YAHOO/INDEX_GDAXI")
    # Japan
    EI.NIKKEI <- Quandl("NIKKEI/INDEX")
    # South Korea
    EI.KS11 <- Quandl("YAHOO/INDEX_KS11")
    # France
    EI.FCHI <- Quandl("YAHOO/INDEX_FCHI")
    # Switzerland
    EI.SSMI <- Quandl("YAHOO/INDEX_SSMI")
    # India
    EI.BSESN <- Quandl("YAHOO/INDEX_BSESN")
    # Brazil
    EI.BCB <- Quandl("BCB/7")
    # Netherlands
    EI.AEX <- Quandl("YAHOO/INDEX_AEX")
    # Germany
    EI.GDAXI <- Quandl("YAHOO/INDEX_GDAXI")
    # Singapore
    EI.STI <- Quandl("YAHOO/INDEX_STI")
    # New Zealand
    EI.NZ50 <- Quandl("YAHOO/INDEX_NZ50")
    # Indonesia
    EI.JKSE <- Quandl("YAHOO/INDEX_JKSE")
    # Malaysia
    # India
    # Peru
    # Thailand
    # Vietnam
    # UK
    # Italy

    all.equity.index <- merge(as.xts(EI.ASX$`Adjusted Close`, EI.ASX$Date)
                              ,as.xts(EI.TSX$`Adjusted Close`, EI.TSX$Date)
                              ,as.xts(EI.USA$`Adjusted Close`, EI.USA$Date)
                              ,as.xts(EI.SSEC$`Adjusted Close`, EI.SSEC$Date)
                              ,as.xts(EI.MXX$`Adjusted Close`, EI.MXX$Date)
                              ,as.xts(EI.GDAXI$`Adjusted Close`, EI.GDAXI$Date)
                              ,as.xts(EI.NIKKEI$`Close Price`, EI.NIKKEI$Date)
                              ,as.xts(EI.KS11$`Adjusted Close`, EI.KS11$Date)
                              ,as.xts(EI.FCHI$`Adjusted Close`, EI.FCHI$Date)
                              ,as.xts(EI.SSMI$`Adjusted Close`, EI.SSMI$Date)
                              ,as.xts(EI.BSESN$`Adjusted Close`, EI.BSESN$Date)
                              ,as.xts(EI.BCB$Value, EI.BCB$Date)
                              ,as.xts(EI.AEX$`Adjusted Close`, EI.AEX$Date)
                              ,as.xts(EI.GDAXI$`Adjusted Close`, EI.GDAXI$Date)
                              ,as.xts(EI.STI$`Adjusted Close`, EI.STI$Date)
                              ,as.xts(EI.NZ50$`Adjusted Close`, EI.NZ50$Date)
                              ,as.xts(EI.JKSE$`Adjusted Close`, EI.JKSE$Date))
    names(all.equity.index) <- c("ASX", "TSX", "SP500", "SSEC", "MXX", "GDAXI",
                                 "NIKKEI", "KS11", "FCHI", "SSMI", "BSESN", "BCB",
                                 "AEX", "GDAXI", "STI", "NZ50", "JKSE")
    Quandl.EquityIndex <- na.omit(all.equity.index)


    # Interest Rates ----------------------------------------------------------
    cat('\nLoading Quandl Government Bond Yield...\n')
    Quandl.YC.2Y <- Quandl(paste0("YC/",QuandlSymbol1, "2Y"), collapse = 'daily')
    Quandl.YC.2Y = convertToXts(Quandl.YC.2Y)
    colnames(Quandl.YC.2Y) = 'Quandl.YC.2Y'

    Quandl.YC.3Y <- Quandl(paste0("YC/",QuandlSymbol1, "3Y"), collapse = 'daily')
    Quandl.YC.3Y = convertToXts(Quandl.YC.3Y)
    colnames(Quandl.YC.3Y) = 'Quandl.YC.3Y'

    Quandl.YC.5Y <- Quandl(paste0("YC/",QuandlSymbol1, "5Y"), collapse = 'daily')
    Quandl.YC.5Y = convertToXts(Quandl.YC.5Y)
    colnames(Quandl.YC.5Y) = 'Quandl.YC.5Y'

    Quandl.YC.10Y <- Quandl(paste0("YC/",QuandlSymbol1, "10Y"), collapse = 'daily')
    Quandl.YC.10Y = convertToXts(Quandl.YC.10Y)
    colnames(Quandl.YC.10Y) = 'Quandl.YC.10Y'

    Quandl.YC.Bond = na.omit(merge(Quandl.YC.2Y, Quandl.YC.3Y, Quandl.YC.5Y, Quandl.YC.10Y))

    Quandl.YC.2Y <- Quandl(paste0("YC/",QuandlSymbol2, "2Y"), collapse = 'daily')
    Quandl.YC.2Y = convertToXts(Quandl.YC.2Y)
    colnames(Quandl.YC.2Y) = 'Quandl.YC.2Y'

    Quandl.YC.3Y <- Quandl(paste0("YC/",QuandlSymbol2, "3Y"), collapse = 'daily')
    Quandl.YC.3Y = convertToXts(Quandl.YC.3Y)
    colnames(Quandl.YC.3Y) = 'Quandl.YC.3Y'

    Quandl.YC.5Y <- Quandl(paste0("YC/",QuandlSymbol2, "5Y"), collapse = 'daily')
    Quandl.YC.5Y = convertToXts(Quandl.YC.5Y)
    colnames(Quandl.YC.5Y) = 'Quandl.YC.5Y'

    Quandl.YC.10Y <- Quandl(paste0("YC/",QuandlSymbol2, "10Y"), collapse = 'daily')
    Quandl.YC.10Y = convertToXts(Quandl.YC.10Y)
    colnames(Quandl.YC.10Y) = 'Quandl.YC.10Y'

    Quandl.YC.Bond2 = na.omit(merge(Quandl.YC.2Y, Quandl.YC.3Y, Quandl.YC.5Y, Quandl.YC.10Y))

    # Commodities -------------------------------------------------------------
    cat('\nLoading Comodity...\n')

    # Eco Calendars -----------------------------------------------------------
    cat('\nLoading Economic Calendars...\n')

    # Fundamental Data --------------------------------------------------------3
    cat('\nLoading Fundamental Data...\n')


    cat('\nEnsembling Final Data Output...\n')
    res = list(IB.BIDASK = IB.BIDASK,
               IB.MID = IB.MID,
               IB.BID = IB.BID,
               IB.ASK = IB.ASK,
               IB.SPREADS = IB.SPREADS,
               IB.SPREADS.Perc = IB.SPREADS.Perc,
               IB.VOLATILITY = IB.VOLATILITY,
               OA.BIDASK = OA.BIDASK,
               OA.SPREADS = OA.SPREADS,
               OA.SPREADS.Perc = OA.SPREADS.Perc,
               OA.VOLUME = OA.VOLUME,
               OA.HistPos = OA.HistPos,
               OA.CBOT = OA.CBOT,
               Oanda.Autochartist = Oanda.Autochartist,
               Quandl.EquityIndex = Quandl.EquityIndex,
               Quandl.YC.Bond = Quandl.YC.Bond,
               Quandl.YC.Bond2 = Quandl.YC.Bond2
    )
    twsDisconnect(tws)
    return(res)
}




#' Prepare all required datasets for Oanda FX daily Modeling
#'
#' Prepare all required datasets for Oanda FX daily Modeling.
#'
#' @param oanda.count length of data to retreive
#' @param Cur1 currency one
#' @param Cur2 currency two
#' @param oanda.granularity barsize of Oanda data
#' @param QuandlSymbol1 currency one for Quandl
#' @param QuandlSymbol2 currency two for Quandl
#'
#' @return A \code{list} of \code{xts}
#'
#' @examples
#' res = prepareForexOandaData(oanda.count = 2500, Cur1 = "USD", Cur2 = "CAD", oanda.granularity = 'D', QuandlSymbol1 = 'CAN', QuandlSymbol2 = 'USA')
#'
#' @export
prepareForexOandaData <- function(oanda.count = 250, Cur1 = "USD", Cur2 = "CAD", oanda.granularity = 'D', QuandlSymbol1 = 'CAN', QuandlSymbol2 = 'USA'){
    library(RQuantAPI)

    convertToXts <- function(cm, tzone = ""){
        dts <- structure(as.numeric(as.POSIXlt(gsub("(\\d{4})(\\d{2})(\\d{2})",
                                                    "\\1-\\2-\\3", cm[, 1], perl = TRUE))),
                         class = c("POSIXct","POSIXt"), tzone = tzone)
        if(ncol(cm) > 2){
            x <- xts(as.matrix(apply(cm[, -1], 2, as.numeric)), order.by = dts, tzone = tzone)
        }else{
            x <- xts(as.numeric(cm[, -1]), order.by = dts, tzone = tzone)
        }

        return(x)
    }

    instrument = paste0(Cur1, '_', Cur2)

    # Oanda OHLC --------------------------------------------------------------
    cat('\nLoading Oanda OHLC...\n')
    OA.ALL = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = instrument,
                                       price = 'MBA', granularity = oanda.granularity, count = oanda.count)
    OA.ALL = convertToXts(OA.ALL[, -1])
    colnames(OA.ALL) = c('OA.Volume', 'OA.Ask.Close', 'OA.Ask.High', 'OA.Ask.Low', 'OA.Ask.Open'
                         , 'OA.Bid.Close', 'OA.Bid.High', 'OA.Bid.Low', 'OA.Bid.Open'
                         , 'OA.Mid.Close', 'OA.Mid.High', 'OA.Mid.Low', 'OA.Mid.Open')
    OA.MID = OA.ALL[, c('OA.Mid.Close', 'OA.Mid.High', 'OA.Mid.Low', 'OA.Mid.Open')]
    OA.BID = OA.ALL[, c('OA.Bid.Close', 'OA.Bid.High', 'OA.Bid.Low', 'OA.Bid.Open')]
    OA.ASK = OA.ALL[, c('OA.Ask.Close', 'OA.Ask.High', 'OA.Ask.Low', 'OA.Ask.Open')]


    # Oanda Spreads -----------------------------------------------------------
    # OA.SPREADS = SpreadsOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID,
    #                           INSTRUMENTS = instrument, PERIOD = '1 year')
    cat('\nLoading Oanda Spreads...\n')
    OA.SPREADS = OA.ALL[, 2:5] - OA.ALL[, 6:9]
    OA.SPREADS.Perc = ((OA.ALL[, 2:5] - OA.ALL[, 6:9])/((OA.ALL[, 2:5] + OA.ALL[, 6:9])/2)) * 100


    # Oanda Volume ------------------------------------------------------------
    cat('\nLoading Oanda Volume...\n')
    OA.VOLUME = OA.ALL[, 1]


    # Oanda Net Positions -----------------------------------------------------
    cat('\nLoading Oanda New Positions...\n')
    OA.HistPos <- HistoricalPositionOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID,
                                          INSTRUMENTS = instrument, PERIOD = '1 year')
    OA.HistPos = convertToXts(as.data.frame(OA.HistPos)[, -1])
    colnames(OA.HistPos) = c('OA.LongPosRatio', 'OA.ExRate')
    OA.HistPos$OA.PosCorr = rollapply(OA.HistPos, by.column = F, width = 20, function(x) cor(x)[1,2])


    # Oanda CBOT --------------------------------------------------------------
    cat('\nLoading Oanda / Quandl CBOT...\n')
    OA.CBOT = COT.Oanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = instrument)
    OA.CBOT = convertToXts(as.data.frame(OA.CBOT)[, -6])

    # Oanda Autochartist ------------------------------------------------------
    cat('\nLoading Oanda / Autochartist Signals...\n')
    Oanda.Autochartist = Autochartist.Oanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN,
                                            INSTRUMENTS = instrument, PERIOD = '1 week')

    # Quandl Indexes ----------------------------------------------------------
    cat('\nLoading Quandl Equity Indexes...\n')
    QuandlConnect()
    # AU
    EI.ASX <- Quandl("YAHOO/INDEX_AXJO")
    # Canada
    EI.TSX <- Quandl("YAHOO/INDEX_GSPTSE")
    # USA
    EI.USA <- Quandl("YAHOO/INDEX_GSPC")
    # China
    EI.SSEC <- Quandl("YAHOO/INDEX_SSEC")
    # Mexico
    EI.MXX <- Quandl("YAHOO/INDEX_MXX")
    # Germany
    EI.GDAXI <- Quandl("YAHOO/INDEX_GDAXI")
    # Japan
    EI.NIKKEI <- Quandl("NIKKEI/INDEX")
    # South Korea
    EI.KS11 <- Quandl("YAHOO/INDEX_KS11")
    # France
    EI.FCHI <- Quandl("YAHOO/INDEX_FCHI")
    # Switzerland
    EI.SSMI <- Quandl("YAHOO/INDEX_SSMI")
    # India
    EI.BSESN <- Quandl("YAHOO/INDEX_BSESN")
    # Brazil
    EI.BCB <- Quandl("BCB/7")
    # Netherlands
    EI.AEX <- Quandl("YAHOO/INDEX_AEX")
    # Germany
    EI.GDAXI <- Quandl("YAHOO/INDEX_GDAXI")
    # Singapore
    EI.STI <- Quandl("YAHOO/INDEX_STI")
    # New Zealand
    EI.NZ50 <- Quandl("YAHOO/INDEX_NZ50")
    # Indonesia
    EI.JKSE <- Quandl("YAHOO/INDEX_JKSE")
    # Malaysia
    # India
    # Peru
    # Thailand
    # Vietnam
    # UK
    # Italy

    all.equity.index <- merge(as.xts(EI.ASX$`Adjusted Close`, EI.ASX$Date)
                              ,as.xts(EI.TSX$`Adjusted Close`, EI.TSX$Date)
                              ,as.xts(EI.USA$`Adjusted Close`, EI.USA$Date)
                              ,as.xts(EI.SSEC$`Adjusted Close`, EI.SSEC$Date)
                              ,as.xts(EI.MXX$`Adjusted Close`, EI.MXX$Date)
                              ,as.xts(EI.GDAXI$`Adjusted Close`, EI.GDAXI$Date)
                              ,as.xts(EI.NIKKEI$`Close Price`, EI.NIKKEI$Date)
                              ,as.xts(EI.KS11$`Adjusted Close`, EI.KS11$Date)
                              ,as.xts(EI.FCHI$`Adjusted Close`, EI.FCHI$Date)
                              ,as.xts(EI.SSMI$`Adjusted Close`, EI.SSMI$Date)
                              ,as.xts(EI.BSESN$`Adjusted Close`, EI.BSESN$Date)
                              ,as.xts(EI.BCB$Value, EI.BCB$Date)
                              ,as.xts(EI.AEX$`Adjusted Close`, EI.AEX$Date)
                              ,as.xts(EI.GDAXI$`Adjusted Close`, EI.GDAXI$Date)
                              ,as.xts(EI.STI$`Adjusted Close`, EI.STI$Date)
                              ,as.xts(EI.NZ50$`Adjusted Close`, EI.NZ50$Date)
                              ,as.xts(EI.JKSE$`Adjusted Close`, EI.JKSE$Date))
    names(all.equity.index) <- c("ASX", "TSX", "SP500", "SSEC", "MXX", "GDAXI",
                                 "NIKKEI", "KS11", "FCHI", "SSMI", "BSESN", "BCB",
                                 "AEX", "GDAXI", "STI", "NZ50", "JKSE")
    Quandl.EquityIndex <- na.omit(all.equity.index)


    # Interest Rates ----------------------------------------------------------
    cat('\nLoading Quandl Government Bond Yield...\n')
    Quandl.YC.2Y <- Quandl(paste0("YC/",QuandlSymbol1, "2Y"), collapse = 'daily')
    Quandl.YC.2Y = convertToXts(Quandl.YC.2Y)
    colnames(Quandl.YC.2Y) = 'Quandl.YC.2Y'

    Quandl.YC.3Y <- Quandl(paste0("YC/",QuandlSymbol1, "3Y"), collapse = 'daily')
    Quandl.YC.3Y = convertToXts(Quandl.YC.3Y)
    colnames(Quandl.YC.3Y) = 'Quandl.YC.3Y'

    Quandl.YC.5Y <- Quandl(paste0("YC/",QuandlSymbol1, "5Y"), collapse = 'daily')
    Quandl.YC.5Y = convertToXts(Quandl.YC.5Y)
    colnames(Quandl.YC.5Y) = 'Quandl.YC.5Y'

    Quandl.YC.10Y <- Quandl(paste0("YC/",QuandlSymbol1, "10Y"), collapse = 'daily')
    Quandl.YC.10Y = convertToXts(Quandl.YC.10Y)
    colnames(Quandl.YC.10Y) = 'Quandl.YC.10Y'

    Quandl.YC.Bond = na.omit(merge(Quandl.YC.2Y, Quandl.YC.3Y, Quandl.YC.5Y, Quandl.YC.10Y))

    Quandl.YC.2Y <- Quandl(paste0("YC/",QuandlSymbol2, "2Y"), collapse = 'daily')
    Quandl.YC.2Y = convertToXts(Quandl.YC.2Y)
    colnames(Quandl.YC.2Y) = 'Quandl.YC.2Y'

    Quandl.YC.3Y <- Quandl(paste0("YC/",QuandlSymbol2, "3Y"), collapse = 'daily')
    Quandl.YC.3Y = convertToXts(Quandl.YC.3Y)
    colnames(Quandl.YC.3Y) = 'Quandl.YC.3Y'

    Quandl.YC.5Y <- Quandl(paste0("YC/",QuandlSymbol2, "5Y"), collapse = 'daily')
    Quandl.YC.5Y = convertToXts(Quandl.YC.5Y)
    colnames(Quandl.YC.5Y) = 'Quandl.YC.5Y'

    Quandl.YC.10Y <- Quandl(paste0("YC/",QuandlSymbol2, "10Y"), collapse = 'daily')
    Quandl.YC.10Y = convertToXts(Quandl.YC.10Y)
    colnames(Quandl.YC.10Y) = 'Quandl.YC.10Y'

    Quandl.YC.Bond2 = na.omit(merge(Quandl.YC.2Y, Quandl.YC.3Y, Quandl.YC.5Y, Quandl.YC.10Y))

    # Commodities -------------------------------------------------------------
    cat('\nLoading Comodity...\n')

    # Eco Calendars -----------------------------------------------------------
    cat('\nLoading Economic Calendars...\n')

    # Fundamental Data --------------------------------------------------------3
    cat('\nLoading Fundamental Data...\n')


    cat('\nEnsembling Final Data Output...\n')
    res = list(OA.MID = OA.MID,
               OA.BID = OA.BID,
               OA.ASK = OA.ASK,
               OA.SPREADS = OA.SPREADS,
               OA.SPREADS.Perc = OA.SPREADS.Perc,
               OA.VOLUME = OA.VOLUME,
               OA.HistPos = OA.HistPos,
               OA.CBOT = OA.CBOT,
               Oanda.Autochartist = Oanda.Autochartist,
               Quandl.EquityIndex = Quandl.EquityIndex,
               Quandl.YC.Bond = Quandl.YC.Bond,
               Quandl.YC.Bond2 = Quandl.YC.Bond2
    )

    return(res)
}



#' Prepare Interactive Brokers FX data
#'
#' Prepare Interactive Brokers FX data
#'
#' @param ib.duration length of data to retreive from Interactive Broker
#' @param ib.barsize the barsize of IB data
#' @param Cur1 currency one
#' @param Cur2 currency two
#' @param ibAcc account type of Interactive Brokers (live or paper)
#' @param midOnly only retreive mid prices
#'
#' @return A \code{list} of \code{xts}
#'
#' @export
prepareIBForexPrices <- function(ib.duration = "1 Y", ib.barsize = "1 day", Cur1 = "USD", Cur2 = "CAD", ibAcc = 'paper', midOnly = TRUE){

    library(IBrokers)
    library(RQuantAPI)

    if(ibAcc == 'live'){
        tws <- twsConnect(port = 7496, clientId = 999)
    }else{
        tws <- twsConnect(port = 7497, clientId = 998)
    }

    # IB OHLC -----------------------------------------------------------------
    cat('\nLoading Interactive Brokers OHLC...\n')
    ccy <- reqContractDetails(tws, twsCurrency(Cur1, Cur2))[[1]]$contract
    cols = c('Open', 'High', 'Low', 'Close')

    if(midOnly){
        # Mid
        IB.MID <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                                    duration = ib.duration, useRTH = "1", whatToShow='MIDPOINT')
        IB.MID = IB.MID[, 1:4]
        IB.MID = IB.MID[!duplicated(index(IB.MID)), ]
        colnames(IB.MID) = paste0('IB.M.', cols)

        res = list(IB.MID = IB.MID)

    }else{
        # Bid Ask
        IB.BIDASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                                       duration = ib.duration, useRTH = "1", whatToShow='BID_ASK')
        IB.BIDASK = IB.BIDASK[, 1:4]
        IB.BIDASK = IB.BIDASK[!duplicated(index(IB.BIDASK)), ]
        colnames(IB.BIDASK) = paste0('IB.BA.', cols)

        # Mid
        IB.MID <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                                    duration = ib.duration, useRTH = "1", whatToShow='MIDPOINT')
        IB.MID = IB.MID[, 1:4]
        IB.MID = IB.MID[!duplicated(index(IB.MID)), ]
        colnames(IB.MID) = paste0('IB.M.', cols)

        # Bid
        IB.BID <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                                    duration = ib.duration, useRTH = "1", whatToShow='BID')
        IB.BID = IB.BID[, 1:4]
        IB.BID = IB.BID[!duplicated(index(IB.BID)), ]
        colnames(IB.BID) = paste0('IB.B.', cols)

        # Ask
        IB.ASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
                                    duration = ib.duration, useRTH = "1", whatToShow='ASK')
        IB.ASK = IB.ASK[, 1:4]
        IB.ASK = IB.ASK[!duplicated(index(IB.ASK)), ]
        colnames(IB.ASK) = paste0('IB.A.', cols)


        # IB Spreads --------------------------------------------------------------
        cat('\nLoading Interactive Brokers Spreads...\n')
        IB.SPREADS = IB.ASK - IB.BID
        colnames(IB.SPREADS) = paste0('IB.SP.', cols)
        IB.SPREADS.Perc = (IB.SPREADS / IB.MID) * 100
        colnames(IB.SPREADS.Perc) = paste0('IB.SPP.', cols)


        # IB Volatility -----------------------------------------------------------
        # cat('\nLoading Interactive Brokers Volatility...\n')
        # IB.VOLATILITY <- reqHistoricalData(conn = tws, Contract = ccy, barSize = ib.barsize,
        #                                    duration = ib.duration, useRTH = "1", whatToShow='HISTORICAL_VOLATILITY')
        # IB.VOLATILITY = IB.VOLATILITY[,6]
        # IB.VOLATILITY = IB.VOLATILITY[!duplicated(index(IB.VOLATILITY)), ]
        # colnames(IB.VOLATILITY) = paste0('IB.Volatility.',Cur1, '.', Cur2)


        cat('\nEnsembling Final Data Output...\n')
        res = list(IB.BIDASK = IB.BIDASK,
                   IB.MID = IB.MID,
                   IB.BID = IB.BID,
                   IB.ASK = IB.ASK,
                   IB.SPREADS = IB.SPREADS,
                   IB.SPREADS.Perc = IB.SPREADS.Perc
                   # IB.VOLATILITY = IB.VOLATILITY
        )
    }


    twsDisconnect(tws)
    return(res)
}






#' Prepare all required datasets for Oanda FX daily Modeling
#'
#' Prepare all required datasets for Oanda FX daily Modeling.
#'
#' @param ACCOUNT_TYPE Account type (e.g. "real" or "paper")
#' @param ACCESS_TOKEN Account API Token
#' @param oanda.count length of data to retreive
#' @param Cur1 currency one
#' @param Cur2 currency two
#' @param oanda.granularity barsize of Oanda data
#'
#' @return A \code{list} of \code{xts}
#'
#' @examples
#' res = prepareForexOandaPrices(oanda.count = 2500, Cur1 = "USD", Cur2 = "CAD", oanda.granularity = 'D')
#'
#' @export
prepareForexOandaPrices <- function(ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE, ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN, oanda.count = 250, Cur1 = "USD", Cur2 = "CAD", oanda.granularity = 'D'){
    library(RQuantAPI)

    convertToXts <- function(cm, tzone = ""){
        dts <- structure(as.numeric(as.POSIXlt(gsub("(\\d{4})(\\d{2})(\\d{2})",
                                                    "\\1-\\2-\\3", cm[, 1], perl = TRUE))),
                         class = c("POSIXct","POSIXt"), tzone = tzone)

        dts[wday(dts) == 1] = dts[wday(dts) == 1] - 3600 * 48 # <=============================== Check if it's Oanda's Bug

        if(ncol(cm) > 2){
            x <- xts(as.matrix(apply(cm[, -1], 2, as.numeric)), order.by = dts, tzone = tzone)
        }else{
            x <- xts(as.numeric(cm[, -1]), order.by = dts, tzone = tzone)
        }

        return(x)
    }

    instrument = paste0(Cur1, '_', Cur2)

    # Oanda OHLC --------------------------------------------------------------
    # cat('\nLoading Oanda OHLC...\n')
    OA.ALL = getOandaInstrumentCandles(ACCOUNT_TYPE, ACCESS_TOKEN, INSTRUMENTS = instrument,
                                       price = 'MBA', granularity = oanda.granularity, count = oanda.count)

    OA.ALL = convertToXts(OA.ALL[, -1])



    colnames(OA.ALL) = c('OA.Volume', 'OA.Bid.Open', 'OA.Bid.High', 'OA.Bid.Low', 'OA.Bid.Close'
                         , 'OA.Ask.Open', 'OA.Ask.High', 'OA.Ask.Low', 'OA.Ask.Close'
                         , 'OA.Mid.Open', 'OA.Mid.High', 'OA.Mid.Low', 'OA.Mid.Close')
    OA.MID = OA.ALL[, c('OA.Mid.Close', 'OA.Mid.High', 'OA.Mid.Low', 'OA.Mid.Open', 'OA.Volume')]
    OA.BID = OA.ALL[, c('OA.Bid.Close', 'OA.Bid.High', 'OA.Bid.Low', 'OA.Bid.Open', 'OA.Volume')]
    OA.ASK = OA.ALL[, c('OA.Ask.Close', 'OA.Ask.High', 'OA.Ask.Low', 'OA.Ask.Open', 'OA.Volume')]


    # Oanda Spreads -----------------------------------------------------------
    # OA.SPREADS = SpreadsOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID,
    #                           INSTRUMENTS = instrument, PERIOD = '1 year')
    # cat('\nLoading Oanda Spreads...\n')
    # OA.SPREADS = OA.ALL[, 2:5] - OA.ALL[, 6:9]
    # OA.SPREADS.Perc = ((OA.ALL[, 2:5] - OA.ALL[, 6:9])/((OA.ALL[, 2:5] + OA.ALL[, 6:9])/2)) * 100


    # Oanda Volume ------------------------------------------------------------
    # cat('\nLoading Oanda Volume...\n')
    # OA.VOLUME = OA.ALL[, 1]


    # Oanda Net Positions -----------------------------------------------------
    # cat('\nLoading Oanda New Positions...\n')
    # OA.HistPos <- HistoricalPositionOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID,
    #                                       INSTRUMENTS = instrument, PERIOD = '1 year')
    # OA.HistPos = convertToXts(as.data.frame(OA.HistPos)[, -1])
    # colnames(OA.HistPos) = c('OA.LongPosRatio', 'OA.ExRate')
    # OA.HistPos$OA.PosCorr = rollapply(OA.HistPos, by.column = F, width = 20, function(x) cor(x)[1,2])



    # cat('\nEnsembling Final Data Output...\n')
    res = list(OA.MID = OA.MID,
               OA.BID = OA.BID,
               OA.ASK = OA.ASK,
               OA.ALL = OA.ALL
               # OA.SPREADS = OA.SPREADS,
               # OA.SPREADS.Perc = OA.SPREADS.Perc,
               # OA.VOLUME = OA.VOLUME,
               # OA.HistPos = OA.HistPos
    )

    return(res)
}
