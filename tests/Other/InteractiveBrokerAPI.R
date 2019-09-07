# The entire mechanism (twsCALLBACK -> processMsg -> eWrapper) is modeled after the official API.
############ IB System Status ####################################################################
# https://www.interactivebrokers.com/en/?f=%2Fen%2Fsoftware%2FsystemStatus.php%3Fib_entity%3Dllc #
##################################################################################################

# Real Account: 7496
# Paper Account: 7497
library(IBrokers)
tws <- twsConnect(port = 7497, clientId = 998)
reqCurrentTime(tws)
serverVersion(tws)

USDCAD <- getIBForexHist(tws, "1 Y", "1 day", "USD", "CAD")
head(USDCAD$BIDASK) # candle
USDAUD <- getIBForexHist(tws, "1 Y", "1 day", "AUD", "USD")

twsDisconnect(tws)


# Contracts ---------------------------------------------------------------
currency <- twsCurrency("AUD")
currency.details <- reqContractDetails(tws, currency)
currency.contract <- as.twsContract(currency.details)[[1]] # reqContractDetails(tws, currency)[[1]]$contract


# Market Data -------------------------------------------------------------
reqMktData(tws, currency, tickerId = '1', snapshot = TRUE)
reqMktData(tws, currency, tickerId = '1',
           eventWrapper = eWrapper(TRUE), # methods to handle messages
           timeStamp = NULL)
# contract = twsContract(conId = '1009', symbol = 'BT:BT_ALL', sectype = 'NEWS', exch = 'BT',
#                        primary = '', expiry = '', strike = '', currency = '', right = '', local = '',
#                        multiplier = '', combo_legs_desc = '', comboleg = '', include_expired = '')
# reqMktData(tws, contract, tickerId = 'mdoff,292', snapshot = FALSE)
# cancelMktData(tws, '1')


# Market Depth ------------------------------------------------------------
reqMktDepth(tws, currency, tickerId = '1')
# cancelMktDepth(tws, '1')


# Real time bar -----------------------------------------------------------
reqRealTimeBars(tws, currency)
# cancelRealTimeBars()


# Historical Data ---------------------------------------------------------
reqHistoricalData(tws, Contract=currency, barSize = '1 day',
                  duration = '1 Y', useRTH = "1", whatToShow='MIDPOINT')
# reqHistory(tws, currency.contract)
USDAUD <- getIBForexHist(tws, "10 Y", "1 day", "AUD", "USD")


# Fundamental Data --------------------------------------------------------
reqIBFundamentalData(twsconn = tws, reqId = '1', contract = twsSTK('AAPL'), reportType = 'RESC')
# eWrapper


# News Bulletins ----------------------------------------------------------
reqIBNewsBulletins(tws, allMsgs=TRUE)
# cancelNewsBulletins(tws)


# Orders ------------------------------------------------------------------
Order.Id = reqIds(tws)
IB.Contract = twsCurrency("AUD")
IB.Order = twsOrder(orderId = Order.Id, action = 'BUY', totalQuantity = 10, orderType = 'MKT')
Exe.IB.Order <- placeOrder(twsconn = tws, Contract = IB.Contract, Order = IB.Order)
cancelOrder(twsconn = tws, '4')
reqOpenOrders(twsconn = tws)


# Account -----------------------------------------------------------------
IB.Acc = reqAccountUpdates(tws, subscribe = T)        # this will return a AccountUpdate object
twsPortfolioValue(x = IB.Acc)
# .reqAccountUpdates(tws)       # this will return immediately
# .reqAccountUpdates(tws, FALSE)  # cancel the request
# cancelAccountUpdates(tws)     # the same


# Executions --------------------------------------------------------------
reqIBExecutions(twsconn = tws, reqId = '4', ExecutionFilter = '')


# eWrapper ----------------------------------------------------------------
myWrapper <- eWrapper()
str(myWrapper)
# remove tickPrice action
myWrapper$tickPrice <- function(msg, timestamp, file, ...) {}
# add new tickPrice action
myWrapper$tickPrice <- function(msg, timestamp, file, ...) { cat("tickPrice",msg) }
# add new data into the object, and retrieve
myWrapper$assign.Data("myData", 1010)
myWrapper$get.Data("myData")



# Financial Advisors ------------------------------------------------------
# ?reqManagedAccts(tws)
# requestFA(tws, 'XML')


# Scanner -----------------------------------------------------------------
# reqScannerParameters(tws)
# reqScannerSubscription(tws)

