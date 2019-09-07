reqMktData(tws, currency.contract, tickerId = '1',
           eventWrapper = eWrapper(TRUE), # methods to handle messages
           timeStamp = NULL)


# twsCALLBACK Function ----------------------------------------------------
twsCALLBACK = function (twsCon, eWrapper, timestamp, file, playback = 1, ...)
{
    if (missing(eWrapper))
        eWrapper <- eWrapper()
    con <- twsCon[[1]]
    if (inherits(twsCon, "twsPlayback")) {
        sys.time <- NULL
        while (TRUE) {
            if (!is.null(timestamp)) {
                last.time <- sys.time
                sys.time <- as.POSIXct(strptime(paste(readBin(con,
                                                              character(), 2), collapse = " "), timestamp))
                if (!is.null(last.time)) {
                    Sys.sleep((sys.time - last.time) * playback)
                }
                curMsg <- readBin(con, character(), 1)
                if (length(curMsg) < 1)
                    next
                processMsg(curMsg, con, eWrapper, format(sys.time,
                                                         timestamp), file, ...)
            }
            else {
                curMsg <- readBin(con, character(), 1)
                if (length(curMsg) < 1)
                    next
                processMsg(curMsg, con, eWrapper, timestamp,
                           file, ...)
                if (curMsg == .twsIncomingMSG$REAL_TIME_BARS)
                    Sys.sleep(5 * playback)
            }
        }
    }
    else {
        tryCatch(while (isConnected(twsCon)) {
            if (!socketSelect(list(con), FALSE, 0.25))  # Wait on Connection
                next
            curMsg <- readBin(con, "character", 1L) # Read Header
            if (!is.null(timestamp)) {
                processMsg(curMsg, con, eWrapper, format(Sys.time(),
                                                         timestamp), file, twsCon, ...)
            }
            else {
                processMsg(curMsg, con, eWrapper, timestamp,  # Process Message
                           file, twsCon, ...)
            }
        }, error = function(e) {
            close(twsCon)
            stop("IB connection error. Connection closed", call. = FALSE)
        })
    }
}



# processMsg Function -----------------------------------------------------
processMsg = function (curMsg, con, eWrapper, timestamp, file, twsconn, ...)
{
    if (curMsg == .twsIncomingMSG$TICK_PRICE) { # New Tick Pricess message
        msg <- readBin(con, "character", 6) # Read fixed size message from connection
        eWrapper$tickPrice(curMsg, msg, timestamp, file, ...) # Process message
    }
    else if (curMsg == .twsIncomingMSG$TICK_SIZE) {
        msg <- readBin(con, "character", 4)
        eWrapper$tickSize(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ORDER_STATUS) {
        msg <- readBin(con, "character", 11)
        eWrapper$orderStatus(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ERR_MSG) {
        msg <- readBin(con, "character", 4)
        eWrapper$errorMessage(curMsg, msg, timestamp, file,
                              twsconn, ...)
    }
    else if (curMsg == .twsIncomingMSG$OPEN_ORDER) {
        msg <- readBin(con, "character", 84)
        eWrapper$openOrder(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCT_VALUE) {
        msg <- readBin(con, "character", 5)
        eWrapper$updateAccountValue(curMsg, msg, timestamp,
                                    file, ...)
    }
    else if (curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
        msg <- readBin(con, "character", 18)
        eWrapper$updatePortfolio(curMsg, msg, timestamp, file,
                                 ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {
        msg <- readBin(con, "character", 2)
        eWrapper$updateAccountTime(curMsg, msg, timestamp, file,
                                   ...)
    }
    else if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
        msg <- readBin(con, "character", 2)
        eWrapper$nextValidId(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
        msg <- readBin(con, "character", 28)
        eWrapper$contractDetails(curMsg, msg, timestamp, file,
                                 ...)
    }
    else if (curMsg == .twsIncomingMSG$EXECUTION_DATA) {
        msg <- readBin(con, "character", 24)
        eWrapper$execDetails(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$MARKET_DEPTH) {
        msg <- readBin(con, "character", 7)
        eWrapper$updateMktDepth(curMsg, msg, timestamp, file,
                                ...)
    }
    else if (curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {
        msg <- readBin(con, "character", 8)
        eWrapper$updateMktDepthL2(curMsg, msg, timestamp, file,
                                  ...)
    }
    else if (curMsg == .twsIncomingMSG$NEWS_BULLETINS) {
        msg <- readBin(con, "character", 5)
        eWrapper$newsBulletins(curMsg, msg, timestamp, file,
                               ...)
    }
    else if (curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
        msg <- readBin(con, "character", 2)
        eWrapper$managedAccounts(curMsg, msg, timestamp, file,
                                 ...)
    }
    else if (curMsg == .twsIncomingMSG$RECEIVE_FA) {
        msg <- readBin(con, "character", 2)
        stop("xml data currently unsupported")
        eWrapper$receiveFA(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
        header <- readBin(con, character(), 5)
        nbin <- as.numeric(header[5]) * 9
        msg <- readBin(con, character(), nbin)
        eWrapper$historicalData(curMsg, msg, timestamp, file,
                                ...)
    }
    else if (curMsg == .twsIncomingMSG$BOND_CONTRACT_DATA) {
        warning("BOND_CONTRACT_DATA unimplemented as of yet")
        eWrapper$bondContractDetails(curMsg, msg, timestamp,
                                     file, ...)
    }
    else if (curMsg == .twsIncomingMSG$SCANNER_PARAMETERS) {
        version <- readBin(con, character(), 1L)
        msg <- readBin(con, raw(), 1000000L)
        eWrapper$scannerParameters(curMsg, msg, timestamp, file,
                                   ...)
    }
    else if (curMsg == .twsIncomingMSG$SCANNER_DATA) {
        cD <- twsContractDetails()
        version <- readBin(con, character(), 1L)
        tickerId <- readBin(con, character(), 1L)
        numberOfElements <- as.integer(readBin(con, character(),
                                               1L))
        for (i in 1:numberOfElements) {
            msg <- readBin(con, character(), 16L)
            rank <- msg[1]
            cD$contract$conId <- msg[2]
            cD$contract$symbol <- msg[3]
            cD$contract$sectype <- msg[4]
            cD$contract$expiry <- msg[5]
            cD$contract$strike <- msg[6]
            cD$contract$right <- msg[7]
            cD$contract$exch <- msg[8]
            cD$contract$currency <- msg[9]
            cD$contract$local <- msg[10]
            cD$marketName <- msg[11]
            cD$tradingClass <- msg[12]
            distance <- msg[13]
            benchmark <- msg[14]
            projection <- msg[15]
            legsStr <- msg[16]
            eWrapper$scannerData(curMsg, tickerId, rank, cD,
                                 distance, benchmark, projection, legsStr)
        }
    }
    else if (curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {
        msg <- readBin(con, "character", 11)
        eWrapper$tickOptionComputation(curMsg, msg, timestamp,
                                       file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_GENERIC) {
        msg <- readBin(con, "character", 4)
        eWrapper$tickGeneric(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_STRING) {
        msg <- readBin(con, "character", 4)
        eWrapper$tickString(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_EFP) {
        msg <- readBin(con, "character", 10)
        eWrapper$tickEFP(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$CURRENT_TIME) {
        msg <- readBin(con, "character", 2)
        eWrapper$currentTime(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
        msg <- readBin(con, "character", 10)
        eWrapper$realtimeBars(curMsg, msg, timestamp, file,
                              ...)
    }
    else if (curMsg == .twsIncomingMSG$FUNDAMENTAL_DATA) {
        msg <- readBin(con, "character", 3)
        eWrapper$fundamentalData(curMsg, msg, timestamp, file,
                                 ...)
    }
    else if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
        msg <- readBin(con, "character", 2)
        eWrapper$contractDetailsEnd(curMsg, msg, timestamp,
                                    file, ...)
    }
    else if (curMsg == .twsIncomingMSG$OPEN_ORDER_END) {
        msg <- readBin(con, "character", 1)
        eWrapper$openOrderEnd(curMsg, msg, timestamp, file,
                              ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) {
        msg <- readBin(con, "character", 2)
        eWrapper$accountDownloadEnd(curMsg, msg, timestamp,
                                    file, ...)
    }
    else if (curMsg == .twsIncomingMSG$EXECUTION_DATA_END) {
        msg <- readBin(con, "character", 2)
        eWrapper$execDetailsEnd(curMsg, msg, timestamp, file,
                                ...)
    }
    else if (curMsg == .twsIncomingMSG$DELTA_NEUTRAL_VALIDATION) {
        msg <- readBin(con, "character", 5)
        eWrapper$deltaNeutralValidation(curMsg, msg, timestamp,
                                        file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {
        msg <- readBin(con, "character", 2)
        eWrapper$tickSnapshotEnd(curMsg, msg, timestamp, file,
                                 ...)
    }
    else {
        warning(paste("Unknown incoming message: ", curMsg,
                      ". Please reset connection", sep = ""), call. = FALSE)
    }
}



# eWrapper Function -------------------------------------------------------
eWrapper = function (debug = FALSE, errfile = stderr()) # Define custom message handling functions quickly and easily
{
    .Data <- new.env()
    get.Data <- function(x) get(x, .Data)
    assign.Data <- function(x, value) assign(x, value, .Data)
    remove.Data <- function(x) remove(x, .Data)
    if (is.null(debug)) {
        errorMessage <- function(curMsg, msg, timestamp, file,
                                 twsconn, ...) {
            cat(msg, "\n", file = errfile)
        }
        tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <- orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <- updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <- contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <- updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <- scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <- deltaNeutralValidation <- tickSnapshotEnd <- function(curMsg,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              msg, timestamp, file, ...) {
            c(curMsg, msg)
        }
    }
    else if (!debug) {
        tickPrice <- function(curMsg, msg, timestamp, file,
                              ...) {
            symbols <- get.Data("symbols")
            e_tick_price(NULL, msg, timestamp, file, symbols,
                         ...)
        }
        tickSize <- function(curMsg, msg, timestamp, file, ...) {
            symbols <- get.Data("symbols")
            e_tick_size(NULL, msg, timestamp, file, symbols,
                        ...)
        }
        tickOptionComputation <- function(curMsg, msg, timestamp,
                                          file, ...) {
            symbols <- get.Data("symbols")
            e_tick_option(NULL, msg, timestamp, file, symbols,
                          ...)
        }
        tickGeneric <- function(curMsg, msg, timestamp, file,
                                ...) {
            symbols <- get.Data("symbols")
            e_tick_generic(NULL, msg, timestamp, file, symbols,
                           ...)
        }
        tickString <- function(curMsg, msg, timestamp, file,
                               ...) {
            symbols <- get.Data("symbols")
            e_tick_string(NULL, msg, timestamp, file, symbols,
                          ...)
        }
        tickEFP <- function(curMsg, msg, timestamp, file, ...) {
            symbols <- get.Data("symbols")
            e_tick_EFP(NULL, msg, timestamp, file, symbols,
                       ...)
        }
        orderStatus <- function(curMsg, msg, timestamp, file,
                                ...) {
            e_order_status(curMsg, msg)
            c(curMsg, msg)
        }
        errorMessage <- function(curMsg, msg, timestamp, file,
                                 twsconn, ...) {
            if (msg[3] == "1100")
                twsconn$connected <- FALSE
            if (msg[3] %in% c("1101", "1102"))
                twsconn$connected <- TRUE
            cat("TWS Message:", msg, "\n")
        }
        openOrder <- function(curMsg, msg, timestamp, file,
                              ...) {
            c(curMsg, msg)
        }
        openOrderEnd <- function(curMsg, msg, timestamp, file,
                                 ...) {
            c(curMsg, msg)
        }
        updateAccountValue <- function(curMsg, msg, timestamp,
                                       file, ...) {
            c(curMsg, msg)
        }
        updatePortfolio <- function(curMsg, msg, timestamp,
                                    file, ...) {
            e_portfolio_value(curMsg, msg)
            c(curMsg, msg)
        }
        updateAccountTime <- function(curMsg, msg, timestamp,
                                      file, ...) {
            c(curMsg, msg)
        }
        accountDownloadEnd <- function(curMsg, msg, timestamp,
                                       file, ...) {
            c(curMsg, msg)
        }
        nextValidId <- function(curMsg, msg, timestamp, file,
                                ...) {
            c(curMsg, msg)
        }
        contractDetails <- function(curMsg, msg, timestamp,
                                    file, ...) {
            c(curMsg, msg)
        }
        bondContractDetails <- function(curMsg, msg, timestamp,
                                        file, ...) {
            c(curMsg, msg)
        }
        contractDetailsEnd <- function(curMsg, msg, timestamp,
                                       file, ...) {
            c(curMsg, msg)
        }
        execDetails <- function(curMsg, msg, timestamp, file,
                                ...) {
            e_execDetails(curMsg, msg, file, ...)
        }
        execDetailsEnd <- function(curMsg, msg, timestamp, file,
                                   ...) {
            c(curMsg, msg)
        }
        updateMktDepth <- function(curMsg, msg, timestamp, file,
                                   ...) {
            symbols <- get.Data("symbols")
            e_update_mkt_depth(NULL, msg, timestamp, file, symbols,
                               ...)
        }
        updateMktDepthL2 <- function(curMsg, msg, timestamp,
                                     file, ...) {
            symbols <- get.Data("symbols")
            e_update_mkt_depthL2(NULL, msg, timestamp, file,
                                 symbols, ...)
        }
        updateNewsBulletin <- function(curMsg, msg, timestamp,
                                       file, ...) {
            cat("newsMsgId: ", msg[2], "newsMsgType: ", msg[3],
                "newsMessage: ", msg[4], "origExch:", msg[5],
                "\n")
            c(curMsg, msg)
        }
        managedAccounts <- function(curMsg, msg, timestamp,
                                    file, ...) {
            c(curMsg, msg)
        }
        receiveFA <- function(curMsg, msg, timestamp, file,
                              ...) {
            c(curMsg, msg)
        }
        historicalData <- function(curMsg, msg, timestamp, file,
                                   ...) {
            c(curMsg, msg)
        }
        scannerParameters <- function(curMsg, msg, timestamp,
                                      file, ...) {
            cat(msg <- rawToChar(msg[-which(msg == as.raw(0))]))
            c(curMsg, msg)
        }
        scannerData <- function(curMsg, reqId, rank, contract,
                                distance, benchmark, projection, legsStr) {
            e_scannerData(curMsg, reqId, rank, contract, distance,
                          benchmark, projection, legsStr)
        }
        scannerDataEnd <- function(curMsg, msg, timestamp, file,
                                   ...) {
            c(curMsg, msg)
        }
        realtimeBars <- function(curMsg, msg, timestamp, file,
                                 ...) {
            symbols <- get.Data("symbols")
            e_real_time_bars(curMsg, msg, symbols, file, ...)
        }
        currentTime <- function(curMsg, msg, timestamp, file,
                                ...) {
            c(curMsg, msg)
        }
        fundamentalData <- function(curMsg, msg, timestamp,
                                    file, ...) {
            e_fundamentalData(curMsg, msg)
        }
        deltaNeutralValidation <- function(curMsg, msg, timestamp,
                                           file, ...) {
            c(curMsg, msg)
        }
        tickSnapshotEnd <- function(curMsg, msg, timestamp,
                                    file, ...) {
            c(curMsg, msg)
        }
    }
    else {
        tickPrice <- tickSize <- tickOptionComputation <- tickGeneric <- tickString <- tickEFP <- orderStatus <- openOrder <- openOrderEnd <- updateAccountValue <- updateAccountTime <- updatePortfolio <- accountDownloadEnd <- nextValidId <- contractDetails <- bondContractDetails <- contractDetailsEnd <- execDetails <- execDetailsEnd <- updateMktDepth <- updateMktDepthL2 <- updateNewsBulletin <- managedAccounts <- receiveFA <- historicalData <- scannerParameters <- scannerData <- scannerDataEnd <- realtimeBars <- currentTime <- fundamentalData <- deltaNeutralValidation <- tickSnapshotEnd <- function(curMsg,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              msg, timestamp, file, ...) {
            cat(as.character(timestamp), curMsg, msg, "\n",
                file = file[[1]], append = TRUE, ...)
        }
        errorMessage <- function(curMsg, msg, timestamp, file,
                                 twsconn, ...) {
            cat(as.character(timestamp), curMsg, msg, "\n",
                file = file[[1]], append = TRUE, ...)
        }
    }
    eW <- list(.Data = .Data, get.Data = get.Data, assign.Data = assign.Data,
               remove.Data = remove.Data, tickPrice = tickPrice, tickSize = tickSize,
               tickOptionComputation = tickOptionComputation, tickGeneric = tickGeneric,
               tickString = tickString, tickEFP = tickEFP, orderStatus = orderStatus,
               errorMessage = errorMessage, openOrder = openOrder,
               openOrderEnd = openOrderEnd, updateAccountValue = updateAccountValue,
               updatePortfolio = updatePortfolio, updateAccountTime = updateAccountTime,
               accountDownloadEnd = accountDownloadEnd, nextValidId = nextValidId,
               contractDetails = contractDetails, bondContractDetails = bondContractDetails,
               contractDetailsEnd = contractDetailsEnd, execDetails = execDetails,
               execDetailsEnd = execDetailsEnd, updateMktDepth = updateMktDepth,
               updateMktDepthL2 = updateMktDepthL2, updateNewsBulletin = updateNewsBulletin,
               managedAccounts = managedAccounts, receiveFA = receiveFA,
               historicalData = historicalData, scannerParameters = scannerParameters,
               scannerData = scannerData, scannerDataEnd = scannerDataEnd,
               realtimeBars = realtimeBars, currentTime = currentTime,
               fundamentalData = fundamentalData, deltaNeutralValidation = deltaNeutralValidation,
               tickSnapshotEnd = tickSnapshotEnd)
    class(eW) <- "eWrapper"
    invisible(eW)
}

