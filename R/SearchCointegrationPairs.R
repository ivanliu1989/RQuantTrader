#' Search Cointegrated FX Pairs via Oanda API
#'
#' Search Cointegrated FX Pairs via Oanda API
#'
#' @param total_points Total data points to lookback (e.g. days)
#' @param testPeriod proportion of data points to be testing set
#' @param INSTRUMENTS a list of available FX instruments
#' @param dir directory to save the output
#'
#' @seealso \link{JohansenCointegrationTest}
#' @seealso \link{AugmentedDickeyFullerTest}
#'
#' @examples
#' INSTRUMENTS = readRDS('~/analytics/common/OANDA_FX_INSTRUMENTS.rds')
#' res = searchCointegratedPairsOanda(total_points = 500, testPeriod = 0.25, INSTRUMENTS = INSTRUMENTS, dir = "~/analytics/common/cointegration/")
#' res$res
#' res$path
#'
#' @export
searchCointegratedPairsOanda <- function(total_points = 500,
                                         testPeriod = 0.25,
                                         INSTRUMENTS,
                                         dir = "~/analytics/common/cointegration/"){
    # 0. Setup and get all Instruments Available ------------------------------
    library(data.table)
    library(tseries)
    library(zoo)
    library(ggplot2)
    if(!dir.exists(dir)){
        dir.create(dir)
    }

    getFXPriceRatio <- function(y, x, log = TRUE){

        p_ratio <- (y/x)
        p_ratio[is.infinite(p_ratio)] <- NA
        p_ratio <- na.omit(p_ratio)
        if(log) p_ratio <- log(p_ratio)

        return(p_ratio)
    }

    FXindexation <- function(x) {
        # Divide each column by the first non-NA value
        # (There may already be a function to do that.)
        coredata(x) <- t(t(coredata(x)) / apply(as.data.frame(coredata(x)),2,function(u){ c(u[!is.na(u)&u!=0],NA)[1] }))
        return(x)
    }

    # 1. Retrieving All Pairs data --------------------------------------------
    for(i in 1:length(INSTRUMENTS)){
        inst = INSTRUMENTS[i]
        cat(paste0("\nRetreiving ", inst, " data..."))

        dat = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = inst,
                                        price = 'M', granularity = 'D', count = total_points)[, c("time", "mid.c")]
        names(dat) = c("Date", inst)
        dat[, inst] <- as.numeric(dat[, inst])
        # dat[, paste0(inst,"_INV")] <- 1/dat[, inst]
        data.table::setDT(dat)
        dat[, Date := as.IDate(Date)]
        if(i == 1){
            ALL_FX = dat
        }else{
            ALL_FX = merge(ALL_FX, dat, by = "Date")
        }
    }



    # 2. Split train and test -------------------------------------------------
    INSTRUMENT_NAMES <- names(ALL_FX)[-1]
    nInstruments <- length(INSTRUMENT_NAMES)
    totalPeriod <- nrow(ALL_FX)
    nDays <- nrow(ALL_FX)

    testDates <- ALL_FX$Date[(nDays-round(nDays*testPeriod)):nDays]
    trainingDates <- ALL_FX$Date[-c((nDays-round(nDays*testPeriod)):nDays)]

    training_ds <- as.data.frame(ALL_FX[Date %in% trainingDates])[,-1]
    test_ds <- as.data.frame(ALL_FX[Date %in% testDates])[,-1]


    # 3. Prepare variables and start testing ----------------------------------
    # prepare variables
    ht_jo <- matrix(data = NA, ncol = nInstruments, nrow = nInstruments)
    ht_adf <- matrix(data = NA, ncol = nInstruments, nrow = nInstruments)
    p_ratio <- list()

    # Test cointegrations of all pairs
    for (j in 1:(nInstruments-1)) {
        for (i in (j+1):nInstruments) {

            cat("Testing cointegrations of ", j, " ", INSTRUMENT_NAMES[j], " - ", i, " ", INSTRUMENT_NAMES[i], "\n")
            tmp_ds <- na.omit(cbind(training_ds[,j], training_ds[,i]))
            if (length(tmp_ds) == 0)
            {
                ht_jo[j, i] <- NA
                ht_adf[j, i] <- NA
                next
            }

            p_ratio <- getFXPriceRatio(tmp_ds[,2],tmp_ds[,1],log = T)

            # The ht object contains the p-value from the ADF test.
            # The p-value is the probability that the spread is NOT
            # mean-reverting.  Hence, a small p-value means it is very
            # improbable that the spread is NOT mean-reverting
            adf.p <- try(AugmentedDickeyFullerTest(na.omit(p_ratio))$signif[[1]])
            if (isTRUE(class(adf.p) == "try-error"))
            {
                ht_adf[j, i] <- NA
                next
            }
            ht_adf[j, i] <- adf.p

            # Johansen test same as above
            jc.p <- try(JohansenCointegrationTest(as.data.frame(na.omit(cbind(tmp_ds[,2],tmp_ds[,1]))))$signif[[1]])
            if (isTRUE(class(jc.p) == "try-error"))
            {
                ht_jo[j, i] <- NA
                next
            }
            ht_jo[j, i] <- jc.p
        }
    }


    # 4. Select well performed ------------------------------------------------
    zscore <- 0;
    rscore <- matrix(data = NA, ncol = 7, nrow = (nInstruments^2)/2)
    pairSummary <- matrix(data = NA, ncol = 5, nrow = (nInstruments^2)/2)

    idx <- 1;

    # lets evaluate the spreads
    for (j in 1:(nInstruments-1)) {
        for (i in (j+1):nInstruments) {

            # if no data, skip
            if (is.na(ht_jo[j, i]) | is.na(ht_adf[j, i])) next

            # is spread stationary (i.e. pair is co-integrated)
            # p-value is the smaller the better
            if (ht_jo[j, i] <= 5 & ht_adf[j, i] <= 5) {

                tmp_ds <- as.data.frame(na.omit(cbind(training_ds[,j], training_ds[,i])))
                if (length(tmp_ds) == 0) next

                p_ratio <- getFXPriceRatio(tmp_ds[,2],tmp_ds[,1],log = T)

                # calculate z-score
                zscore <- sum(abs(scale(p_ratio)))/length(p_ratio)
                rscore[idx, 3] <- sd(p_ratio)
                rscore[idx, 4] <- zscore
                rscore[idx, 5] <- mean(p_ratio)
                rscore[idx, 1] <- j
                rscore[idx, 2] <- i
                rscore[idx, 6] <- ht_jo[j, i] # Johnson
                rscore[idx, 7] <- ht_adf[j, i] # ADF

                pairSummary[idx, ] = fivenum(p_ratio)[1:5]
                idx <- idx + 1
            }
        }

        cat("Calculating ", j, "\n")
    }

    # clean up na rows
    rscore <- na.remove(rscore)
    pairSummary <- na.remove(pairSummary)



    # 5. Visualise good pairs -------------------------------------------------
    cat(paste0("Found ", length(rscore[,1]), " good pairs!"))

    if (length(rscore[,1]) == 0) { stop("No good pair found!") }

    path = paste0(dir, "Cointegration_Test_", as.numeric(Sys.time()), ".pdf")
    pdf(path, useDingbats=FALSE, width=12, height=6)

    for (pos in 1:length(rscore[,1])) {
        j <- rscore[pos, 1]
        i <- rscore[pos, 2]
        name_j <- INSTRUMENT_NAMES[j]
        name_i <- INSTRUMENT_NAMES[i]
        cat(paste0("\nPair: ", name_j, " to ", name_i, " | Score: Johansen-", ht_jo[j,i], " & ADF-", ht_adf[j,i]))

        # training set
        tmp_ds <- as.data.frame(na.omit(cbind(training_ds[,j], training_ds[,i])))
        tmp_ds$Date = trainingDates
        tmp_ds$Period = "Train"
        tmp_ds_t <- as.data.frame(na.omit(cbind(test_ds[,j], test_ds[,i])))
        tmp_ds_t$Date = testDates
        tmp_ds_t$Period = "Test"
        tmp_ds <- rbind(tmp_ds, tmp_ds_t)

        colnames(tmp_ds) <- c(name_j, name_i, "Date", "Period")
        if (length(tmp_ds) == 0) next
        tmp_ds$l_pr <- getFXPriceRatio(tmp_ds[,2],tmp_ds[,1], log = T)
        tmp_ds$l_pr <- zscores(tmp_ds$l_pr)
        tmp_ds$l_ds_j <- FXindexation(as.zoo(tmp_ds[,2]))
        tmp_ds$l_ds_i <- FXindexation(as.zoo(tmp_ds[,1]))
        tmp_ds$lb = -1
        tmp_ds$ub = 1


        # plot 1
        p1 <- ggplot(tmp_ds[tmp_ds$Period == "Train",], aes(x=Date, y=l_ds_j)) +
            geom_line(color = quantColours()[1], size = 1) +
            geom_line(data=tmp_ds[tmp_ds$Period == "Train",], aes(x=Date, y=l_ds_i), color = "orange", size = 1) +
            geom_line(data=tmp_ds[tmp_ds$Period == "Test",], aes(x=Date, y=l_ds_j), color = quantColours()[1], linetype = 8, size = 1) +
            geom_line(data=tmp_ds[tmp_ds$Period == "Test",], aes(x=Date, y=l_ds_i), color = "orange", linetype = 8, size = 1) +
            xlab(NULL) +  ylab("Index Prices") +
            ggtitle(paste0(name_j, " - ", name_i, "\nCointegration Test \nAugmentedDickeyFuller Test: p-value ",rscore[pos, 7],"\nJohansenCointegration Test: p-value ", rscore[pos, 6])) +
            theme_rquant(rquant_font = F, rquant_colours = T, font_size = 12, opacity = 0.05, logo = "rquant", inv = 1)

        # plot 2
        p2 <- ggplot(tmp_ds[tmp_ds$Period == "Train",], aes(x=Date, y=l_pr)) +
            geom_line(color = quantColours(inv = 3)[8], size = 1) +
            geom_line(data=tmp_ds[tmp_ds$Period == "Test",], aes(x=Date, y=l_pr), color = quantColours(inv = 3)[8], linetype = 8, size = 1) +
            geom_line(data=tmp_ds, aes(x=Date, y=ub), color = "black", linetype = 3,size = 1) +
            geom_line(data=tmp_ds, aes(x=Date, y=lb), color = "black", linetype = 3,size = 1) +
            xlab("Date") +  ylab("Mean reversion - Z Score") +
            theme_rquant(rquant_font = F, rquant_colours = T, font_size = 12, opacity = 0.05, logo = "rquant", inv = 1)

        multiplot(p1, p2, cols = 1)

    }

    dev.off()


    # 6. Report ---------------------------------------------------------------
    rscore <- as.data.frame(rscore)
    names(rscore) <- c("Pair1", "Pair2", "SDPriceRatio", "ZScore", "AvgPriceRatio", "JohansenScore", "ADFScore")
    pairSummary <- as.data.frame(pairSummary)
    names(pairSummary) <- c("MinPriceRatio", "LowerPriceRatio", "MedPriceRatio", "UpperPriceRatio", "MaxPriceRatio")
    res <- cbind(rscore, pairSummary)
    res$PairName1 <- INSTRUMENTS[res$Pair1]
    res$PairName2 <- INSTRUMENTS[res$Pair2]

    res <- res[, c("PairName1", "PairName2", "JohansenScore", "ADFScore", "ZScore", "SDPriceRatio", "AvgPriceRatio", "MinPriceRatio", "LowerPriceRatio", "MedPriceRatio", "UpperPriceRatio", "MaxPriceRatio")]

    return(list(res = res,
                path = path))
}


#' #' Search Cointegrated Pairs
#' #'
#' #' Conduct the Johansen procedure and ADF test on all combination of pairs to find potential integrated pairs
#' #' Conduct the out of sample tests on all pairs to test the consistencies of cointegrations
#' #'
#' #' @param dataset Data matrix including all serires
#' #' @param path the path of the pdf report to be saved
#' #' @param to a list of email addressed to receive the reports
#' #' @param testPeriod number of points used to test cointegrations
#' #' @param trainPeriod number of points used to train cointegrations
#' #'
#' #' @seealso \link{JohansenCointegrationTest}
#' #' @seealso \link{AugmentedDickeyFullerTest}
#' #'
#' #' @examples
#' #' data("sp500")
#' #' datasets <- sp500[,sample(1:500, 100)]
#' #' searchCointegratedPairs(datasets, path = "GoodIntegratedPairs.pdf",
#' #' to = c("ivan.liuyanfeng@gmail.com"),testPeriod = 63, trainPeriod = 252)
#' #'
#' #' @export
#' searchCointegratedPairs <- function(dataset, path = "GoodIntegratedPairs.pdf", to = c("ivan.liuyanfeng@gmail.com"),
#'                                     testPeriod = 63, trainPeriod = 252){
#'     library(tseries)
#'     stocks <- names(dataset)
#'     nrStocks <- length(stocks)
#'     totalPeriod <- testPeriod + trainPeriod
#'     nDays <- nrow(dataset)
#'     testDates <- (nDays-testPeriod):nDays
#'     learningDates <- (nDays - testPeriod - trainPeriod):(nDays - testPeriod)
#'
#'     learning_ds <- dataset[learningDates,]
#'     test_ds <- dataset[testDates,]
#'
#'     # 1. Calculating Cointegrations for all Pairs -----------------------------
#'     # prepare variables
#'     ht_jo <- matrix(data = NA, ncol = nrStocks, nrow = nrStocks)
#'     ht_adf <- matrix(data = NA, ncol = nrStocks, nrow = nrStocks)
#'     p_ratio <- list()
#'
#'     # Test cointegrations of all pairs
#'     for (j in 1:(nrStocks-1)) {
#'         for (i in (j+1):nrStocks) {
#'
#'             cat("Testing cointegrations of ", j, " - ", i, "\n")
#'             tmp_ds <- na.omit(cbind(learning_ds[,j], learning_ds[,i]))
#'             if (length(tmp_ds) == 0)
#'             {
#'                 ht_jo[j, i] <- NA
#'                 ht_adf[j, i] <- NA
#'                 next
#'             }
#'
#'             p_ratio <- getPriceRatio(tmp_ds[,2],tmp_ds[,1],log = T)
#'
#'             # The ht object contains the p-value from the ADF test.
#'             # The p-value is the probability that the spread is NOT
#'             # mean-reverting.  Hence, a small p-value means it is very
#'             # improbable that the spread is NOT mean-reverting
#'             adf.p <- try(AugmentedDickeyFullerTest(na.omit(coredata(p_ratio)))$signif[[1]])
#'             if (isTRUE(class(adf.p) == "try-error"))
#'             {
#'                 ht_adf[j, i] <- NA
#'                 next
#'             }
#'             ht_adf[j, i] <- adf.p
#'
#'             # Johansen test same as above
#'             jc.p <- try(JohansenCointegrationTest(na.omit(merge(tmp_ds[,2],tmp_ds[,1])))$signif[[1]])
#'             if (isTRUE(class(jc.p) == "try-error"))
#'             {
#'                 ht_jo[j, i] <- NA
#'                 next
#'             }
#'             ht_jo[j, i] <- jc.p
#'         }
#'     }
#'
#'
#'     # 2. Select well performed ------------------------------------------------
#'     zscore <- 0;
#'     rscore <- matrix(data = NA, ncol = 5, nrow = (nrStocks^2)/2)
#'     pairSummary <- matrix(data = NA, ncol = 5, nrow = (nrStocks^2)/2)
#'
#'     idx <- 1;
#'
#'     # lets evaluate the spreads
#'     for (j in 1:(nrStocks-1)) {
#'         for (i in (j+1):nrStocks) {
#'
#'             # if no data, skip
#'             if (is.na(ht_jo[j, i]) | is.na(ht_adf[j, i])) next
#'
#'             # is spread stationary (i.e. pair is co-integrated)
#'             # p-value is the smaller the better
#'             if (ht_jo[j, i] <= 5 & ht_adf[j, i] <= 5) {
#'
#'                 tmp_ds <- na.omit(cbind(learning_ds[,j], learning_ds[,i]))
#'                 if (length(tmp_ds) == 0) next
#'
#'                 p_ratio <- getPriceRatio(tmp_ds[,2],tmp_ds[,1],log = T)
#'
#'                 # calculate z-score
#'                 zscore <- sum(abs(scale(p_ratio)))/length(p_ratio)
#'                 rscore[idx, 3] <- sd(p_ratio)
#'                 rscore[idx, 4] <- zscore
#'                 rscore[idx, 5] <- mean(p_ratio)
#'                 rscore[idx, 1] <- j
#'                 rscore[idx, 2] <- i
#'
#'                 pairSummary[idx, ] = fivenum(coredata(p_ratio))[1:5]
#'                 idx <- idx + 1
#'             }
#'         }
#'
#'         cat("Calculating ", j, "\n")
#'     }
#'
#'     # clean up na rows
#'     rscore <- na.remove(rscore)
#'     pairSummary <- na.remove(pairSummary)
#'
#'     # 3. Visualise good pairs -------------------------------------------------
#'     cat(paste0("Found ", length(rscore[,1]), " good pairs!"))
#'
#'     if (length(rscore[,1]) == 0) { stop("No good pair found!") }
#'
#'
#'     pdf(path, useDingbats=FALSE, width=8, height=9)
#'     for (pos in 1:length(rscore[,1])) {
#'         j <- rscore[pos, 1]
#'         i <- rscore[pos, 2]
#'         name_j <- stocks[j]
#'         name_i <- stocks[i]
#'         cat(paste0("\nPair: ", name_j, " to ", name_i, " | Score: Johansen-", ht_jo[j,i], " & ADF-", ht_adf[j,i]))
#'
#'         # training set
#'         tmp_ds <- na.omit(cbind(learning_ds[,j], learning_ds[,i]))
#'         if (length(tmp_ds) == 0) next
#'         l_pr <- getPriceRatio(tmp_ds[,2],tmp_ds[,1], log = T)
#'         l_pr <- zscores(l_pr)
#'         l_ds_j <- indexation(tmp_ds[,1])
#'         l_ds_i <- indexation(tmp_ds[,2])
#'
#'         # testing set
#'         tmp_ds <- na.omit(cbind(test_ds[,j], test_ds[,i]))
#'         if (length(tmp_ds) == 0) next
#'         t_pr <- getPriceRatio(tmp_ds[,2],tmp_ds[,1], log = T)
#'         t_pr <- zscores(t_pr)
#'
#'         lb = -1
#'         ub = 1
#'
#'         par(mfrow=c(3,1))
#'         par(mar = c(2, 4, 4, 2))
#'         chart.TimeSeries(merge(l_ds_j, l_ds_i), ylab = "Prices", colorset = bluefocus,
#'                          legend.loc = "topleft", xaxis = TRUE, main = paste0("Price Index - ", name_j, " to ", name_i))
#'         l_dt <- cbind(l_pr, 1, -1); colnames(l_dt) <- c("Price Ratio", "Upper Band", "Low Band")
#'         chart.TimeSeries(l_dt,xaxis = TRUE, ylab = "Price Ratio", colorset = bluefocus,
#'                          legend.loc = "topleft", main = "Train Price Ratio")
#'         t_dt <- cbind(t_pr, 1, -1); colnames(t_dt) <- c("Price Ratio", "Upper Band", "Low Band")
#'         par(mar = c(3, 4, 4, 2))
#'         chart.TimeSeries(t_dt,xaxis = TRUE, ylab = "Price Ratio", colorset = bluefocus,
#'                          legend.loc = "topleft", main = "Test Price Ratio")
#'
#'         # cmd <- readline()
#'         # if (cmd == 'c') break
#'     }
#'     dev.off()
#'
#'     # 4. Email ----------------------------------------------------------------
#'     library(mailR)
#'     library(xtable)
#'     report.dt <- cbind(rscore, pairSummary)
#'     report.dt[,1] <- stocks[as.numeric(report.dt[,1])]
#'     report.dt[,2] <- stocks[as.numeric(report.dt[,2])]
#'     colnames(report.dt) <- c("Symbol Y", "Symbol X", "Std", "Zscore", "Mean", "Min", "Lower-hinge", "Median", "Upper-hinge", "Max")
#'     from = "ivan.liuyanfeng@gmail.com"
#'     subject = "AutoPairTrading - Search Cointegration Report"
#'     msg = paste0("<h3>AutoPairTrading Cointegration Search Notification - ", Sys.Date(), "</h3>",
#'                  paste0("<h4>Found ", length(rscore[,1]), " good pairs!</h4>"),
#'                  paste0("<p>Searched ", nrStocks, " unique series and ", round(nrStocks^2/2), " pairs in total.</p>"),
#'                  paste0("<p>Following ", length(rscore[,1]), " have been found as good pairs in terms of their Johansen & ADF scores.</p>"),
#'                  print(xtable(report.dt), type = "html"),
#'                  "<br>",
#'                  "<p>For more details, please see attached <b>GoodIntegratedPairs.pdf</b></p>"
#'     )
#'     tryCatch({
#'         send.mail(from = "ivan.liuyanfeng@gmail.com",
#'                   to = to,
#'                   subject = subject,
#'                   body = msg,
#'                   html = TRUE,
#'                   smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "ivan.liuyanfeng@gmail.com", passwd = "Kalmanfilter123", ssl = TRUE),
#'                   authenticate = TRUE,
#'                   attach.files = path,
#'                   send = TRUE)
#'     }, finally = {
#'         unlink(path, recursive = T, force = T)
#'         cat("Reports sent! Please check your email box!")
#'     })
#' }


