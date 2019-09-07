checkBlotterUpdate <- function(port.st = portfolio.st,
                               account.st = account.st,
                               verbose = TRUE) {
    # The purpose of this function is to check for discrepancies
    # between the account object and portfolio object.
    ok <- TRUE
    p <- getPortfolio(port.st)
    a <- getAccount(account.st)
    syms <- names(p$symbols)
    port.tot <- sum(
        sapply(
            syms,
            FUN = function(x) eval(
                parse(
                    text = paste("sum(p$symbols",
                                 x,
                                 "posPL.USD$Net.Trading.PL)",
                                 sep = "$")))))

    port.sum.tot <- sum(p$summary$Net.Trading.PL)

    if(!isTRUE(all.equal(port.tot, port.sum.tot))) {
        ok <- FALSE
        if(verbose) print("portfolio P&L doesn't match sum of symbols P&L")
    }

    initEq <- as.numeric(first(a$summary$End.Eq))
    endEq <- as.numeric(last(a$summary$End.Eq))

    if(!isTRUE(all.equal(port.tot, endEq - initEq)) ) {
        ok <- FALSE
        if(verbose) print("portfolio P&L doesn't match account P&L")
    }

    if(sum(duplicated(index(p$summary)))) {
        ok <- FALSE
        if(verbose)print("duplicate timestamps in portfolio summary")

    }

    if(sum(duplicated(index(a$summary)))) {
        ok <- FALSE
        if(verbose) print("duplicate timestamps in account summary")
    }
    return(ok)
}



PairRatio <- function(x) { #returns the ratio of close prices for 2 symbols
    x1 <- get(x[1])
    x2 <- get(x[2])
    rat <- log10(Cl(x1) / Cl(x2))
    colnames(rat) <- 'Price.Ratio'
    rat
}

MaRatio <- function(x){
    Mavg <- rollapply(x, N , mean)
    colnames(Mavg) <- 'Price.Ratio.MA'
    Mavg
}

Sd <- function(x){
    Stand.dev <- rollapply(x, N, sd)
    colnames(Stand.dev) <- "Price.Ratio.SD"
    Stand.dev
}

ZScore <- function(x){
    a1 <- x$Price.Ratio
    b1 <- x$Price.Ratio.MA
    c1 <- x$Price.Ratio.SD
    z <- (a1-b1)/c1
    colnames(z)<- 'Z.Score'
    z
}

ft2<-function(x){
    adf.test(x)$p.value
}

Pval <- function(x){
    Augmented.df <- rollapply(x, width = N.ADF, ft2)
    colnames(Augmented.df) <- "P.Value"
    Augmented.df
}

chart.P2 = function (Portfolio, Symbol, Dates = NULL, ..., TA = NULL)
{
    pname <- Portfolio
    Portfolio <- getPortfolio(pname)
    if (missing(Symbol))
        Symbol <- ls(Portfolio$symbols)[[1]]
    else Symbol <- Symbol[1]
    Prices = get(Symbol)
    if (!is.OHLC(Prices)) {
        if (hasArg(prefer))
            prefer = eval(match.call(expand.dots = TRUE)$prefer)
        else prefer = NULL
        Prices = getPrice(Prices, prefer = prefer)
    }
    freq = periodicity(Prices)
    switch(freq$scale, seconds = {
        mult = 1
    }, minute = {
        mult = 60
    }, hourly = {
        mult = 3600
    }, daily = {
        mult = 86400
    }, {
        mult = 86400
    })
    if (!isTRUE(freq$frequency * mult == round(freq$frequency,
                                               0) * mult)) {
        n = round((freq$frequency/mult), 0) * mult
    }
    else {
        n = mult
    }
    tzero = xts(0, order.by = index(Prices[1, ]))
    if (is.null(Dates))
        Dates <- paste(first(index(Prices)), last(index(Prices)),
                       sep = "::")
    Portfolio$symbols[[Symbol]]$txn <- Portfolio$symbols[[Symbol]]$txn[Dates]
    Portfolio$symbols[[Symbol]]$posPL <- Portfolio$symbols[[Symbol]]$posPL[Dates]
    Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
    Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades >
                                                               0)]
    Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades <
                                                                0)]
    Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
    if (nrow(Position) < 1)
        stop("no transactions/positions to chart")
    if (as.POSIXct(first(index(Prices))) < as.POSIXct(first(index(Position))))
        Position <- rbind(xts(0, order.by = first(index(Prices) -
                                                      1)), Position)
    Positionfill = na.locf(merge(Position, index(Prices)))
    CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
    if (length(CumPL) > 1)
        CumPL = na.omit(na.locf(merge(CumPL, index(Prices))))
    else CumPL = NULL
    if (!is.null(CumPL)) {
        CumMax <- cummax(CumPL)
        Drawdown <- -(CumMax - CumPL)
        Drawdown <- rbind(xts(-max(CumPL), order.by = first(index(Drawdown) -
                                                                1)), Drawdown)
    }
    else {
        Drawdown <- NULL
    }
    if (!is.null(Dates))
        Prices = Prices[Dates]
    chart_Series(Prices, name = Symbol, TA = TA, ...)
    if (!is.null(nrow(Buys)) && nrow(Buys) >= 1)
        (add_TA(Buys, pch = 2, type = "p", col = "green", on = 1))
    if (!is.null(nrow(Sells)) && nrow(Sells) >= 1)
        (add_TA(Sells, pch = 6, type = "p", col = "red", on = 1))
    if (nrow(Position) >= 1) {
        (add_TA(Positionfill, type = "h", col = "blue", lwd = 2))
        (add_TA(Position, type = "p", col = "orange", lwd = 2,
                on = 2))
    }
    if (!is.null(CumPL))
        (add_TA(CumPL, col = "darkgreen", lwd = 2))
    if (!is.null(Drawdown))
        (add_TA(Drawdown, col = "darkred", lwd = 2, yaxis = c(0,
                                                              -max(CumMax))))
    plot(current.chob())
}






backtestingEmailReport <- function(portfolio.name, account.name, strategy.name,
                                   symbols, initDate, initEq,
                                   title = "AUDUSD & CADUSD Pair Trading",
                                   toAddress = c("ivan.liu@servian.com.au", "sky_x123@hotmail.com")){

    portfolio.st <- portfolio.name
    account.st <- account.name
    strategy.st <- strategy.name

    load.strategy(strategy.st)
    checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)

    updatePortf(portfolio.st)
    updateAcct(account.st)
    updateEndEq(account.st)


    my.dir = paste0("../tempdir",Sys.info()[["user"]],format(Sys.time(), "%H%M%S"))
    dir.create(my.dir)

    # Chart Positions ---------------------------------------------------------
    png(filename=paste(my.dir, "chartPosn.png",sep='/'), width=25, height=15,units = "cm",res = 300)
    chart.Posn(portfolio.st, Symbol = symbols,
               TA = "add_SMA(n = 10, col = 4); add_SMA(n = 30, col = 2)")
    dev.off()


    # Trade Statistics --------------------------------------------------------
    tstats <- tradeStats(portfolio.st)
    tradeStat = data.frame(Item = rownames(t(tstats)), Value = t(tstats),row.names = NULL)

    # Performance Summary
    # rets <- PortfReturns(Account = account.st)
    # rownames(rets) <- NULL
    # charts.PerformanceSummary(rets, colorset = bluefocus)

    # Per Trade Statistics
    pts <- perTradeStats(portfolio.st, Symbol = symbols)
    # kable(pts, booktabs = TRUE, caption = symbol)


    # Performance Statistics
    # tab.perf <- table.Arbitrary(rets,
    #                             metrics=c(
    #                                 "Return.cumulative",
    #                                 "Return.annualized",
    #                                 "SharpeRatio.annualized",
    #                                 "CalmarRatio"),
    #                             metricsNames=c(
    #                                 "Cumulative Return",
    #                                 "Annualized Return",
    #                                 "Annualized Sharpe Ratio",
    #                                 "Calmar Ratio"))
    # kable(tab.perf)

    # Risk Statistics
    # tab.risk <- table.Arbitrary(rets,
    #                             metrics=c(
    #                                 "StdDev.annualized",
    #                                 "maxDrawdown",
    #                                 "VaR",
    #                                 "ES"),
    #                             metricsNames=c(
    #                                 "Annualized StdDev",
    #                                 "Max DrawDown",
    #                                 "Value-at-Risk",
    #                                 "Conditional VaR"))
    # kable(tab.risk)

    # Buy and Hold Performance
    rm.strat("buyHold")
    # initialize portfolio and account
    initPortf("buyHold", symbols = symbols, initDate = initDate)
    initAcct("buyHold", portfolios = "buyHold",
             initDate = initDate, initEq = initEq)
    # place an entry order
    CurrentDate <- time(getTxns(Portfolio = portfolio.st, Symbol = symbols))[2]
    equity = getEndEq("buyHold", CurrentDate)
    ClosePrice <- as.numeric(Cl(spreads[CurrentDate,]))
    UnitSize = as.numeric(trunc(equity/ClosePrice))
    addTxn("buyHold", Symbol = symbols, TxnDate = CurrentDate, TxnPrice = ClosePrice,
           TxnQty = UnitSize, TxnFees = 0)
    # place an exit order
    LastDate <- last(time(spreads))
    LastPrice <- as.numeric(Cl(spreads[LastDate,]))
    addTxn("buyHold", Symbol = "spreads", TxnDate = LastDate, TxnPrice = LastPrice,
           TxnQty = -UnitSize , TxnFees = 0)
    # update portfolio and account
    updatePortf(Portfolio = "buyHold")
    updateAcct(name = "buyHold")
    updateEndEq(Account = "buyHold")
    png(filename=paste(my.dir, "chartPosn_buyhold.png",sep='/'), width=25, height=15,units = "cm",res = 300)
    chart.Posn("buyHold", Symbol = "spreads")
    dev.off()


    # Strategy vs. Market
    # rets <- PortfReturns(Account = account.st)
    # rets.bh <- PortfReturns(Account = "buyHold")
    # returns <- cbind(rets, rets.bh)
    # charts.PerformanceSummary(returns, geometric = FALSE, wealth.index = TRUE,
    #                           main = "Strategy vs. Market")


    # Risk/Return Scatterplot
    # chart.RiskReturnScatter(returns, Rf = 0, add.sharpe = c(1, 2),
    #                         main = "Return vs. Risk", colorset = c("red", "blue"))


    # Relative Performance
    # for(n in 1:(ncol(returns) - 1)) {
    #     chart.RelativePerformance(returns[, n], returns[, ncol(returns)],
    #                               colorset = c("red", "blue"), lwd = 2,
    #                               legend.loc = "topleft")
    # }

    # Portfolio Summary
    # pf <- getPortfolio(portfolio.st)
    # xyplot(pf$summary, type = "h", col = 4)

    # Order Book
    # ob <- getOrderBook(portfolio.st)
    # orderBook = data.frame(ob$Port.PairsStrat$spreads)

    # Maximum Adverse Excursion
    png(filename=paste(my.dir, "maxAdverseExcursion.png",sep='/'), width=15, height=10,units = "cm",res = 300)
    chart.ME(Portfolio = portfolio.st, Symbol = symbols, type = "MAE",scale = "percent")
    dev.off()

    # Maximum Favorable Excursion
    png(filename=paste(my.dir, "maxFavorableExcursion.png",sep='/'), width=15, height=10,units = "cm",res = 300)
    chart.ME(Portfolio = portfolio.st, Symbol = symbols, type = "MFE",scale = "percent")
    dev.off()


    # Account Summary ---------------------------------------------------------
    # a <- getAccount(account.st)
    # xyplot(a$summary, type = "h", col = 4)

    # Equity Curve
    # equity <- a$summary$End.Eq
    # plot(equity, main = "Equity Curve")

    # Account Performance Summary
    # ret <- Return.calculate(equity, method = "log")
    # charts.PerformanceSummary(ret, colorset = bluefocus,
    #                           main = "Strategy Performance")

    # Cumulative Returns
    rets <- PortfReturns(Account = account.st)
    # chart.CumReturns(rets, colorset = rich10equal, legend.loc = "topleft",
    #                  main="SPDR Cumulative Returns")

    # Distribution Analysis
    png(filename=paste(my.dir, "retDistribution.png",sep='/'), width=15, height=10,units = "cm",res = 300)
    chart.Boxplot(rets, main = "SPDR Returns", colorset= rich10equal)
    dev.off()

    png(filename=paste(my.dir, "retDistribution2.png",sep='/'), width=15, height=10,units = "cm",res = 300)
    chart.Histogram(rets,breaks=100,
                    main = "Strategy Based Returns Distribution",
                    methods = c("add.density", "add.normal","add.centered", "add.rug", "add.risk"))
    dev.off()
    # Annualized Returns
    # (ar.tab <- table.AnnualizedReturns(rets))

    # Performance Scatter Plot
    # max.risk <- max(ar.tab["Annualized Std Dev",])
    # max.return <- max(ar.tab["Annualized Return",])
    # chart.RiskReturnScatter(rets,
    #                         main = "SPDR Performance", colorset = rich10equal,
    #                         xlim = c(0, max.risk * 1.1), ylim = c(0, max.return))

    # Notional Costs
    #quantstratII pp. 67/69
    # mnc <- pts$Max.Notional.Cost
    # pe <- sapply(pts$Start,getEndEq, Account = account.st)/3
    # barplot(rbind(pe,mnc),beside=T,col=c(2,4),names.arg=format(pts$Start,"%m/%d/%y"),
    #         ylim=c(0,1.5e5),ylab="$",xlab="Trade Date")
    # legend(x="topleft",legend=c("(Portfolio Equity)/9","Order Size"),
    #        pch=15,col=c(2,4),bty="n")
    # title("Percent of Portfolio Equity versus Trade Size for XLU")

    # 8. HTML -----------------------------------------------------------------
    my.png.files<-list.files(my.dir,pattern="*.png",full.names=TRUE)
    my.txt.file<-list.files(my.dir,pattern="*.txt",full.names=TRUE)

    my.body <- c(
        toHTML(paste0(title, " - Backtesting Performance Report"), "h2")
        ,toHTML(hr())

        ,toHTML("Performance Summary Chart", "h3")
        ,'<img width="800" src="cid:chartPosn.png">'
        ,toHTML(hr())

        ,toHTML("Trade Statistics", "h3")
        ,htmlTableQuant(tradeStat)
        ,toHTML(hr())


        ,toHTML("Performance Summary - Buy and Hold", "h3")
        ,'<img width="800" src="cid:chartPosn_buyhold.png">'
        ,toHTML(hr())

        ,toHTML("Return Distribution", "h3")
        ,'<img width="800" src="cid:retDistribution2.png">'
        ,toHTML(hr())

        ,toHTML("Per Trade Statistics", "h3")
        ,htmlTableQuant(pts)
        ,toHTML(hr())
    )

    my.msg <- buildhtmlmsg(
        my.body
        ,attachmentFileNames = c(my.png.files, my.txt.file)
    )

    my.subject<-paste0(title, " - Backtesting Performance Report (", Sys.Date(), ")")

    tryCatch({
        result<-RQuantSendMail(
            to = toAddress
            ,subject=my.subject
            ,msg=my.msg$html
            ,attach.files = my.msg$attach.files)
    })

    # clean up ----------------------------------------------------------------

    unlink(my.dir, recursive = T)
    return(result)
}
