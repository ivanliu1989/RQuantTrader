#' Get a basic report of trading strategy
#'
#' Get a basic report of trading strategy
#'
#' @param dt.summary trading activity sheet returned from \code{BackTesting}
#'
#' @return A \code{list} of basic trading details
#'
#' @seealso \link{BackTesting}
#'
#' @export
performanceReport <- function(dt.summary){
  # - entry/exit points
  # - average holding time
  # - num of trades in the period
  # - average return per trade
  # - distribution of returns per trade
  # - distribution of days holding
  ret <- dt.summary[dt.summary$in_short | dt.summary$in_long,]
  Beta = mean(ret$beta)
  Alpha = mean(ret$alpha)
  Max.Drawdown = max(ret$real.capital) - min(ret$real.capital)
  Max.Drawdown.Ratio = Max.Drawdown / max(ret$real.capital)
  ret <- ROC(ret$real.capital, n = 1, type = "discrete", na.pad = 0)
  APR <- round(prod(1+ret)**(252/length(ret)) - 1, 5)
  Sharpe <- round(sqrt(252)*mean(ret)/sd(ret), 5)
  Volatility = sd(ret)


  trade.summary <- dt.summary[!dt.summary$trade == "NO Trade",]
  table(trade.summary$trade)
  benchmark.ret = mean(c((trade.summary$y.close[length(trade.summary$y.close)] - trade.summary$y.close[1]) / trade.summary$y.close[1],
  (trade.summary$x.close[length(trade.summary$x.close)] - trade.summary$x.close[1]) / trade.summary$x.close[1])) - 0.02

  # short
  trade.short <- trade.summary[!trade.summary$trade == "Go Long",]
  trade.short <- setDataTable(trade.short)
  trade.short[, exit.capital := shift(real.capital, n = 1, fill = 0, type = "lead")]
  trade.short[, returns := (exit.capital-real.capital)/real.capital]
  trade.short[, trade.days := shift(Dates, n = 1, fill = 0, type = "lead") - Dates]
  trade.short <- trade.short[!trade == "Exit"]

  # long
  trade.long <- trade.summary[!trade.summary$trade == "Go Short",]
  trade.long <- setDataTable(trade.long)
  trade.long[, exit.capital := shift(real.capital, n = 1, fill = 0, type = "lead")]
  trade.long[, returns := (exit.capital-real.capital)/real.capital]
  trade.long[, trade.days := shift(Dates, n = 1, fill = 0, type = "lead") - Dates]
  trade.long <- trade.long[!trade == "Exit"]

  # All trades
  trade.details <- rbind(trade.long[,.(Dates, y.close,y.bid,y.ask, x.close, x.bid, x.ask, y.shares, x.shares, hedgeRatio, brokerage, trade, real.capital, returns, trade.days)],
                         trade.short[,.(Dates, y.close,y.bid,y.ask, x.close, x.bid, x.ask, y.shares, x.shares, hedgeRatio, brokerage, trade, real.capital, returns, trade.days)])
  setorder(trade.details, Dates)

  Volatility =
  num.Txns <- nrow(trade.summary)
  win.loss.ratio <- mean(trade.details$returns > 0)
  avg.holdingdays <- mean(trade.details$trade.days)
  avg.returns <- mean(trade.details$returns[-length(trade.details$returns)])
  tot.returns <- prod(trade.details$returns[-length(trade.details$returns)]+1)-1
  tot.tradingdays <- round(sum(trade.details$trade.days[-length(trade.details$trade.days)]) * 252/365)

  res <- list(
    trade.summary = dt.summary,
    trade.details = trade.details,
    stats.summary = list(avg.holdingdays = avg.holdingdays,
    avg.returns = avg.returns,
    tot.returns = tot.returns,
    tot.tradingdays = tot.tradingdays,
    APR = APR,
    Sharpe = Sharpe,
    Alpha = Alpha,
    Beta = Beta,
    Volatility = Volatility,
    Max.Drawdown = Max.Drawdown,
    Max.Drawdown.Ratio = Max.Drawdown.Ratio,
    Num.Txns = num.Txns,
    Win.Loss.Ratio = win.loss.ratio,
    benchmark.ret = benchmark.ret)
  )
  return(res)
}
