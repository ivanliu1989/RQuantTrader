source("tests/Utilities.R")
portfolio.name = portfolio.st
account.name = account.st
strategy.name = strategy.st

backtestingEmailReport(portfolio.name, account.name, strategy.name,
                       symbols, initDate, initEq,
                       title = "AUDUSD & CADUSD Pair Trading",
                       toAddress = c("sky_x123@hotmail.com"))
