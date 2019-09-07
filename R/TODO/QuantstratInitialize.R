#' initialise a new trading environment
#'
#' @export
initTradingEnv <- function(){
  .tradingEnv <- new.env(hash = TRUE)
  return(.tradingEnv)
}
.onLoad <- function(lib, pkg) {
  if(!exists('.tradingEnv'))
    .tradingEnv <<- new.env()
}

#' Constructs the data container used to store transactions and resulting positions.
#'
#' The data series stored here will be an irregular time series.
#' @param initDate date prior to the first close price given, used to contain initial account equity and initial position
#' @param initPosQty initial position, default is zero
#' @param \dots any other passthrough parameters
#' @return Constructs multi-column xts object used to store transactions
#' @rdname initTxn
initTxn <- function (initDate = "1950-01-01", initPosQty = 0, ...)
{
  txn <- xts(as.matrix(t(c(0, 0, 0, 0, initPosQty, 0, 0, 0,
                           0, 0))), order.by = as.POSIXct(initDate, ... = ...),
             ... = ...)
  colnames(txn) <- c("Txn.Qty", "Txn.Price", "Txn.Value",
                     "Txn.Avg.Cost", "Pos.Qty", "Pos.Avg.Cost", "Gross.Txn.Realized.PL",
                     "Txn.Fees", "Net.Txn.Realized.PL", "Con.Mult")
  class(txn) <- c("transactions", class(txn))
  return(txn)
}

#' initializes position P&L for a portfolio instrument
#'
#' Constructs the data container used to store calculated P&L values from
#' transactions and close prices.
#'
#' Constructs multi-column xts object used to store derived position information
#' @param initDate date prior to the first close price given, used to contain initial account equity and initial position
#' @param \dots any other passthrough parameters
#' @param initPosQty initial position, default is zero
#' @param initConMult initial contract multiplier, default is one(1)
#' @param initCcyMult initial currency multiplier, default is one(1)
#' @rdname initPosPL
initPosPL <- function (initDate = "1950-01-01", ..., initPosQty = 0, initConMult = 1,
          initCcyMult = 1)
{
  posPL <- xts(as.matrix(t(c(initPosQty, initConMult, initCcyMult,
                             0, 0, 0, 0, 0, 0, 0, 0))), order.by = as.POSIXct(initDate,
                                                                              ... = ...), ... = ...)
  colnames(posPL) <- c("Pos.Qty", "Con.Mult", "Ccy.Mult",
                       "Pos.Value", "Pos.Avg.Cost", "Txn.Value", "Period.Realized.PL",
                       "Period.Unrealized.PL", "Gross.Trading.PL", "Txn.Fees",
                       "Net.Trading.PL")
  class(posPL) <- c("posPL", class(posPL))
  return(posPL)
}

#' initialize the summary table used in portfolio and account lists
#' @param initDate date prior to the first close price given, used to contain initial account equity and initial position
#' @param \dots any other passthrough parameters
#' @rdname initSummary
initSummary <- function (initDate = "1950-01-01", ...)
{
  summary <- xts(as.matrix(t(rep(0, 9))), order.by = as.POSIXct(initDate,
                                                                ... = ...), ... = ...)
  colnames(summary) <- c("Long.Value", "Short.Value", "Net.Value",
                         "Gross.Value", "Realized.PL", "Unrealized.PL", "Gross.Trading.PL",
                         "Txn.Fees", "Net.Trading.PL")
  class(summary) <- c("portfolio_summary", class(summary))
  return(summary)
}




