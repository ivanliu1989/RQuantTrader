#' initialize order container
#'
#' This function sets up the order container by portfolio.
#'
#' If no symbols list is provided (the default) the function will attempt
#' to retrieve the symbols list from the portfolio in the trade blotter.
#'
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbols a list of identifiers of the instruments to be contained in the Portfolio.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param initDate date (ISO8601) prior to the first close price given in mktdata, used to initialize the order book with a dummy order
#' @param \dots any other passthrough parameters
#' @concept order book
#' @export
initOrders = function (portfolio = NULL, symbols = NULL, initDate = "1950-01-01",
                       ...)
{
  orders <- try(getOrderBook(portfolio), silent = TRUE)
  if (inherits(orders, "order_book")) {
    stop(paste("Order Book for portfolio", portfolio, "already exists."))
  }
  else {
    orders <- list()
    orders[[portfolio]] <- list()
  }
  ordertemplate <- xts(as.matrix(t(c(0, NA, "init", "long",
                                     0, "closed", as.character(as.POSIXct(initDate)), "",
                                     "", 0, "", ""))), order.by = as.POSIXct(initDate), ... = ...)
  colnames(ordertemplate) <- c("Order.Qty", "Order.Price",
                               "Order.Type", "Order.Side", "Order.Threshold", "Order.Status",
                               "Order.StatusTime", "Prefer", "Order.Set", "Txn.Fees",
                               "Rule", "Time.In.Force")
  if (is.null(symbols)) {
    pfolio <- getPortfolio(portfolio)
    symbols <- ls(pfolio$symbols)
  }
  if (!is.null(symbols)) {
    orders[[portfolio]][symbols] <- list(NULL)
  }
  else {
    stop("You must specify a symbols list or a valid portfolio to retrieve the list from.")
  }
  class(orders) <- "order_book"
  assign(paste("order_book", portfolio, sep = "."), orders,
         envir = .strategyEnv)
}



#' get orders by time span, status, type, and side
#'
#' This function exists so that other code can find open orders, potentially to update or cancel them.
#'
#' It has some use as a reporting or post-hoc analytics tool, but it may not always be exported.
#'
#' should this be symbols instead of symbol?
#'
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identifier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param status one of "open", "closed", "canceled", "revoked", or "replaced", default "open"
#' @param timespan xts-style character timespan to be the period to find orders of the given status and ordertype
#' @param ordertype one of NULL, "market","limit","stoplimit", "stoptrailing" or "iceberg" default NULL
#' @param side one of NULL, "long" or "short", default NULL
#' @param qtysign one of NULL, -1,0,1 ; could be useful when all qty's are reported as positive numbers and need to be identified other ways, default NULL
#' @param orderset a tag identifying the orderset
#' @param which.i if TRUE, return the row index numbers rather than the order rows matching the criteria, default FALSE
#' @seealso getOrderBook
#' @seealso addOrder
#' @concept order book
#' @export
get.orders <- function(portfolio,symbol,status="open",timespan=NULL,ordertype=NULL, side=NULL, qtysign=NULL, orderset=NULL, which.i=FALSE)
{
  #if(is.null(timespan)) stop("timespan must be an xts style timestring")
  # get order book
  orderbook <- get.orderbook(portfolio)
  if(!any(names(orderbook[[portfolio]]) == symbol)) stop(paste("symbol",symbol,"does not exist in portfolio",portfolio,"having symbols",names(orderbook[[portfolio]])))

  ordersubset <- orderbook[[portfolio]][[symbol]]
  if(is.null(ordersubset))
    return(NULL)

  #data quality checks
  if(!is.null(status) & !length(grep(status,c("open", "closed", "canceled", "revoked","replaced")))==1) stop(paste("order status:",status,' must be one of "open", "closed", "canceled", "revoked", or "replaced"'))
  if(!is.null(ordertype)) {
    if(is.na(charmatch(ordertype,c("market","limit","stoplimit","stoptrailing","iceberg")))){
      stop(paste("ordertype:",ordertype,' must be one of "market","limit","stoplimit", "stoptrailing" or "iceberg"'))
    }
  }

  indices <- which(#if(!is.null(timespan)) ordersubset[timespan,which.i=TRUE] else TRUE &
    (if(!is.null(status)) ordersubset[,"Order.Status"]==status else TRUE) &
      (if(!is.null(ordertype)) ordersubset[,"Order.Type"]==ordertype else TRUE) &
      (if(!is.null(side)) ordersubset[,"Order.Side"]==side else TRUE) &
      (if(!is.null(orderset)) ordersubset[,"Order.Set"]==orderset else TRUE) &
      (if(!is.null(qtysign)) sign(as.numeric(ordersubset[,"Order.Qty"]))==qtysign else TRUE)
  )

  if(isTRUE(which.i)){
    return(indices)
  } else {
    # extract
    ordersubset<-orderbook[[portfolio]][[symbol]][indices,]
    #subset by time
    if(nrow(ordersubset)>1 && !is.null(timespan)) ordersubset<-ordersubset[timespan]
    return(ordersubset)
  }
}


#' get the order book object
#'
#' @param portfolio text name of the portfolio the order book is associated with
#' @param envir the environment to retrieve the orderbook object from, defaults to .strategy
#' @seealso addOrder
#' @seealso getOrders
#' @concept order book
#' @return
#' A \code{data.frame} containing:
#'
#' \describe{
#'      \item{Order.Qty}{}
#'     \item{Order.Price}{}
#'     \item{Order.Type}{}
#'     \item{Order.Side}{}
#'     \item{Order.Threshold}{}
#'     \item{Order.Status}{}
#'     \item{Order.StatusTime}{}
#'     \item{Prefer}{}
#'     \item{Order.Set}{}
#'     \item{Txn.Fees}{}
#'     \item{Rule}{}
#' }
#' @aliases
#' get.orderbook
#' getOrderbook
#' @rdname getOrderBook
#' @export get.orderbook
get.orderbook <- function(portfolio, envir=.strategyEnv) #should symbol subsets be supported too?  probably not.
{
  if(!grepl("order_book",portfolio)) orders<-try(get(paste("order_book",portfolio,sep='.'),envir=envir),silent=TRUE)
  else orders<-try(get(portfolio,envir=envir),silent=TRUE)
  if(inherits(orders,"try-error"))
    stop(paste("Orders for ",portfolio," not found, use initOrders() to create a new order book for this portfolio"))
  if(!inherits(orders,"order_book")) stop("Order Book for portfolio",portfolio,"does not appear to name an order book object.")
  return(orders)
}

#' put an orderbook object in .strategy env
#' @param portfolio.st string identifying portfolio
#' @param orderbook orderbook object
#' @param envir the environment to store the orderbook object in, defaults to .strategy
#' @seealso getOrderBook
#' @concept order book
#' @export
put.orderbook <- function(portfolio.st, orderbook, envir=.strategyEnv)
{
  strategy.orderbook.st <- paste('order_book', portfolio.st, sep='.')

  assign(strategy.orderbook.st, orderbook, envir=envir)
}
