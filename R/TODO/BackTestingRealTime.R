#' Real Time Backtesting Function for Modeling
#'
#' Real time backtesting function for modeling with removed survivor biased and bias of using future elements
#'
#' @param context trading context
#' @param datasets a dataset to be used for modeling
#' @param nEval length of periods used for training
#' @param strategyFunc customised function to calculate indicator from \code{datasets}
#' @param strategyParam parameters used in \code{strategyFunc}
#' @param longTrigger a number of thresholds for going long
#' @param shortTrigger a number of thresholds for going short
#' @param exitTrigger a trigger to exit current positions
#'
#' @return A \code{data.frame} with trading activities
#'
#' @seealso \link{getUserTemplate}
#'
#' @export
BackTestingRealTime <- function(context, datasets, nEval = 500, longTrigger = -1, shortTrigger = 1, exitTrigger = 0, strategyFunc = NULL, strategyParam = 5){
  lookback <- context$lookback
  brokerage <- context$brokerage
  capital <- context$capital
  stoploss <- context$stoploss
  in_short = FALSE
  in_long = FALSE
  stdat = nrow(datasets) - lookback + 1
  enddat = nrow(datasets)
  y = datasets$y.close
  y.bid = datasets$y.bid
  y.ask = datasets$y.ask
  x = datasets$x.close
  x.bid = datasets$x.bid
  x.ask = datasets$x.ask
  prices <- merge(y, y.bid, y.ask, x, x.bid, x.ask)
  dt.summary <- createSummarySheet(prices, context$capital)
  i = 0

  for(d in stdat:enddat){
    i = i + 1
    cat(paste0("\nDate: ", index(datasets[d,])))
    # create strategy indicator
    dt = datasets[max(1, d-nEval):d, ]
    half.life <- HalfLifeMeanReversion(dt$price.ratio)$half.life.round; #cat(paste0("Half-Life: ", half.life))

    # skip unnecessary modeling
    zc <- zscores(dt$price.ratio)
    indicator <- tail(zc,1)
    # exit = ((in_short & indicator < exitTrigger) | (in_long & indicator > exitTrigger))
    golong = (!in_long & indicator < longTrigger)
    goshort = (!in_short & indicator > shortTrigger)
    if(any(golong, goshort)){
      indicator <- strategyBuilder(dt, half.life, strategyParam)
    }

    # calculate hedge ratio
    hedgeRatio <- HedgeRatioOLS(dt[max((nrow(dt)-half.life),1):nrow(dt),2],
                                dt[max((nrow(dt)-half.life),1):nrow(dt),3])


    if(d == enddat){
      # Update numbers
      y_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$y.holdings[i], dt.summary$y.bid[i], dt.summary$y.shares[i], 0, brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$x.holdings[i], dt.summary$x.bid[i], dt.summary$x.shares[i], 0, brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + as.numeric(x_order_update$brokerage)

      in_short = FALSE
      in_long = FALSE
      dt.summary$long.pos[i] = 0
      dt.summary$short.pos[i] = 0
      dt.summary$trade[i] = "Exit"

    }else if((in_short & indicator < exitTrigger) | (in_long & indicator > exitTrigger)){
      # Update numbers
      y_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$y.holdings[i], dt.summary$y.bid[i], dt.summary$y.shares[i], 0, brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$capital[i+1] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$y.holdings[i+1] = y_order_update$holdings
      dt.summary$y.shares[i+1] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$x.holdings[i], dt.summary$x.bid[i], dt.summary$x.shares[i], 0, brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$capital[i+1] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
      dt.summary$x.holdings[i+1] = x_order_update$holdings
      dt.summary$x.shares[i+1] = x_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + as.numeric(x_order_update$brokerage)

      in_short = FALSE
      in_long = FALSE
      dt.summary$long.pos[i] = 0
      dt.summary$short.pos[i] = 0
      dt.summary$trade[i] = "Exit"

    }else if(!in_long & indicator < longTrigger){
      # Only trade if NOT already in a trade
      y_target_shares = 1
      x_target_shares = -hedgeRatio$beta
      in_long = TRUE
      in_short = FALSE

      target_pct <- as.vector(computeHoldingsPct(y_target_shares,x_target_shares,y[i], x[i]))

      # Update numbers
      y_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$y.holdings[i], dt.summary$y.ask[i], dt.summary$y.shares[i], target_pct[1], brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$capital[i+1] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.holdings[i+1] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$y.shares[i+1] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$x.holdings[i], dt.summary$x.bid[i], dt.summary$x.shares[i], target_pct[2], brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$capital[i+1] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
      dt.summary$x.holdings[i+1] = x_order_update$holdings
      dt.summary$x.shares[i+1] = x_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + as.numeric(x_order_update$brokerage)


      dt.summary$long.pos[i] = 1
      dt.summary$short.pos[i] = -1
      dt.summary$trade[i] = "Go Long"

    }else if(!in_short & indicator > shortTrigger){
      # Only trade if NOT already in a trade
      y_target_shares = -1
      x_target_shares = hedgeRatio$beta
      in_long = FALSE
      in_short = TRUE

      target_pct <- as.vector(computeHoldingsPct(y_target_shares,x_target_shares,y[i], x[i]))

      # Update numbers
      y_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$y.holdings[i], dt.summary$y.bid[i], dt.summary$y.shares[i], target_pct[1], brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$capital[i+1] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$y.holdings[i+1] = y_order_update$holdings
      dt.summary$y.shares[i+1] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$x.holdings[i], dt.summary$x.ask[i], dt.summary$x.shares[i], target_pct[2], brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$capital[i+1] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
      dt.summary$x.holdings[i+1] = x_order_update$holdings
      dt.summary$x.shares[i+1] = x_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + as.numeric(x_order_update$brokerage)

      dt.summary$long.pos[i] = -1
      dt.summary$short.pos[i] = 1
      dt.summary$trade[i] = "Go Short"

    }else{
      if(i > 1){
        dt.summary$capital[i] = dt.summary$capital[i-1]
        dt.summary$y.holdings[i] = dt.summary$y.holdings[i-1]
        dt.summary$x.holdings[i] = dt.summary$x.holdings[i-1]
        dt.summary$y.shares[i] = dt.summary$y.shares[i-1]
        dt.summary$x.shares[i] = dt.summary$x.shares[i-1]
        dt.summary$long.pos[i] = dt.summary$long.pos[i-1]
        dt.summary$short.pos[i] = dt.summary$short.pos[i-1]
        # cat("\nWhat!!!!!!!!!!!!!!!!!!!!!!!")
        dt.summary$capital[i+1] = dt.summary$capital[i]
        dt.summary$y.holdings[i+1] = dt.summary$y.holdings[i]
        dt.summary$x.holdings[i+1] = dt.summary$x.holdings[i]
        dt.summary$y.shares[i+1] = dt.summary$y.shares[i]
        dt.summary$x.shares[i+1] = dt.summary$x.shares[i]
        dt.summary$long.pos[i+1] = dt.summary$long.pos[i]
        dt.summary$short.pos[i+1] = dt.summary$short.pos[i]
      }

    }

    dt.summary$in_short[i] = in_short
    dt.summary$in_long[i] = in_long
    dt.summary$hedgeRatio[i] = hedgeRatio$beta
    dt.summary$beta[i] = hedgeRatio$beta
    dt.summary$alpha[i] = hedgeRatio$alpha
  }

  dt.summary$real.capital <- dt.summary$y.bid * dt.summary$y.shares + dt.summary$x.bid * dt.summary$x.shares + dt.summary$capital

  return(dt.summary)
}





#' Real Time Backtesting Function for Modeling - Benchmark
#'
#' Real time backtesting function for modeling with removed survivor biased and bias of using future elements
#'
#' @param context trading context
#' @param datasets a dataset to be used for modeling
#' @param nEval length of periods used for training
#' @param strategyFunc customised function to calculate indicator from \code{datasets}
#' @param strategyParam parameters used in \code{strategyFunc}
#' @param longTrigger a number of thresholds for going long
#' @param shortTrigger a number of thresholds for going short
#' @param exitTrigger a trigger to exit current positions
#'
#' @return A \code{data.frame} with trading activities
#'
#' @seealso \link{getUserTemplate}
#'
#' @export
BackTestingRealTimeBenchmark <- function(context, datasets, nEval = 500, longTrigger = -1, shortTrigger = 1, exitTrigger = 0, strategyFunc = NULL, strategyParam = 5){
  lookback <- context$lookback
  brokerage <- context$brokerage
  capital <- context$capital
  stoploss <- context$stoploss
  in_short = FALSE
  in_long = FALSE
  stdat = nrow(datasets) - lookback + 1
  enddat = nrow(datasets)
  y = datasets$y.close
  y.bid = datasets$y.bid
  y.ask = datasets$y.ask
  x = datasets$x.close
  x.bid = datasets$x.bid
  x.ask = datasets$x.ask
  prices <- merge(y, y.bid, y.ask, x, x.bid, x.ask)
  dt.summary <- createSummarySheet(prices, context$capital)
  i = 0

  for(d in stdat:enddat){
    i = i + 1
    cat(paste0("\nDate: ", index(datasets[d,])))
    # create strategy indicator
    dt = datasets[max(1, d-nEval):d, ]
    half.life <- HalfLifeMeanReversion(dt$price.ratio)$half.life.round; #cat(paste0("Half-Life: ", half.life))

    # skip unnecessary modeling
    zc <- zscores(dt$price.ratio)
    indicator <- tail(zc,1)

    # calculate hedge ratio
    hedgeRatio <- HedgeRatioOLS(dt[max((nrow(dt)-half.life),1):nrow(dt),2],
                                dt[max((nrow(dt)-half.life),1):nrow(dt),3])


    if(d == enddat){
      # Update numbers
      y_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$y.holdings[i], dt.summary$y.bid[i], dt.summary$y.shares[i], 0, brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$x.holdings[i], dt.summary$x.bid[i], dt.summary$x.shares[i], 0, brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + as.numeric(x_order_update$brokerage)

      in_short = FALSE
      in_long = FALSE
      dt.summary$long.pos[i] = 0
      dt.summary$short.pos[i] = 0
      dt.summary$trade[i] = "Exit"

    }else if((in_short & indicator < exitTrigger) | (in_long & indicator > exitTrigger)){
      # Update numbers
      y_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$y.holdings[i], dt.summary$y.bid[i], dt.summary$y.shares[i], 0, brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$capital[i+1] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$y.holdings[i+1] = y_order_update$holdings
      dt.summary$y.shares[i+1] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$x.holdings[i], dt.summary$x.bid[i], dt.summary$x.shares[i], 0, brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$capital[i+1] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
      dt.summary$x.holdings[i+1] = x_order_update$holdings
      dt.summary$x.shares[i+1] = x_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + as.numeric(x_order_update$brokerage)

      in_short = FALSE
      in_long = FALSE
      dt.summary$long.pos[i] = 0
      dt.summary$short.pos[i] = 0
      dt.summary$trade[i] = "Exit"

    }else if(!in_long & indicator < longTrigger){
      # Only trade if NOT already in a trade
      y_target_shares = 1
      x_target_shares = -hedgeRatio$beta
      in_long = TRUE
      in_short = FALSE

      target_pct <- as.vector(computeHoldingsPct(y_target_shares,x_target_shares,y[i], x[i]))

      # Update numbers
      y_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$y.holdings[i], dt.summary$y.ask[i], dt.summary$y.shares[i], target_pct[1], brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$capital[i+1] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.holdings[i+1] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$y.shares[i+1] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$x.holdings[i], dt.summary$x.bid[i], dt.summary$x.shares[i], target_pct[2], brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$capital[i+1] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
      dt.summary$x.holdings[i+1] = x_order_update$holdings
      dt.summary$x.shares[i+1] = x_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + as.numeric(x_order_update$brokerage)


      dt.summary$long.pos[i] = 1
      dt.summary$short.pos[i] = -1
      dt.summary$trade[i] = "Go Long"

    }else if(!in_short & indicator > shortTrigger){
      # Only trade if NOT already in a trade
      y_target_shares = -1
      x_target_shares = hedgeRatio$beta
      in_long = FALSE
      in_short = TRUE

      target_pct <- as.vector(computeHoldingsPct(y_target_shares,x_target_shares,y[i], x[i]))

      # Update numbers
      y_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$y.holdings[i], dt.summary$y.bid[i], dt.summary$y.shares[i], target_pct[1], brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$capital[i+1] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$y.holdings[i+1] = y_order_update$holdings
      dt.summary$y.shares[i+1] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(capital, dt.summary$capital[i], dt.summary$x.holdings[i], dt.summary$x.ask[i], dt.summary$x.shares[i], target_pct[2], brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$capital[i+1] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
      dt.summary$x.holdings[i+1] = x_order_update$holdings
      dt.summary$x.shares[i+1] = x_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + as.numeric(x_order_update$brokerage)

      dt.summary$long.pos[i] = -1
      dt.summary$short.pos[i] = 1
      dt.summary$trade[i] = "Go Short"

    }else{
      if(i > 1){
        dt.summary$capital[i] = dt.summary$capital[i-1]
        dt.summary$y.holdings[i] = dt.summary$y.holdings[i-1]
        dt.summary$x.holdings[i] = dt.summary$x.holdings[i-1]
        dt.summary$y.shares[i] = dt.summary$y.shares[i-1]
        dt.summary$x.shares[i] = dt.summary$x.shares[i-1]
        dt.summary$long.pos[i] = dt.summary$long.pos[i-1]
        dt.summary$short.pos[i] = dt.summary$short.pos[i-1]
        # cat("\nWhat!!!!!!!!!!!!!!!!!!!!!!!")
        dt.summary$capital[i+1] = dt.summary$capital[i]
        dt.summary$y.holdings[i+1] = dt.summary$y.holdings[i]
        dt.summary$x.holdings[i+1] = dt.summary$x.holdings[i]
        dt.summary$y.shares[i+1] = dt.summary$y.shares[i]
        dt.summary$x.shares[i+1] = dt.summary$x.shares[i]
        dt.summary$long.pos[i+1] = dt.summary$long.pos[i]
        dt.summary$short.pos[i+1] = dt.summary$short.pos[i]
      }
    }

    dt.summary$in_short[i] = in_short
    dt.summary$in_long[i] = in_long
    dt.summary$hedgeRatio[i] = hedgeRatio$beta
    dt.summary$beta[i] = hedgeRatio$beta
    dt.summary$alpha[i] = hedgeRatio$alpha
  }

  dt.summary$real.capital <- dt.summary$y.bid * dt.summary$y.shares + dt.summary$x.bid * dt.summary$x.shares + dt.summary$capital

  return(dt.summary)
}
