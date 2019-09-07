#' Back Testing Strategy
#'
#' Back Testing Strategy
#'
#' @param y long series
#' @param x short series
#' @param context trading context
#' @param strategy a vector of trading indicator calculated by user's strategy
#' @param goLong a vector of thresholds for going long
#' @param goShort a vector of thresholds for going short
#'
#' @return A \code{data.frame} with trading activities
#'
#' @seealso \link{getUserTemplate}
#'
#' @export
BackTesting <- function(y, x, context, strategy, goLong, goShort, exitPoint){
  dt.summary <- createSummarySheet(y,x, context$capital)
  in_short = FALSE
  in_long = FALSE

  for(i in (half.life+1):nrow(y)){
    # create strategy indicator
    indicator <- as.numeric(strategy[i-1])
    exitTrigger <- as.numeric(exitPoint[i-1])
    longTrigger <- as.numeric(goLong[i-1])
    shortTrigger <- as.numeric(goShort[i-1])

    # calculate hedge ratio
    hedgeRatio <- HedgeRatioOLS(y[(i-half.life):(i-1)],
                                x[(i-half.life):(i-1)])

    if(i == nrow(dt.summary)){
      # Update numbers
      y_order_update <- make_order_pct(context$capital, dt.summary$capital[i], dt.summary$y.holdings[i-1], dt.summary$y.prices[i], dt.summary$y.shares[i-1], 0, context$brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(context$capital, dt.summary$capital[i], dt.summary$x.holdings[i-1], dt.summary$x.prices[i], dt.summary$x.shares[i-1], 0, context$brokerage)
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
      y_order_update <- make_order_pct(context$capital, dt.summary$capital[i], dt.summary$y.holdings[i-1], dt.summary$y.prices[i], dt.summary$y.shares[i-1], 0, context$brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$capital[i+1] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(context$capital, dt.summary$capital[i], dt.summary$x.holdings[i-1], dt.summary$x.prices[i], dt.summary$x.shares[i-1], 0, context$brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$capital[i+1] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
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
      y_order_update <- make_order_pct(context$capital, dt.summary$capital[i], dt.summary$y.holdings[i-1], dt.summary$y.prices[i], dt.summary$y.shares[i-1], target_pct[1], context$brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$capital[i+1] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(context$capital, dt.summary$capital[i], dt.summary$x.holdings[i-1], dt.summary$x.prices[i], dt.summary$x.shares[i-1], target_pct[2], context$brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$capital[i+1] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
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
      y_order_update <- make_order_pct(context$capital, dt.summary$capital[i], dt.summary$y.holdings[i-1], dt.summary$y.prices[i], dt.summary$y.shares[i-1], target_pct[1], context$brokerage)
      dt.summary$capital[i] = y_order_update$capital
      dt.summary$capital[i+1] = y_order_update$capital
      dt.summary$y.holdings[i] = y_order_update$holdings
      dt.summary$y.shares[i] = y_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + y_order_update$brokerage

      x_order_update <- make_order_pct(context$capital, dt.summary$capital[i], dt.summary$x.holdings[i-1], dt.summary$x.prices[i], dt.summary$x.shares[i-1], target_pct[2], context$brokerage)
      dt.summary$capital[i] = x_order_update$capital
      dt.summary$capital[i+1] = x_order_update$capital
      dt.summary$x.holdings[i] = x_order_update$holdings
      dt.summary$x.shares[i] = x_order_update$shares
      dt.summary$brokerage[i] = as.numeric(dt.summary$brokerage[i]) + as.numeric(x_order_update$brokerage)

      dt.summary$long.pos[i] = -1
      dt.summary$short.pos[i] = 1
      dt.summary$trade[i] = "Go Short"

    }else{
      dt.summary$long.pos[i] = dt.summary$long.pos[i-1]
      dt.summary$short.pos[i] = dt.summary$short.pos[i-1]
      dt.summary$capital[i] = dt.summary$capital[i-1]
      dt.summary$capital[i+1] = dt.summary$capital[i-1]
      dt.summary$y.holdings[i] = dt.summary$y.holdings[i-1]
      dt.summary$x.holdings[i] = dt.summary$x.holdings[i-1]
      dt.summary$y.shares[i] = dt.summary$y.shares[i-1]
      dt.summary$x.shares[i] = dt.summary$x.shares[i-1]
      dt.summary$long.pos[i] = dt.summary$long.pos[i-1]
      dt.summary$short.pos[i] = dt.summary$short.pos[i-1]
    }

    dt.summary$in_short[i] = in_short
    dt.summary$in_long[i] = in_long
    dt.summary$hedgeRatio[i] = hedgeRatio$beta
    dt.summary$beta[i] = hedgeRatio$beta
    dt.summary$alpha[i] = hedgeRatio$alpha
  }

  dt.summary$real.capital <- dt.summary$y.prices * dt.summary$y.shares + dt.summary$x.prices * dt.summary$x.shares + dt.summary$capital

  return(dt.summary)
}
