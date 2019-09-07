#' Calculate Half-Life for Mean-Reversion
#'
#' Computed from an Ornstein–Uhlenbeck process. This is the theoretically computed time, based on a historical window of data,
#' that it will take for the spread to mean-revert half of its distance after having diverged from the mean of the spread.
#'
#' @param price.Ratio price ratio between pairs
#'
#' @return A \code{list} of half life results
#'
#' @examples
#' getFX("AUD/USD")
#' getFX("CAD/USD")
#' half.life <- OrnsteinUhlenbeckHalfLife(AUDUSD/CADUSD)
#' half.life
#'
#' @export
OrnsteinUhlenbeckHalfLife <- function(price.Ratio){
    price.Ratio.lag <- lag(price.Ratio, 1)
    delta.price.Ratio <- diff(price.Ratio)
    df <- cbind(price.Ratio, price.Ratio.lag, delta.price.Ratio)
    df <- df[-1 ,] #remove first row with NAs
    regress.results <- lm(delta.price.Ratio ~ price.Ratio.lag, data = df)
    lambda <- summary(regress.results)$coefficients[2]
    half.life <- -log(2)/lambda
    half.life.round=round(half.life)

    return(list(half.life = half.life,
                half.life.round = half.life.round))
}
