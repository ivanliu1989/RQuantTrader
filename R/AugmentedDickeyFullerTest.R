#' Augmented Dickey Fuller Test
#'
#' ADF test is a unit-root test for determining whether the spread is cointegrated
#'
#' @param y Vector to be tested for a unit root.
#' @param type Test type, either "none", "drift" or "trend".
#' @param lags Number of lags for endogenous variable to be included.
#'
#' @details
#' If type is set to "none" neither an intercept nor a trend is included in the test regression.
#' If it is set to "drift" an intercept is added and if it is set to "trend" both an intercept and a trend is added.
#'
#' @return A \code{list} of ADF test results
#'
#' @examples
#' data(Raotbl3)
#' attach(Raotbl3)
#' lc.df <- AugmentedDickeyFullerTest(y=lc, lags=3, type='trend')
#' lc.df$res
#'
#' @import urca
#' @export
AugmentedDickeyFullerTest <- function(y, type = "drift", lags = 1){
    library(urca)
    lc.df <- summary(ur.df(y=y, lags=lags, type=type))
    z.lag.1 <- lc.df@testreg$coefficients["z.lag.1","t value"]
    tau2 <- lc.df@cval[1,]
    signif <- ifelse(z.lag.1 <= tau2["1pct"], 1, ifelse(z.lag.1 <= tau2["5pct"], 5, ifelse(z.lag.1 <= tau2["10pct"], 10, 99)))
    return(list(res = lc.df,
                z.lag.1 = z.lag.1,
                tau2 = tau2,
                signif = signif))
}
