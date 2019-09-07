#' Johansen Cointegration Test
#'
#' Conducts the Johansen procedure on a given data set.
#' The "trace" or "eigen" statistics are reported and the matrix of eigenvectors as well as the loading matrix.
#'
#' @param x Data matrix to be investigated for cointegration.
#' @param type The test to be conducted, either ‘eigen’ or ‘trace’.
#' @param ecdet Character, ‘none’ for no intercept in cointegration, ‘const’ for constant term in cointegration and ‘trend’ for trend variable in cointegration.
#' @param K The lag order of the series (levels) in the VAR.
#'
#' @return A \code{list} of Johansen Cointegration test results
#'
#' @examples
#' getFX("AUD/USD")
#' getFX("CAD/USD")
#' res <- JohansenCointegrationTest(merge(AUDUSD, CADUSD), type = "trace", ecdet = "none", K = 2)
#' res
#'
#' @import urca
#' @export
JohansenCointegrationTest <- function(x, type = "trace", ecdet = "none", K = 2){
    library(urca)
    johansen.test <- summary(ca.jo(x, type=type, ecdet=ecdet, K=K))
    r.1 <- johansen.test@teststat[1]
    p.value = johansen.test@cval[1,]
    signif <- ifelse(r.1 >= p.value["1pct"], 1, ifelse(r.1 >= p.value["5pct"], 5, ifelse(r.1 >= p.value["10pct"], 10, 99)))

    res = list(jc.test = johansen.test,
               r.1 = r.1,
               p.value = p.value,
               signif = signif)
    return(res)
}


