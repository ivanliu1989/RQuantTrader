#' add an indicator to a strategy
#'
#' Indicators are typically standard technical or statistical analysis outputs,
#' such as moving averages, bands, or pricing models.
#'
#' Indicators are always path-independent, and should be constructed from vectorized functions where possible.
#'
#' Indicators are applied before signals and rules, and the output of indicators
#' may be used as inputs to construct signals or fire rules.
#'
#' \code{arguments} and \code{parameters} are named lists that describe the arguments to be passed to the indicator function.
#' \code{arguments} is for defining any non-default arguments to be passed to the function named in the \code{name} of the indicator.
#' For example, the \code{x} argument to a moving average function may be defined as \code{x=quote(Cl(mktdata))}
#'
#' If you look at the demo scripts, you'll notice that we often use \code{quote(mktdata)} in setting up indicators, signals, or rules.
#' This tells \R to delay evaluation via \code{quote()}, and to use the special variable \code{mktdata}.
#'
#' \code{mktdata} is typically created internally to \code{quantstrat} by looking in the global environment for
#' a time series of prices or returns. mktdata may also contain other data you've manipulated outside quantstrat,
#' though where possible you should use quantstrat to contain all the logic for the strategy,
#' to aid in maintenance and modifications.
#'
#' The use of \code{quote()} tells R to not evaluate what's inside the quote until the function is evaluated later.
#' By the time that code is evaluated, \code{mktdata} will be populated with the correct price information based on the contents of whatever portfolio you are evaluating the strategy on.
#'
#' \code{parameters} is another named list, and normally will not be needed.
#' If you have multiple indicator, signal, or rule functions share the that
#' \emph{both} share the same argument names \emph{and} will need to have
#' different values passed to those arguments as defined parameters at apply-time,
#' then you may need to give them unique names so that delayed evaluation can
#' sort it all out for you at apply-time.
#' We will endeavor to get an example of named parameters into the demo scripts.
#'
#' if \code{label} is not supplied,  NULL default will be converted to '<name>.ind' unless
#' there already exists an indicator with that label in which case it will be appended
#' with a number (i.e. '<name>.ind.2', '<name>.ind.3', etc.).
#' If the indicator function returns multiple columns, the label will be
#' \code{\link{paste}}'d to the end of either the returned column names or the
#' respective column number when applying it to \code{mktdata}.
#'
#' @param strategy an object (or the name of an object) type 'strategy' to add the indicator to
#' @param name name of the indicator function -- must correspond to an R function
#' @param arguments default arguments to be passed to an indicator function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition,default NULL, only needed if you need special names to avoid argument collision
#' @param label arbitrary text label for indicator output.  This will also be used as the
#' name of the indicator list when it is stored.  NULL default will be converted to '<name>.ind'
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the indicator is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific indicator, the \code{label} or the index number in the $indicators list to update.
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy.
#' @seealso
#' \code{\link{quote}}
#' \code{\link{applyIndicators}}
#' \code{\link{add.signal}}
#' \code{link{add.rule}}
#' @examples
#' \dontrun{
#' strategy("example", store=TRUE)
#' getSymbols("SPY", src='yahoo')
#' add.indicator('example', 'SMA', arguments=list(x=quote(Ad(SPY)), n=20))
#' str(getStrategy('example')$indicators)
#' out <- applyIndicators('example', SPY)
#' tail(out)
#' }
#' @export
add.indicator <- function (strategy, name, arguments, parameters = NULL, label = NULL,
          ..., enabled = TRUE, indexnum = NULL, store = FALSE)
{
  if (!is.strategy(strategy)) {
    strategy <- try(get.strategy(strategy))
    if (inherits(strategy, "try-error"))
      stop("You must supply an object or the name of an object of type 'strategy'.")
    store = TRUE
  }
  tmp_indicator <- list()
  tmp_indicator$name <- name
  if (is.null(label)) {
    label <- paste(name, "ind", sep = ".")
    gl <- grep(label, names(strategy$indicators))
    if (!identical(integer(0), gl))
      label <- paste(label, length(gl) + 1, sep = ".")
  }
  tmp_indicator$label <- label
  tmp_indicator$enabled = enabled
  if (!is.list(arguments))
    stop("arguments must be passed as a named list")
  tmp_indicator$arguments <- arguments
  if (!is.null(parameters))
    tmp_indicator$parameters = parameters
  if (length(list(...)))
    tmp_indicator <- c(tmp_indicator, list(...))
  indexnum <- if (!is.null(indexnum)) {
    indexnum
  }
  else label
  tmp_indicator$call <- match.call()
  class(tmp_indicator) <- "strat_indicator"
  strategy$indicators[[indexnum]] <- tmp_indicator
  if (store)
    assign(strategy$name, strategy, envir = as.environment(.strategyEnv))
  else return(strategy)
  strategy$name
}
