#' constructor for objects of type 'strategy'
#'
#' variables passed in dots will be added to the strategy object, and may
#' be used by initialization and wrapup functions, as well as
#' indicators, signals, and rules.
#'
#' @param name character string naming the strategy
#' @param ... any other passthru parameters
#' @param assets optional list of assets to apply the strategy to, should normally be defined in the portfolio, not here
#' @param constraints optional portfolio constraints object matching assets
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @export
#' @seealso \code{\link{applyStrategy}}
init.strategy <- function (name, ..., assets = NULL, constraints = NULL, store = FALSE)
{
  if (!is.null(assets)) {
    if (is.numeric(assets)) {
      if (length(assets) == 1) {
        nassets = assets
        message("assuming equal weighted seed portfolio")
        assets <- rep(1/nassets, nassets)
      }
      else {
        nassets = length(assets)
      }
      if (is.null(names(assets))) {
        for (i in 1:length(assets)) {
          names(assets)[i] <- paste("Asset", i, sep = ".")
        }
      }
    }
    if (is.character(assets)) {
      nassets = length(assets)
      assetnames = assets
      message("assuming equal weighted seed portfolio")
      assets <- rep(1/nassets, nassets)
      names(assets) <- assetnames
    }
  }
  rules <- list()
  rules$order <- list()
  strat <- structure(list(name = name, assets = assets, indicators = list(),
                          signals = list(), rules = rules, constraints = NULL,
                          init = list(), wrapup = list(), call = match.call()),
                     class = c("strategy"))
  arg <- list(...)
  if (length(arg) >= 1) {
    strat <- c(strat, arg)
    class(strat) <- "strategy"
  }
  if (store)
    assign(strat$name, strat, envir = as.environment(.strategyEnv))
  else return(strat)
}


#' test to see if object is of type 'strategy'
#' @param x object to be tested
#' @export
is.strategy <- function( x ) {
  inherits( x, "strategy" )
}

#' retrieve strategy from the container environment
#' @param x string name of object to be retrieved
#' @param envir the environment to retrieve the strategy object from, defaults to .strategy
#' @rdname get.strategy
#' @aliases
#' get.strategy
#' getStrategy
#' @export get.strategy
get.strategy <- function(x, envir=.strategyEnv){
  tmp_strat<-get(as.character(x),pos=envir, inherits=TRUE)
  if( inherits(tmp_strat,"try-error") | !is.strategy(tmp_strat) ) {
    warning(paste("Strategy",x," not found, please create it first."))
    return(FALSE)
  } else {
    if(is.strategy(tmp_strat)) return(tmp_strat) else return(NULL)
  }
}

.onLoad <- function(lib, pkg) {
  if(!exists('.strategyEnv'))
    .strategyEnv <<- new.env()
}

