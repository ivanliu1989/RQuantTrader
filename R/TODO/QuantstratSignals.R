#' add a signal to a strategy
#'
#' This adds a signal definition to a strategy object.
#'
#' Signals denote times at which the strategy \emph{may} want to
#' take action.  Common signals types from the literature include
#' crossovers, thresholds, or other interactions between your \code{mktdata}
#' and your indicators.
#'
#' if \code{label} is not supplied,  NULL default will be converted to '<name>.sig'
#' if the signal function returns one named column, we use that, and ignore the label.
#' If the signal function returns multiple columns, the label will be
#' \code{\link{paste}}'d to either the returned column names or the
#' respective column number.
#'
#' @param strategy an object (or the name of an object) of type 'strategy' to add the signal to
#' @param name name of the signal, must correspond to an R function
#' @param arguments named list of default arguments to be passed to an signal function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition,default NULL, only needed if you need special names to avoid argument collision
#' @param label arbitrary text label for signal output, default NULL
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the signal is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific signal, the index number in the $signals list to update
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy.
#' @seealso
#' \code{\link{applySignals}}
#' \code{\link{add.indicator}}
#' \code{\link{add.rule}}
#' \code{\link{sigComparison}}
#' \code{\link{sigCrossover}}
#' \code{\link{sigFormula}}
#' \code{\link{sigPeak}}
#' \code{\link{sigThreshold}}
#'
#' @export
add.signal <- function(strategy, name, arguments, parameters=NULL, label=NULL, ..., enabled=TRUE, indexnum=NULL, store=FALSE) {
  if (!is.strategy(strategy)) {
    strategy<-try(get.strategy(strategy))
    if(inherits(strategy,"try-error"))
      stop ("You must supply an object or the name of an object of type 'strategy'.")
    store=TRUE
  }
  tmp_signal<-list()
  tmp_signal$name<-name
  if(is.null(label)) label = paste(name,"sig",sep='.')
  tmp_signal$label<-label
  tmp_signal$enabled<-enabled
  if (!is.list(arguments)) stop("arguments must be passed as a named list")
  arguments$label=label
  tmp_signal$arguments<-arguments
  if(!is.null(parameters)) tmp_signal$parameters = parameters
  if(length(list(...))) tmp_signal<-c(tmp_signal,list(...))

  if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$signals)+1
  tmp_signal$call<-match.call()
  class(tmp_signal)<-'strat_signal'
  strategy$signals[[indexnum]]<-tmp_signal

  if (store) assign(strategy$name,strategy,envir=as.environment(.strategyEnv))
  else return(strategy)
  strategy$name
}
