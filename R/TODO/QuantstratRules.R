#' add a rule to a strategy
#'
#' Rules will be processed in a very particular manner, so it bears going over.
#'
#' First, rules are either path dependent or non-path-dependent.  Path dependent rules
#' will be processed in every time increment for the \code{mktdata} passed into
#' \code{\link{applyStrategy}}.  Non path dependent rules will likely be quite rare in real life,
#' and will be applied after indicators and signals, and before path-dependent rules are processed.
#'
#'
#' All rules have a \code{type}.  These may be any of:
#' \describe{
#'   \item{risk}{ rules that check and react to risk of positions, may stop all other rule execution temporarily or permanently}
#'   \item{order}{ rules for order processing of any open orders at time t, always path-dependent}
#'   \item{rebalance}{ rules executed specifically in a portfolio context, unnecessary in univariate strategies}
#'   \item{exit}{ rules to determine whether to exit a position}
#'   \item{enter}{ rules to determine whether to enter or increase a position}
#'   \item{chain}{ rules executed upon fill of an order corresponding to the label of the parent rule identified by the \code{parent} arg. }
#' }
#'
#' The rules will be executed by type, in the order listed above.
#' Multiple rules of each type may be defined, as with signals and indicators,
#' they will be executed in order by index number with any other rules sharing the same
#' type.
#'
#' The rule execution order was constructed because path-dependent rules may modify
#' the ability of rules that have not fired yet to be evaluated.  For example, a
#' risk rule may flatten (close out) an entire position and put new orders
#' on hold, effectively stopping all further execution of the strategy.
#' Another example would be a rebalancing rule function that would enter
#' orders to rebalance the portfolio, and would hold other strategy processing
#' until the rebalancing period was over.
#'
#' The \code{timespan} parameter will limit rule execution by time of day using
#' time based subsetting.  See ISO-8601 specification and xts documentation for
#' more details.  Note that these are only applicable to intra-day execution,
#' and will remain that way barring patches (tests and documentation) from
#' interested parties.  The subsetting may (will likely) work with normal
#' ISO/xts subset ranges, but consider it unsupported.
#'
#' The \code{name} parameter should be a character string naming the function
#' to be called in the \code{\link{applyRules}} loop. The \code{add.rule}
#' function will then call \code{\link{match.fun}}, ands store the actual function
#' in your strategy object.
#' This will avoid lookups via \code{\link{match.fun}} at \code{\link{applyRules}} time,
#' and may provide a significant speed increase on higher frequency data (20\% or more).
#'
#' We anticipate that rules will be the portion of a strategy most likely to
#' not have suitable template code included with this package, as every strategy
#' and environment are different, especially in this respect.
#' We will attempt to provide enough examples and generic rules to give strategy
#' authors a place to start.
#'
#' For quantstrat to be able to (largly) vectorize the execution of path-dependent
#' rule evaluation, the rule function is presumed to have a function signature
#' like that of \code{\link{ruleSignal}}, specifically the arguments \code{sigcol}
#' and \code{sigval}.  If these are present and function in a manner similar to
#' \code{\link{ruleSignal}} we can do some preprocessing to significantly reduce the
#' dimensionality of the index we need to loop over.  The speedup is the ratio of
#' (symbols\*total observations)/signal observations, so it can be significant for many strategies.
#'
#' @param strategy an object of type 'strategy' to add the rule to
#' @param name name of the rule, must correspond to an R function
#' @param arguments named list of default arguments to be passed to an rule function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition
#' @param label arbitrary text label for rule output, NULL default will be converted to '<name>.rule'
#' @param type one of "risk","order","rebalance","exit","enter","chain" see Details
#' @param parent the label of the parent rule for a chain rule
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific rule, the index number in the $rules[type] list to update
#' @param path.dep TRUE/FALSE whether rule is path dependent, default TRUE, see Details
#' @param timespan an xts/ISO-8601 style \emph{time} subset, like "T08:00/T15:00", see Details
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @param storefun TRUE/FALSE whether to store the function in the rule, default TRUE.  setting this option to FALSE may slow the backtest, but makes \code{\link{debug}} usable
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy.
#' @export
add.rule <- function(strategy, name, arguments, parameters=NULL, label=NULL, type=c(NULL,"risk","order","rebalance","exit","enter","chain"), parent=NULL, ..., enabled=TRUE, indexnum=NULL, path.dep=TRUE, timespan=NULL, store=FALSE, storefun=TRUE) {
  if (!is.strategy(strategy)) {
    strategy<-try(get.strategy(strategy))
    if(inherits(strategy,"try-error"))
      stop ("You must supply an object or the name of an object of type 'strategy'.")
    store=TRUE
  }
  type=type[1]
  if(is.null(type)) stop("You must specify a type")
  if(is.na(charmatch(type,c("risk","order","rebalance","exit","enter","chain","pre","post")))) stop(paste("type:",type,' must be one of "risk", "order", "rebalance", "exit", "enter", "chain", "pre", or "post"'))
  tmp_rule<-list()
  if(!is.function(name) && isTRUE(storefun)) {
    if(!is.function(get(name))){
      if(!is.function(get(paste("sig",name,sep='.')))){
        message(paste("Skipping rule",name,"because there is no function by that name to call"))
        next()
      } else {
        name<-paste("sig",rule$name,sep='.')
      }
    }
    fn<-match.fun(name)
  } else {
    fn <- name
  }

  tmp_rule$name<-fn
  tmp_rule$type<-type
  if(type == 'chain')
  {
    if(is.null(parent)) stop("You must specify the label of the parent rule if ruletype=='chain'")
    tmp_rule$parent<-parent
  }
  tmp_rule$enabled<-enabled
  if (!is.list(arguments)) stop("arguments must be passed as a named list")
  if(is.null(label)) label = paste(name,"rule",sep='.')
  tmp_rule$label<-label
  tmp_rule$arguments<-arguments
  if(!is.null(parameters)) tmp_rule$parameters = parameters
  if(!is.null(timespan)) tmp_rule$timespan = timespan
  tmp_rule$path.dep<-path.dep
  if(length(list(...))) tmp_rule<-c(tmp_rule,list(...))

  tmp_rule$call<-match.call()
  class(tmp_rule)<-'trade_rule'
  if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$rules[[type]])+1
  strategy$rules[[type]][[indexnum]]<-tmp_rule

  if (store) assign(strategy$name,strategy,envir=as.environment(.strategyEnv))
  else return(strategy)
  strategy$name
}
