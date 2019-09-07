#' Generate a pdf report and email users
#'
#' Generate a pdf report and email users
#'
#' @param performanceReport an object returned from \code{performanceReport}
#' @param to a list of email addresses of report receivers
#' @param path report path
#' @param message additional message in email title
#'
#' @seealso \link{performanceReport}
#'
#' @export
#' @import xtable
#' @import mailR
#' @import data.table

performanceEvaluationEmail <- function(performanceReport, to = c("ivan.liuyanfeng@gmail.com"),
                                       path = "PerformanceEvaluation.pdf", message = NULL){
  library(xtable)
  library(data.table)

  redfocus = c("red","#6D6D6D", "#929292", "#B6B6B6")
  bluefocus = c("#084594", "#6D6D6D", "#929292", "#B6B6B6")
  first.trade <- as.Date(performanceReport$trade.details$Dates[1]-1)
  full.trades <- performanceReport$trade.summary[rownames(performanceReport$trade.summary)>=first.trade,]

  pdf(path, useDingbats=FALSE, width=8, height=9)

  # 1. Performance plots ----------------------------------------------------
  R <- ROC(full.trades[, c("real.capital", "y.close", "x.close")])[-1,]
  colnames(R) <- c("Strategy Based", "Y Series", "X Series")
  charts.PerformanceSummary(R, main = "Strategy Performance Summary", Rf = 0.02, methods = "ModifiedVaR",
                            event.labels = T, wealth.index = T, colorset=bluefocus)
  # charts.RollingPerformance(R[,1:3, drop = FALSE], Rf = 0.02, main = "Rolling Performance", legend.loc = "topleft", colorset=bluefocus)

  # 2. Convergence & Divergence ---------------------------------------------
  # 2.1 Price ratio
  layout(matrix(c(1, 2, 3)), heights = c(2, 1, 1.3), widths = 1)
  par(mar = c(1, 4, 4, 2))
  trade.points <- full.trades[!full.trades$trade == "NO Trade",]
  chart.TimeSeries(full.trades[,c("y.close", "x.close")], event.lines = rownames(trade.points),
                   event.labels = trade.points$trade, ylab = "X. Y Prices", colorset = bluefocus,
                   legend.loc = "topleft", xaxis = FALSE, main = "Convergence & Divergence")
  zc <- zscores(getPriceRatio(full.trades$y.close, full.trades$x.close, FALSE))
  pratio <- data.frame(zscor = zc, low = -1, high = 1)
  rownames(pratio) <- rownames(full.trades)
  par(mar = c(1, 4, 0, 2))
  chart.TimeSeries(pratio, event.lines = rownames(trade.points),event.labels = trade.points$trade,xaxis = FALSE,
                   ylab = "ZScores Price Ratio", colorset = bluefocus,legend.loc = "topleft", main = NA)

  # 2.2 Alpha and Beta
  risk.trades <- full.trades[, c("y.close", "x.close", "alpha", "beta")]
  par(mar = c(5, 4, 0, 2))
  chart.Bar(risk.trades[,c("alpha", "beta")], colorset = bluefocus, legend.loc = "topleft",
            ylab = "Alpha & Beta", main = NA)


  # 3. Scatter and density plots --------------------------------------------
  layout(matrix(c(1, 2, 3)), heights = c(1.5, 1.5, 1.5), widths = 1)
  par(mar = c(2, 4, 4, 2))
  chart.Histogram(R[!R[, "Strategy Based"] == 0,"Strategy Based",drop=F],breaks=100,
                  main = "Strategy Based Returns Distribution",
                  methods = c("add.density", "add.normal","add.centered", "add.rug", "add.risk"))
  chart.Histogram(R[,"Y Series",drop=F], breaks=100,
                  main = "Y Series Returns Distribution",
                  methods = c("add.density", "add.normal","add.centered", "add.rug", "add.risk"))
  par(mar = c(5, 4, 4, 2))
  chart.Histogram(R[,"X Series",drop=F], breaks=100,
                  main = "X Series Returns Distribution",
                  methods = c("add.density", "add.normal","add.centered", "add.rug", "add.risk"))

  layout(matrix(c(1, 2, 3)), heights = c(2, 2, 2), widths = 1)
  par(mar = c(2, 4, 4, 2))
  chart.RiskReturnScatter(R, Rf=.02/12, main = "Risk & Return Scatter", colorset = bluefocus)
  chart.Scatter(performanceReport$trade.details$trade.days, performanceReport$trade.details$returns,
                main = "Holding Days & Return Scatter", ylab = "Returns", xlab = "Holding Days")
  par(mar = c(5, 4, 4, 2))
  tryCatch({chart.VaRSensitivity(R)}, error = function(e){cat("Error!")})
  dev.off()


  # 4. Email ----------------------------------------------------------------
  library(mailR)
  from = "ivan.liuyanfeng@gmail.com"
  subject = "AutoPairTrading - Performance Evaluation Report"
  performanceReport$trade.details$Dates <- as.character(performanceReport$trade.details$Dates)

  perfTable = t(as.data.table(performanceReport$stats.summary))
  colnames(perfTable) = c("Values")
  msg = paste0("<h3>AutoPairTrading Model Notification - ", Sys.Date(), "</h3>",
               "<h4>", message, "</h4>",
               "<h4>Trading Activities:</h4>",
               print(xtable(performanceReport$trade.details), type = "html"),
               "<br>",
               "<h4>Strategy Performance</h4>",
               print(xtable(perfTable), type = "html"),
               "<br>",
               "<p>For more details, please see attached <b>PerformanceEvaluation.pdf</b></p>"
  )
  tryCatch({
    send.mail(from = "ivan.liuyanfeng@gmail.com",
              to = to,
              subject = subject,
              body = msg,
              html = TRUE,
              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "ivan.liuyanfeng@gmail.com", passwd = "Kalmanfilter123", ssl = TRUE),
              authenticate = TRUE,
              attach.files = path,
              send = TRUE)
  }, finally = {
    unlink(path, recursive = T, force = T)
    cat("Reports sent! Please check your email box!")
  })
}








