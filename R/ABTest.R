#' Setup New Strategy Directory
#'
#' Setup New Strategy Directory
#'
#' @param strategyName Strategy Name
#' @param strategyPath Path of directory
#'
#' @examples
#' setupNewStrategy("strategy1", "./Common/")
#'
#' @export
setupNewStrategy = function(strategyName, strategyPath){
    mainDir = paste(strategyPath, strategyName, sep = "/")
    dir.create(mainDir)
    dir.create(paste(mainDir, "log",sep = "/"))
    dir.create(paste(mainDir, "models",sep = "/"))
    dir.create(paste(mainDir, "orderbook",sep = "/"))
    dir.create(paste(mainDir, "plots",sep = "/"))

    cat("\nNew strategy directory has been setup.")
    cat(paste0("\n", mainDir, "\n"))

    updateDirFilePermissions(mainDir)
}

#' Store the historical Pricing points from Oanda
#'
#' Store the historical Pricing points from Oanda
#'
#' @param Cur1 Currency 1
#' @param Cur2 Currency 2
#' @param ACCOUNT_TYPE Account type (e.g. "real" or "paper")
#' @param ACCESS_TOKEN Account API Token
#' @param dir Data file directory
#'
#' @examples
#' storeOandaPricing(Cur1 = 'AUD', Cur2 = 'CAD', ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE, ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN, dir = "~/Common/models/AUDCAD_Oanda.RData")
#'
#' @export
storeOandaPricing = function(Cur1 = 'AUD', Cur2 = 'CAD',
                             ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE,
                             ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN,
                             dir = "~/Common/models/AUDCAD_Oanda.RData"){
    pricing_m1 = prepareForexOandaPrices(ACCOUNT_TYPE = ACCOUNT_TYPE,
                                         ACCESS_TOKEN = ACCESS_TOKEN,
                                         oanda.count = 5000, Cur1 = Cur1, Cur2 = Cur2,
                                         oanda.granularity = "M1")$OA.ALL
    pricing_m5 = prepareForexOandaPrices(ACCOUNT_TYPE = ACCOUNT_TYPE,
                                         ACCESS_TOKEN = ACCESS_TOKEN,
                                         oanda.count = 5000, Cur1 = Cur1, Cur2 = Cur2,
                                         oanda.granularity = "M5")$OA.ALL
    pricing_m15 = prepareForexOandaPrices(ACCOUNT_TYPE = ACCOUNT_TYPE,
                                         ACCESS_TOKEN = ACCESS_TOKEN,
                                         oanda.count = 5000, Cur1 = Cur1, Cur2 = Cur2,
                                         oanda.granularity = "M15")$OA.ALL
    pricing_m30 = prepareForexOandaPrices(ACCOUNT_TYPE = ACCOUNT_TYPE,
                                         ACCESS_TOKEN = ACCESS_TOKEN,
                                         oanda.count = 5000, Cur1 = Cur1, Cur2 = Cur2,
                                         oanda.granularity = "M30")$OA.ALL
    pricing_h1 = prepareForexOandaPrices(ACCOUNT_TYPE = ACCOUNT_TYPE,
                                         ACCESS_TOKEN = ACCESS_TOKEN,
                                         oanda.count = 5000, Cur1 = Cur1, Cur2 = Cur2,
                                         oanda.granularity = "H1")$OA.ALL
    pricing_d1 = prepareForexOandaPrices(ACCOUNT_TYPE = ACCOUNT_TYPE,
                                         ACCESS_TOKEN = ACCESS_TOKEN,
                                         oanda.count = 5000, Cur1 = Cur1, Cur2 = Cur2,
                                         oanda.granularity = "D")$OA.ALL

    oandaPricing.new = list(pricing_m1 = pricing_m1,
                        pricing_m5 = pricing_m5,
                        pricing_m15 = pricing_m15,
                        pricing_m30 = pricing_m30,
                        pricing_h1 = pricing_h1,
                        pricing_d1 = pricing_d1)
    if(file.exists(dir)){
        load(file = dir)
        oandaPricing$pricing_m1 = rbind(oandaPricing$pricing_m1, oandaPricing.new$pricing_m1)
        oandaPricing$pricing_m1 = oandaPricing$pricing_m1[!duplicated(index(oandaPricing$pricing_m1))]
        oandaPricing$pricing_m5 = rbind(oandaPricing$pricing_m5, oandaPricing.new$pricing_m5)
        oandaPricing$pricing_m5 = oandaPricing$pricing_m5[!duplicated(index(oandaPricing$pricing_m5))]
        oandaPricing$pricing_m15 = rbind(oandaPricing$pricing_m15, oandaPricing.new$pricing_m15)
        oandaPricing$pricing_m15 = oandaPricing$pricing_m15[!duplicated(index(oandaPricing$pricing_m15))]
        oandaPricing$pricing_m30 = rbind(oandaPricing$pricing_m30, oandaPricing.new$pricing_m30)
        oandaPricing$pricing_m30 = oandaPricing$pricing_m30[!duplicated(index(oandaPricing$pricing_m30))]
        oandaPricing$pricing_h1 = rbind(oandaPricing$pricing_h1, oandaPricing.new$pricing_h1)
        oandaPricing$pricing_h1 = oandaPricing$pricing_h1[!duplicated(index(oandaPricing$pricing_h1))]
        oandaPricing$pricing_d1 = rbind(oandaPricing$pricing_d1, oandaPricing.new$pricing_d1)
        oandaPricing$pricing_d1 = oandaPricing$pricing_d1[!duplicated(index(oandaPricing$pricing_d1))]
        save(oandaPricing, file = dir)
    }else{
        oandaPricing = oandaPricing.new
        save(oandaPricing, file = dir)
    }
}

