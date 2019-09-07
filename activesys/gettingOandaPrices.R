startGettingOandaPrice = function(){
    loadQuantPackages()
    storeOandaPricing(Cur1 = 'AUD', Cur2 = 'CAD', ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE, ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN,
                      dir = "~/Common/models/AUDCAD_Oanda.RData")

    storeOandaPricing(Cur1 = 'USD', Cur2 = 'CAD', ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE, ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN,
                      dir = "~/Common/models/USDCAD_Oanda.RData")

    storeOandaPricing(Cur1 = 'AUD', Cur2 = 'USD', ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE, ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN,
                      dir = "~/Common/models/AUDUSD_Oanda.RData")

    storeOandaPricing(Cur1 = 'EUR', Cur2 = 'USD', ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE, ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN,
                      dir = "~/Common/models/EURUSD_Oanda.RData")

    storeOandaPricing(Cur1 = 'GBP', Cur2 = 'USD', ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE, ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN,
                      dir = "~/Common/models/GBPUSD_Oanda.RData")

    storeOandaPricing(Cur1 = 'USD', Cur2 = 'JPY', ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE, ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN,
                      dir = "~/Common/models/USDJPY_Oanda.RData")

    Sys.sleep(3600 * 24)

    updateDirFilePermissions("~/Common/models")
}

startGettingOandaPrice()
