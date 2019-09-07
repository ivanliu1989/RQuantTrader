files <- list.files("data/", full.names = T)




all.bonds <- na.omit(merge(AUSYC, CANYC, USAYC))

# 1. Uncovered Interest Rate Parity ---------------------------------------
all.bonds$IR.AUSUSA.2 = (all.bonds$AUS2Y+100)/(all.bonds$USA2Y+100)
all.bonds$IR.AUSUSA.3 = (all.bonds$AUS3Y+100)/(all.bonds$USA3Y+100)
all.bonds$IR.AUSUSA.5 = (all.bonds$AUS5Y+100)/(all.bonds$USA5Y+100)
all.bonds$IR.AUSUSA.10 = (all.bonds$AUS10Y+100)/(all.bonds$USA10Y+100)
all.bonds$IR.CANUSA.2 = (all.bonds$CAN2Y+100)/(all.bonds$USA2Y+100)
all.bonds$IR.CANUSA.3 = (all.bonds$CAN3Y+100)/(all.bonds$USA3Y+100)
all.bonds$IR.CANUSA.5 = (all.bonds$CAN5Y+100)/(all.bonds$USA5Y+100)
all.bonds$IR.CANUSA.10 = (all.bonds$CAN10Y+100)/(all.bonds$USA10Y+100)
all.bonds$IR.AUSCAN.2 = (all.bonds$AUS2Y+100)/(all.bonds$CAN2Y+100)
all.bonds$IR.AUSCAN.3 = (all.bonds$AUS3Y+100)/(all.bonds$CAN3Y+100)
all.bonds$IR.AUSCAN.5 = (all.bonds$AUS5Y+100)/(all.bonds$CAN5Y+100)
all.bonds$IR.AUSCAN.10 = (all.bonds$AUS10Y+100)/(all.bonds$CAN10Y+100)

# 2. Calculate Forward Rate -----------------------------------------------
all.bonds$IR.AUS.10.2 = (all.bonds$AUS10Y+100)/(all.bonds$AUS2Y+100)
all.bonds$IR.AUS.10.3 = (all.bonds$AUS10Y+100)/(all.bonds$AUS3Y+100)
all.bonds$IR.AUS.10.5 = (all.bonds$AUS10Y+100)/(all.bonds$AUS5Y+100)
all.bonds$IR.CAN.10.2 = (all.bonds$CAN10Y+100)/(all.bonds$CAN2Y+100)
all.bonds$IR.CAN.10.3 = (all.bonds$CAN10Y+100)/(all.bonds$CAN3Y+100)
all.bonds$IR.CAN.10.5 = (all.bonds$CAN10Y+100)/(all.bonds$CAN5Y+100)

all.bonds = all.bonds[, -c(1:12)]

