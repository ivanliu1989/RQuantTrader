load("./bloomberg/backtestResult.RData")

all.ts = merge(zScore1_threshold1_ml6_sizing_train201$Strategy,
               zScore1_threshold1_ml6_sizing_train252$Strategy,
               zScore1_threshold1.5_ml6_sizing_train252$Strategy,
               zScore1_threshold1_train201$Strategy,
               zScore1_threshold1.5$Strategy,
               zScore1_threshold2$Strategy,
               zScore1_threshold1$Strategy,
               zScore2_threshold1$Strategy,
               zScore2_threshold1$Benchmark)

all.ts = merge(
    zScore1_thres1_ml6_train252_reg01_xgb07$Strategy ,
    zScore1_thres1_ml6_train252_reg01_xgb006$Strategy,
    zScore1_threshold1_ml6_sizing_train201_reg_ret_pos02_xgb1$Strategy,
    zScore1_threshold1_ml6_sizing_train252_reg_ret_pos02$Strategy,
    zScore1_threshold1_ml6_sizing_train252_reg_ret$Strategy,
    zScore1_threshold1_ml6_sizing_train252_reg_sl$Strategy,
    zScore1_threshold1_ml6_sizing_train252_cl_sl$Strategy,
    zScore1_threshold1_ml6_sizing_train252_cl_sl$Benchmark
)

names(all.ts) = c(paste0('Startegy', 1:7), "Benchmark")
all.ts = na.omit(all.ts)
all.ts = cumsum(na.omit(all.ts))*100


# Define colour of default chart line to chart_Series in mytheme object
# which is passed to chart_Series:
mytheme <- chart_theme()
mytheme$col$line.col <- "darkgreen"
chart_Series(all.ts[,1], theme = mytheme, name = "Backtesting Results, %")
add_TA(all.ts[,2], on = 1, col = "red", lty = 3)
add_TA(all.ts[,3], on = 1, col = "blue", lty =2)
add_TA(all.ts[,4], on = 1, col = "red", lty =1)
add_TA(all.ts[,5], on = 1, col = "green", lty =1)
add_TA(all.ts[,6], on = 1, col = "orange", lty =1)
add_TA(all.ts[,7], on = 1, col = "darkblue", lty =1)
add_TA(all.ts[,8], on = 1, col = "skyblue", lty =1)
add_TA(all.ts[,9], on = 1, col = "black", lty =1)


all.ts = zScore1_threshold1_ml6_sizing_train252_reg_sl$Strategy
for(i in 1:ncol(all.ts)){
    # a = sum(all.ts[,i]>0) / (sum(all.ts[,i]>0) + sum(all.ts[,i]<0))
    # a = sum(all.ts[,i]!=0) / length(all.ts[,i]>0)
    # a = sd(all.ts[,i][all.ts[,i] != 0] * 100)
    # a = mean(all.ts[,i][all.ts[,i] != 0] * 100)
    a = tail(all.ts[,i],1) / (699/252)
    cat(paste0("\n",a))
}




# leverage
leverage.ts = merge(zScore1_threshold1_ml6_sizing_train201$Positions,
               zScore1_threshold1_ml6_sizing_train252$Positions,
               zScore1_threshold1.5_ml6_sizing_train252$Positions,
               zScore1_threshold1_train201$Positions,
               zScore1_threshold1.5$Positions,
               zScore1_threshold2$Positions,
               zScore1_threshold1$Positions,
               zScore2_threshold1$Positions)

leverage.ts = merge(
    zScore1_threshold1_ml6_sizing_train201_reg_ret_pos02_xgb1$Positions,
    zScore1_threshold1_ml6_sizing_train252_reg_ret_pos02$Positions,
    zScore1_threshold1_ml6_sizing_train252_reg_ret$Positions,
    zScore1_threshold1_ml6_sizing_train252_reg_sl$Positions,
    zScore1_threshold1_ml6_sizing_train252_cl_sl$Positions
)
leverage.ts = na.omit(leverage.ts)
for(i in 1:ncol(leverage.ts)){
    a = max(abs(leverage.ts[,i]))
    cat(paste0("\n",a))
}


# max drawdown
for(i in 1:ncol(all.ts)){
    max.drawdown = 0
    for(j in 1:length(all.ts[,i])){
        drawdown = max(all.ts[,i][1:j]) - all.ts[,i][j]
        max.drawdown = max(drawdown, max.drawdown)
    }
    cat(paste0("\n",max.drawdown))
}

