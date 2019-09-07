#' Draw Economic Calendar Plots
#'
#' Detect the impacts of Economic Calendar on Forex market
#'
#' @param INSTRUMENT instrument of OANDA
#' @param font_size size of font
#'
#'
#' @examples
#' a = drawEconomicCalendar("AUD_USD")
#'
#' @export
drawEconomicCalendar <- function(INSTRUMENTS = "AUD_USD", font_size = 10){

    # 4. Oanda Eco Calendar ---------------------------------------------------
    # load calendar
    Calendar <- unique(EconomicCalendarOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID,
                                             INSTRUMENTS = INSTRUMENTS, PERIOD = '3 months'))
    setorder(Calendar, -timestamp, -impact, currency)
    # load markets data
    Mkt_data = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = INSTRUMENTS,
                                         price = 'BMA', granularity = 'D', count = round(90*5/7+3))

    for(i in 3:ncol(Mkt_data)){
        Mkt_data[, i] <- as.numeric(Mkt_data[, i])
    }
    Mkt_data$Spreads = (Mkt_data$ask.h - Mkt_data$bid.l) * 10
    # h = max(AUD_USD$volume, na.rm = T) / max(Calendar$impact, na.rm = T) / 10
    p1 = ggplot(Mkt_data, aes(x = as.Date(time), y = volume)) +
        geom_rug(sides = "r")+
        xlim(range(as.Date(Mkt_data$time), as.Date(Calendar$timestamp))) +
        geom_area(alpha = 0.7, fill = "skyblue", color = "skyblue", show.legend = FALSE) + xlab(NULL) +
        theme_rquant(rquant_font = T, rquant_colours = T, font_size = font_size, opacity = 0.05, logo = "rquant", inv = 1) +
        theme(axis.text.y = element_blank())
    p2 = ggplot(Calendar, aes(x = as.Date(timestamp), y = impact, fill = currency)) +
        xlim(range(as.Date(Mkt_data$time), as.Date(Calendar$timestamp))) +
        geom_rug(sides = "r")+
        geom_bar(stat = "identity", alpha = 0.6, show.legend = TRUE) +
        xlab("Date") +  ylab("Event Impact") +
        theme_rquant(rquant_font = T, rquant_colours = T, font_size = font_size, opacity = 0.05, logo = "rquant", inv = 1) +
        theme(axis.text.y = element_blank(), legend.position=c(0.1,0.9))
    p3 = ggplot(Mkt_data, aes(x = as.Date(time), y = mid.c)) +
        xlim(range(as.Date(Mkt_data$time), as.Date(Calendar$timestamp))) +
        geom_rug(sides = "r")+geom_line(color = quantColours()[1]) +
        geom_line(data = Mkt_data, aes(x = as.Date(time), y = ask.h), alpha = 0.5, linetype = 3) +
        geom_line(data = Mkt_data, aes(x = as.Date(time), y = bid.l), alpha = 0.5, linetype = 3) +
        ggtitle(paste0(INSTRUMENTS, " - Economic Calendar Event, Forex Volume and Forex Prices")) +
        xlab(NULL) +ylab("Exchange rate") +
        theme_rquant(rquant_font = T, rquant_colours = T, font_size = font_size, opacity = 0.05, logo = "rquant", inv = 2) +
        theme(axis.text.y = element_blank())
    p4 = ggplot(Mkt_data, aes(x = as.Date(time), y = Spreads)) +
        geom_rug(sides = "r")+
        xlim(range(as.Date(Mkt_data$time), as.Date(Calendar$timestamp))) +
        geom_area(alpha = 0.7, fill = "lightgreen", color = "lightgreen", show.legend = FALSE) + xlab(NULL) +
        theme_rquant(rquant_font = T, rquant_colours = T, font_size = font_size, opacity = 0.05, logo = "rquant", inv = 1) +
        theme(axis.text.y = element_blank())


    multiplot(p3,p1,p4,p2, cols = 1)

    return(list(
        EcoCalendar = Calendar,
        p1=p3,
        p2=p1,
        p3=p4,
        p4=p2
    ))
}

