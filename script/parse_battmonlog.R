#!/usr/bin/Rscript
args = (commandArgs(TRUE))

if (length(args) == 1) {
    library(ggplot2)

    colnames <- c('TimeStamp',
                'ElapsedTime',
                'Charge',
                'MilliCurrent',
                'Voltage',
                'MaxVoltage',
                'MinVoltage',
                'MilliPower',
                'Temperature')

    filename <- args[1]
    battdata <- read.table(file=filename, col.names=colnames)
    maxCurrent <- ceiling(max(battdata$MilliCurrent))

    ## Discharge Histogram
    p1 <- qplot(battdata$MilliCurrent,
        binwidth=1,
        geom="histogram",
        xlab="discharge current (mA)",
        ylab="count",
        main="Histogram of discharge current")

    ## Energy Consumption Graph
    p2 <- qplot(battdata$ElapsedTime, battdata$MilliCurrent,
        geom="line",
        xlab="elapsed time (s)",
        ylab="discharge current (mA)",
        main="current discharge over time")

    ## Voltage change Graph
    p3 <- qplot(battdata$ElapsedTime, battdata$Voltage,
        geom="line",
        xlab="elapsed time (s)",
        ylab="battery voltage (V)",
        main="battery voltage over time")

    ## Voltage change Graph
    p4 <- qplot(battdata$ElapsedTime, battdata$MilliPower,
         geom="line",
         xlab="elapsed time (s)",
         ylab="power consumption (mW)",
         main="power consumption over time")

    ## Battery discharge Graph
    p5 <- qplot(battdata$ElapsedTime, battdata$Charge,
         geom="line",
         xlab="elapsed time (s)",
         ylab="battery charge (%)",
         main="battery charge over time")

    ggsave(filename="discharge_histogram.png",     p1)
    ggsave(filename="discharge_graph.png",         p2)
    ggsave(filename="battery_voltage_graph.png",   p3)
    ggsave(filename="power_consumption_graph.png", p4)
    ggsave(filename="battery_charge_graph.png",    p5)

} else {
    print("How to use: ./parse_battmonlog.R battery.log")
}

