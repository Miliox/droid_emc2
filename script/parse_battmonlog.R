#!/usr/bin/Rscript
args = (commandArgs(TRUE))

if (length(args) == 1) {
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
    hist(battdata$MilliCurrent, breaks=100,
         xlab="discharge current (mA)",
         ylab="count",
         main="Histogram of discharge current")

    ## Energy Consumption Graph
    plot(battdata$ElapsedTime, battdata$MilliCurrent,
         type="l",
         xlab="elapsed time (s)",
         ylab="discharge current (mA)",
         main="current discharge over time")

    ## Voltage change Graph
    plot(battdata$ElapsedTime, battdata$Voltage,
         type="l",
         xlab="elapsed time (s)",
         ylab="battery voltage (V)",
         main="battery voltage over time")

    ## Voltage change Graph
    plot(battdata$ElapsedTime, battdata$MilliPower,
         type="l",
         xlab="elapsed time (s)",
         ylab="power consumption (mW)",
         main="power consumption over time")

    ## Battery discharge Graph
    plot(battdata$ElapsedTime, battdata$Charge,
         type="l",
         xlab="elapsed time (s)",
         ylab="battery charge (%)",
         main="battery charge over time")


} else {
    print("How to use: ./parse_battmonlog.R battery.log")
}

