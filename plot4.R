##############
## C4 - Exploratory Data Analysis
## Course Project 1
## 
## plot4.R
## This function plots multiple time series diagrams in a 2 by 2 grid. It includes plots 2 and 3 as well as
## the time series of minute-averaged voltage and household global minute-averaged reactive power (in kilowatt) 
## for 2007-02-01 and 2007-02-02.
##
## This includes the following steps:
##  0 - Downloads and extracts the data set zip file if necessary.
##  1 - Retrieves the data for the dates 2007-02-01 and 2007-02-02 and performs necessary type conversions.
##  2 - Plots the multiple time series in a 2 by 2 grid. 
##
## Last edited: 06/09/2021
##############
library(lubridate)

plot4 <- function(){
    
    ###
    # #Step 0: Preparation - Check if required data set is available, download and unzip data sets if needed
    fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    
    if(!file.exists("data")){
        dir.create("data")
    }
    if(!file.exists("./data/household_power_consumption.zip")){
        download.file(fileUrl, destfile = "./data/household_power_consumption.zip", method = "curl")
        dateDownloaded <- date()
        unzip("./data/household_power_consumption.zip", exdir = "./data")
    }
    
    ###
    # #Step 1: Read the data from the dates 2007-02-01 and 2007-02-02 into the variable dataSubset. Rename the 
    # and convert the data in columns Date columns and Time into date and time formats. Extract weekdays.
    dataSubset <- read.table("./data/household_power_consumption.txt", header=TRUE, sep=";", na.strings = c("?"), skip = 66636, nrows = 2881, strip.white=TRUE)
    
    colnames <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    names(dataSubset) <- colnames
    
    dataSubset$Date <- as.Date(dataSubset$Date,"%d/%m/%Y")
    dataSubset$Time <- format(strptime(dataSubset$Time,"%H:%M:%S"), format="%H:%M:%S")
    
    dates <- unique(dataSubset$Date)
    weekdays <- wday(dates, label=TRUE, abbr=TRUE)
    
    ###
    # #Step 2: Plot multiple time series diagrams in a 2 by 2 grid and save it as png.
    
    png(file = "plot4.png", width = 480, height = 480)
    
    par(mfcol = c(2,2)) # arrange time series plots in a 2 by 2 grid
    
    # top left plot
    plot.ts(dataSubset$Global_active_power, col= "black", xlab = "", ylab = "Global Active Power (kilowatts)", xaxt="n")
    axis(side = 1, at = c(1, nrow(dataSubset)/2, nrow(dataSubset)), labels = weekdays)
    
    # bottom left plot
    plot.ts(dataSubset$Sub_metering_1, col= "black", xlab = "", ylab = "Energy sub metering", xaxt="n")
    lines(dataSubset$Sub_metering_2, col= "red" )
    lines(dataSubset$Sub_metering_3, col= "blue" )
    axis(side = 1, at = c(1, nrow(dataSubset)/2, nrow(dataSubset)), labels = weekdays)
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = c(1, 1, 1), bty = "n")
    
    # top right plot
    plot.ts(dataSubset$Voltage, col= "black", xlab = "datetime", ylab = "Voltage", xaxt="n")
    axis(side = 1, at = c(1, nrow(dataSubset)/2, nrow(dataSubset)), labels = weekdays)
    
    # bottom right plot
    plot.ts(dataSubset$Global_reactive_power, col= "black", xlab = "datetime", ylab = "Global_reactive_power", xaxt="n")
    axis(side = 1, at = c(1, nrow(dataSubset)/2, nrow(dataSubset)), labels = weekdays)
    
    dev.off()
    
}