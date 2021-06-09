##############
## C4 - Exploratory Data Analysis
## Course Project 1
## 
## plot3.R
## This function plots a time series diagram of household energy sub-metering data 1 (kitchen), 2 (laundry room), and
## 3 (electric water heater and air-conditioner) for 2007-02-01 and 2007-02-02.
## It includes the following steps:
##  0 - Downloads and extracts the data set zip file if necessary.
##  1 - Retrieves the data for the dates 2007-02-01 and 2007-02-02 and performs necessary type conversions.
##  2 - Plots the time series, adds axis labels and a legend, and saves it as png. 
##
## Last edited: 06/09/2021
##############
library(lubridate)

plot3 <- function(){
    
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
    # #Step 2: Plot a time series diagram of energy sub-metering data 1, 2, and 3 using weekdays as x-axis labels.
    # Add a legend and save it as png.
    png(file = "plot3.png", width = 480, height = 480)
    
    plot.ts(dataSubset$Sub_metering_1, col= "black", xlab = "", ylab = "Energy sub metering", xaxt="n")
    lines(dataSubset$Sub_metering_2, col= "red" )
    lines(dataSubset$Sub_metering_3, col= "blue" )
    axis(side = 1, at = c(1, nrow(dataSubset)/2, nrow(dataSubset)), labels = weekdays)
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = c(1, 1, 1))
    
    dev.off()
}