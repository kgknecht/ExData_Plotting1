##############
## C4 - Exploratory Data Analysis
## Course Project 1
## 
## plot2.R
## This function plots a time series diagram of household global minute-averaged active power (in kilowatt)
## for 2007-02-01 and 2007-02-02.
## It includes the following steps:
##  0 - Downloads and extracts the data set zip file if necessary.
##  1 - Retrieves the data for the dates 2007-02-01 and 2007-02-02 and performs necessary type conversions.
##  2 - Plots the time series diagram, adds axis labels, and saves it as png. 
##
## Last edited: 06/09/2021
##############
library(lubridate)

plot2 <- function(){
    
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
    # #Step 2: Plot a time series diagram of global active power using weekdays as x-axis labels.
    # Save it as png.
    png(file = "plot2.png", width = 480, height = 480)
    
    plot.ts(dataSubset$Global_active_power, col= "black", xlab = "", ylab = "Global Active Power (kilowatts)", xaxt="n")
    axis(side = 1, at = c(1, nrow(dataSubset)/2, nrow(dataSubset)), labels = weekdays)

    dev.off()
}