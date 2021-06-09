##############
## C4 - Exploratory Data Analysis
## Course Project 1
## 
## plot1.R
## This function plots a histogram of the frequency of household global minute-averaged active power (in kilowatt).
## It includes the following steps:
##  0 - Downloads and extracts the data set zip file if necessary.
##  1 - Read the data from the dates 2007-02-01 and 2007-02-02 and performs necessary type conversions.
##  2 - Plots the histogram and saves it as png. 
##
## Last edited: 06/09/2021
##############

plot1 <- function(){

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
    # and convert the data in columns Date columns and Time into date and time formats.
    dataSubset <- read.table("./data/household_power_consumption.txt", header=TRUE, sep=";", na.strings = c("?"), skip = 66636, nrows = 2880)
    
    colnames <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    names(dataSubset) <- colnames
    
    dataSubset$Date <- as.Date(dataSubset$Date,"%d/%m/%Y")
    dataSubset$Time <- format(strptime(dataSubset$Time,"%H:%M:%S"), format="%H:%M:%S")
    
    ###
    # #Step 2: Plot a histogram of global active power vs. frequency using respective labels and color it red.
    # Add the title "Global Active Power" to the plot and save it as png.
    png(file = "plot1.png", width = 480, height = 480)
    hist(dataSubset$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power")
    dev.off()
}