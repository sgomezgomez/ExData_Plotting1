## plot4.R
## This script does the following:
## 0. Defines and calls the loadingAndReadingData function that loads and reads the required data set
## (see documentation from the loadingAndReadingDataR script for more information)
## 1. Produces the plot4.png plot

## loadingAndReadingData function
## This function does the following:
## 0. Loads required packages and dependencies
## 1. Downloads and unzips the information from the "Electric Power Consumption"
## Data set from UC Irvine Machine Learning Repository
## 2. Reads the dataset file into R
## 3. Subsets data set to match desired dates only (2007-02-01 and 2007-02-02)
## 4. Returns the hpc_data data table
loadingAndReadingData <- function() {
        ## Step 0 - Loading required packages and dependencies
        print('Function loadingAndReadingData: Step 0 - Loading required packages and dependencies')
        dplyr_loaded <- require('dplyr')
        if(!dplyr_loaded) {
                install.packages('dplyr')
                library(dplyr)
        }
        lubridate_loaded <- require('lubridate')
        if(!lubridate_loaded) {
                install.packages('lubridate')
                library(lubridate)
        }
        print('Function loadingAndReadingData: Dependencies and packages loaded')
        
        ## Step 1 - Downloading and extracting the dataset
        print('Function loadingAndReadingData: Step 1 - Downloading and extracting the dataset')
        filename <- 'Coursera_EDA_W1_Project.zip'
        unzippedfilename <- 'household_power_consumption.txt'
        ## Downloading zip file, if non-existent
        if (!file.exists(filename)){
                fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                download.file(fileURL, filename, method = "curl")
                print('Function loadingAndReadingData: Dataset file downloaded')
        } else {
                print('Function loadingAndReadingData: Dataset had already been downloaded')
        }
        ## Unzipping downloaded file, if non-unzipped
        if (!file.exists(unzippedfilename)){
                unzip(filename)
                print('Function loadingAndReadingData: Dataset file unzipped')
        } else {
                print('Function loadingAndReadingData: Dataset file had already been unzipped')
        }
        
        ## Step 2 - Reading the dataset file into R
        print('Function loadingAndReadingData: Step 2 - Reading the dataset file into R')
        hpc_data <- read.table(unzippedfilename, sep = ';', header = TRUE, na.strings = '?')
        hpc_data <- tibble::as_tibble(hpc_data)
        print('Function loadingAndReadingData: Dataset loaded into R')
        hpc_data <- mutate(hpc_data, DateTime = paste(Date, Time))
        hpc_data$Date <- dmy(hpc_data$Date)
        hpc_data$Time <- hms(hpc_data$Time)
        hpc_data$DateTime <- dmy_hms(hpc_data$DateTime)
        hpc_data$Global_active_power <- as.numeric(hpc_data$Global_active_power)
        hpc_data$Global_reactive_power <- as.numeric(hpc_data$Global_reactive_power)
        hpc_data$Voltage <- as.numeric(hpc_data$Voltage)
        hpc_data$Global_intensity <- as.numeric(hpc_data$Global_intensity)
        hpc_data$Sub_metering_1 <- as.numeric(hpc_data$Sub_metering_1)
        hpc_data$Sub_metering_2 <- as.numeric(hpc_data$Sub_metering_2)
        hpc_data$Sub_metering_3 <- as.numeric(hpc_data$Sub_metering_3)
        print('Function loadingAndReadingData: Data set fields formatted appropriately')
        
        ## Step 3 - Subsetting data set to match desired dates only (2007-02-01 and 2007-02-02)
        date_from <- ymd('2007-02-01')
        date_to <- ymd('2007-02-02')
        hpc_data <- hpc_data %>% filter(Date >= date_from & Date <= date_to)
        print('Function loadingAndReadingData: Data set filtered to match date range')
        
        ## Step 4 - Returning the hpc_data data table
        return(hpc_data)
}

## Step 0 - Sourcing R script and loading data set into R
print('plot4.R: Step 0 - Sourcing R script and loading data set into R')
## Sourcing loadingAndReadingData.R r script
## source('loadingAndReadingData.R')
## Calling loadingandReadingData function to retrieve the hpc_data dataset
hpc_data <- loadingAndReadingData()
print('plot4.R: hpc_data data set loaded')

## Step 1 - Producing plot4.png plot
print('plot4.R: Step 1 - Producing plot4.png plot')
## Validating if plot already existed. If so, removing it.
plotname <- 'plot4.png'
if (file.exists(plotname)){
        file.remove(plotname)
        print('plot4.R: Existing plot was removed')
}
## Opening file graphic device
png(plotname, width = 480, height = 480)
par(mfrow = c(2, 2))
## Generating top left plot
plot(hpc_data$DateTime, 
     hpc_data$Global_active_power, 
     lwd = c(0.5),
     type = 'l', 
     xlab = '',
     ylab = 'Global Active Power')
print('plot4.R: Top left plot completed')
## Generating top right plot
plot(hpc_data$DateTime, 
     hpc_data$Voltage, 
     lwd = c(0.5), 
     type = 'l', 
     xlab = 'datetime',
     ylab = 'Voltage')
print('plot4.R: Top right plot completed')
## Generating bottom left plot
plot_columns <- c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3')
plot_colors <- c('black', 'red', 'blue')
matplot(hpc_data$DateTime, 
        select(hpc_data, all_of(plot_columns)), 
        type = c('l', 'l', 'l'),
        lty = c(1),
        lwd = c(0.5), 
        col = plot_colors,
        xlab = '',
        ylab = 'Energy sub metering')
legend('topright', 
       legend = plot_columns, 
       lty = c(1), 
       col = plot_colors,
       bty = 'n')
print('plot4.R: Bottom left plot completed')
## Generating bottom right plot
plot(hpc_data$DateTime, 
     hpc_data$Global_reactive_power, 
     type = 'l', 
     lwd = c(0.5),
     xlab = 'datetime',
     ylab = 'Global_reactive_power')
print('plot4.R: Bottom right plot completed')
## Closing file graphic device
dev.off()
print(paste('plot4.R:', plotname, 'created'))

