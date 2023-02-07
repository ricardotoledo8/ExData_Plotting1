#       Scritp to my Course Project 1 in Exploratory Data Analysis from 
# J.Hopkins University.
#
#
#       This assignment uses data from the UC Irvine Machine Learning Repository,
# a popular repository for machine learning datasets. In particular, we will be 
# using the “Individual household electric power consumption Data Set” which 
# I have made available on the course web site:
# 
# Dataset: Electric power consumption [20Mb]
# link: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#
# Description: Measurements of electric power consumption in one household with 
# a one-minute sampling rate over a period of almost 4 years. Different 
# electrical quantities and some sub-metering values are available.
#
# The following descriptions of the 9 variables in the dataset are taken 
# from the UCI web site:
#        
# Date: Date in format dd/mm/yyyy
# Time: time in format hh:mm:ss
# Global_active_power: household global minute-averaged active power (in kilowatt)
# Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
# Voltage: minute-averaged voltage (in volt)
# Global_intensity: household global minute-averaged current intensity (in ampere)
# Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). 
#       It corresponds to the kitchen, containing mainly a dishwasher, 
#       an oven and a microwave (hot plates are not electric but gas powered).
# Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). 
#       It corresponds to the laundry room, containing a washing-machine, 
#       a tumble-drier, a refrigerator and a light.
# Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). 
#       It corresponds to an electric water-heater and an air-conditioner.


# Calculating Memory Requirements for the database ----------------------
# The dataset has 2,075,259 rows and 9 columns. 
# First calculate a rough estimate of how much memory the dataset will require 
# in memory before reading into R. Make sure your computer has enough memory 
# (most modern computers should be fine).

# numbers of rows * numbers of columns * 8 bytes 
# and divide by 2^20 is the MB conversion.


memoryAllocation_MB <- ((2075259 * 9 * 8) / (2^20))
# 142.50 MB of ram memory


# Download dataset: ----------------------------------------------------------

# check if data folder exist or create it
if(!file.exists("./data")){dir.create("./data")}

# run download file from server
con <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(con, destfile = "./data/project_Dataset.zip")

# Unzip the dataset
unzip(zipfile = "./data/project_Dataset.zip", exdir = "./data")

# read the data
hpc_data <- read.table("./data/household_power_consumption.txt", 
                       sep = ";", header = TRUE, na.strings = "NA",
                       dec = ".", stringsAsFactors = FALSE)

# convert the character "?" in the NA character for each variable
hpc_data$Global_active_power[hpc_data$Global_active_power == "?"] <- NA_character_
hpc_data$Global_reactive_power[hpc_data$Global_reactive_power == "?"] <- NA_character_
hpc_data$Voltage[hpc_data$Voltage == "?"] <- NA_character_
hpc_data$Global_intensity[hpc_data$Global_intensity == "?"] <- NA_character_
hpc_data$Sub_metering_1[hpc_data$Sub_metering_1 == "?"] <- NA_character_
hpc_data$Sub_metering_2[hpc_data$Sub_metering_2 == "?"] <- NA_character_
hpc_data$Sub_metering_3[hpc_data$Sub_metering_3 == "?"] <- NA_character_


# extract the subset of data with specific Date 
hpc_data <- subset(hpc_data, 
                   hpc_data$Date == "1/2/2007" | hpc_data$Date == "2/2/2007")

# convert classes of variables:
library(dplyr)
hpc_data <- hpc_data %>%
        mutate(DateTime = strptime(paste(Date, Time), "%d/%m/%Y %H:%M:%S")) %>%
        select(-Date, - Time) %>%
        mutate(Global_active_power = as.numeric(Global_active_power)) %>%
        mutate(Global_reactive_power = as.numeric(Global_reactive_power)) %>%
        mutate(Voltage = as.numeric(Voltage)) %>%
        mutate(Global_intensity = as.numeric(Global_intensity )) %>%
        mutate(Sub_metering_1 = as.numeric(Sub_metering_1 )) %>%
        mutate(Sub_metering_2 = as.numeric(Sub_metering_2 )) %>%
        mutate(Sub_metering_3 = as.numeric(Sub_metering_3 ))



# Plot 2 ------------------------------------------------------------------

# Line graphic
# Start png graphic device
png("plot2.png", width = 480, height = 480)

# Graphic
plot(hpc_data$DateTime, hpc_data$Global_active_power, 
     type = "l",
     xlab = "", 
ylab = "Global Active Power (kilowatts)")

# Close the png
dev.off()




