#########################################################################
## Case 2:                                                             ##
## HTK Case: Energy performance of buildings                           ##
#########################################################################

# Authors: Begoña Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load Data ---------------------------------------------------------------

require("car")
require("ggplot2")
require("dplyr")
require("xtable")
library(stringr)


# Load data ----------------------------------------------------------

htk <- read_excel("~/Github/02441_Applied_Statistics/Case2/2_Data/HTK_building_data_share.xlsx")
load("~/Github/02441_Applied_Statistics/Case2/2_Data/WUndergroundHourly.RData")
files <- dir("~/Github/02441_Applied_Statistics/Case2/2_Data/meterdata", pattern="*.txt", full.names=TRUE)


# WUnderground ----------------------------------------------------------------

summary(WG)

# Remove NA columns
data_0 <- Filter(function(x)!all(is.na(x)), WG)

# Check removed columns
setdiff(names(WG),names(data_0))

# Remove columns with fixed values
data <- Filter(function(x) length(unique(x))!=1, data_0)

# Check removed columns
setdiff(names(data_0),names(data))

# Change full date to short date
day <- data.frame(str_split_fixed(data$date, " ", 2))
day <- day[,-2]
data<- cbind(day, data)
data <- data[,-2]

# Check summary and structure of data
summary(data)
str(data)

# Factorize fog, rain, snow, cond, and dir
data$fog <- factor(data$fog)
data$rain <- factor(data$rain)
data$snow <- factor(data$snow)
data$cond <- factor(data$cond)
data$dir <- factor(data$dir)

# Sanity-check
str(data)

# Calculate mean value for continuous and mode for factor variables
mean <- data.frame()
mode <- data.frame()
for (i in 2:nrow(data)){
  if (is.numeric(i) == FALSE){
    mode[,i] <- aggregate(data$i, list(Date=data$day), mode)
  }
  if (is.numeric(i) == TRUE){
    mean[,i] <- aggregate(data$i, list(Date=data$day), mean)
  }
}
#mean_temp <- aggregate(data$temp, list(Date=data$day), mean)


# Meter -------------------------------------------------------------------



# Analysis ----------------------------------------------------------------

df <- read_csv("~/Github/02441_Applied_Statistics/Case2/2_Data/merged_data.csv")

str(df)
summary(df)