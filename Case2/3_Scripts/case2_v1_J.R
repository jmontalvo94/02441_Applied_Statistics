#########################################################################
## Case 2:                                                             ##
## HTK Case: Energy performance of buildings                           ##
#########################################################################

# Authors: Begoña Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load Data ---------------------------------------------------------------

require("car")
require("ggplot2")
require("xtable")
require("tidyverse")
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
for (i in 2:ncol(data)){
  if (is.numeric(data[1,i]) == FALSE){
    mode[,i] <- aggregate(data[,i], list(day=data$day), mode)
  }
  if (is.numeric(data[1,i]) == TRUE){
    mean[,i] <- aggregate(data[,i], list(day=data$day), mean)
  }
}
mean_temp <- aggregate(data$temp, list(Date=data$day), mean)


# Meter -------------------------------------------------------------------



# Analysis ----------------------------------------------------------------

df <- read_csv("~/Github/02441_Applied_Statistics/Case2/2_Data/merged_data.csv")

# Factorize variables
df$date <- factor(df$date)
df$ID <- factor(df$ID)
df$dir <- factor(df$dir)
df$cond <- factor(df$cond)

# Removing direction, visibility, and fog - future work
df <- df[,-c(8,9,12)]

# Initial investigation
plot(as.numeric(df$date), df$consumption, type="p", col=df$ID, ylim=c(0,10), pch=19)

# Full model but without interactions
lm <- lm(consumption~date+ID+temp+dew_pt+hum+wind_spd+pressure+cond+fog+rain, df)
Anova(lm)
alias(lm) # We found aliased variables that we should remove

# Check dew collinearity
pairs(subset(df, select=c(4:6)))
cor.test(df$temp, df$dew_pt)
# Correlation very high at 0.95, thus remove dew_pt
df <- df[,-5]

# Removing outliers 3357, 3282
df <- df[-c(3282,3357),]

# Checking if condition gives a hint about fog and rain
plot(fog~cond, df)
plot(rain~cond, df)

# Remove condition
df <- df[,-8]

# Calculating insulation
lm_u <- lm(consumption~ID:I(21-temp),df)
Anova(lm_u)
summary(lm_u)
plot(lm_u)

# Data Visualization ------------------------------------------------------

pairs(df)