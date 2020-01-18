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
require("readxl")


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
# data$fog <- factor(data$fog)
# data$rain <- factor(data$rain)
# data$snow <- factor(data$snow)
data$cond <- factor(data$cond)
data$dir <- factor(data$dir)

# Sanity-check
str(data)

# Calculate mean value for continuous and mode for factor variables
# Create a mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create a data frame for the means and the modes
mean_mode <- cbind.data.frame(day)
# it has only unique values, remove repeated dates
mean_mode <- unique(mean_mode)

# get the column names
names <- colnames(data)

# Calculate the mean and mode for each colum of the df
for (i in 2:ncol(data)){
  if (is.numeric(data[,i]) == FALSE){
    values <- cbind.data.frame(data$day, data[,i])
    colnames(values) <- c("date","value")
    mode_value<- aggregate(values$value,list(values$date), getmode)
    mean_mode <- cbind.data.frame(mean_mode, mode_value)
  }
  if (is.numeric(data[,i]) == TRUE){
    values <- cbind.data.frame(data$day, data[,i])
    colnames(values) <- c("date","value")
    mean_value <- aggregate(values$value,list(values$date), mean)
    mean_mode <- cbind.data.frame(mean_mode, mean_value)
  } 
}

# Erase duplicate dates
mean_mode <- mean_mode[!duplicated(as.list(mean_mode))]
# Erase an extra column 
mean_mode <- mean_mode[,-1]
# change column names
colnames(mean_mode) <- names



# Meter -------------------------------------------------------------------



# Analysis ----------------------------------------------------------------

df <- read_csv("~/Github/02441_Applied_Statistics/Case2/2_Data/merged_data.csv")

# Date to workweek and weekend, per month
df$month <- months(df$date)
df$week <- weekdays(df$date)
workweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")
for (i in workweek) {
  df$week[df$week == i] <- "Wrk"
}
for (i in weekend) {
  df$week[df$week == i] <- "Wknd"
}

df$seasonality <- paste(df$month,df$week)
df <- df[,-c(14,15)]

# Factorize variables
df$seasonality <- factor(df$seasonality)
df$ID <- factor(df$ID)
df$dir <- factor(df$dir)
df$cond <- factor(df$cond)

# Removing direction, visibility, condition, fog, and rain
plot(fog~cond, df)
plot(rain~cond, df) # Condition doesn't seem to be easily interpretable
df <- df[,-c(8,9,11,12,13)]

# Outlier investigation
plot(df$temp, df$consumption, type="p", col=df$ID, pch=19)
plot(consumption~temp, subset(df, ID==78185925), pch=19, col=2)
df <- df[-c(3282,3357),] # Removing outliers 3282 and 3357

# Calculating insulation
lm_u <- lm(consumption~ID*I(21-temp),df)
Anova(lm_u)
summary(lm_u)
df_u <- data.frame(data.frame(lm_u$coefficients)[c(84:166),],row.names=levels(df$ID))
colnames(df_u) <- "u"
df_u$u <- df_u$u+df_u$u[1]
df_u$ID <- levels(df$ID)

# Check dew collinearity
pairs(subset(df, select=c(4:6)))
cor.test(df$temp, df$dew_pt)
df <- df[,-5] # Correlation very high at 0.95, thus remove dew_pt

# Split building type 
type <- data.frame(str_split_fixed(htk$Anvendelse, " ", 2))
type <- type[,-2]
df_type <- cbind.data.frame(htk$Målernr, type)  # merge ID and type 
colnames(df_type) <- c("ID", "type") # rename columns

# Now add new type column to the df 
# df <- merge(df, df_type ,by="ID")

# Data Visualization ------------------------------------------------------

pairs(subset(df, select=c(3:8), col=df$ID))

# Plot of consumption against 21-Temp and say something about variability of each building
# Conclude that each building's variance increases with the size of the building, related to the average consumption of the building
# To normalize we should make a linear model of consumption given temperature per building and 
# then divide consumption by mean consumption of each building

# Models ------------------------------------------------------------------

# Test simple model
lm1 <- lm(consumption~(ID+seasonality)*I(21-temp), df)
Anova(lm1)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1, col=df$ID, pch=19)
par(mfrow=c(1,1))
plot(consumption~I(21-temp),subset(df, ID==78185925), pch=19, col=3)
plot(lm1$residuals~I(21-temp),df, col=df$ID, pch=19)

# Plot Residuals variance per building to see variation
residuals <- unname(lm1$residuals)
variance <- aggregate(residuals, list(df$ID), var)
colnames(variance) <- c("ID","variance")
df <- merge(df, variance, by = "ID")

# Mean consumption by building
mean <- aggregate(df$consumption, list(df$ID), mean)
colnames(mean) <- c("ID","mean")
df <- merge(df, mean, by = "ID")

# Plot mean consumption vs. id
plot(df$mean~df$ID)

buildings <- data.frame(df[df$mean>0.5,])
n <- levels(buildings$ID)
plot(buildings$consumption~I(21-buildings$temp), col=buildings$ID, pch=19)
for (i in 2:length(n)) {
  
}

# Variance per building against mean consumption per building
plot(variance ~ mean, df, col=df$ID, pch=19, log="y")

df$adjconsumption <- df$consumption/df$mean

# Full model with interactions
lm2a <- step(lm(adjconsumption~.+I(21-temp)-date-consumption-temp-variance-mean, df), scope=~.^3, k=log(nrow(df)), trace=FALSE)
Anova(lm2a)
par(mfrow=c(1,1))
plot(lm2a, col=df$ID, pch=19)
sum1 <- summary(lm2a, correlation=TRUE)
corr1 <- data.frame(sum1$correlation)

# Check wind speed, temperature, and pressure - check correlation between those 3
pairs(subset(df, select=c(4,6,7)))

lm2b <- update(lm2a,~.-pressure)
Anova(lm2b)
alias(lm2b)
plot(lm2b, col=df$ID, pch=19)
sum1 <- summary(lm2b, correlation=TRUE)
corr1 <- data.frame(sum1$correlation)