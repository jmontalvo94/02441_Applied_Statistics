#########################################################################
## Case 2:                                                             ##
## HTK Case: Energy performance of buildings                           ##
#########################################################################

# Authors: Bego?a Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load Data ---------------------------------------------------------------

require("car")
require("tidyverse")
library("stringr")
require("readxl")

# Visualization packages
require("xtable")
require("ggpubr")
require("ggplot2")
require("ggExtra")
require("GGally")
require('ggcorrplot')
require("wesanderson")
library("dplyr")

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

# Factorize cond, and dir
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

files <- dir("~/Github/02441_Applied_Statistics/Case2/2_Data/meterdata", pattern = "*.txt", full.names=TRUE)

# Read all data into a single dataframe
df <- do.call(rbind, lapply(files, read.table, sep=";", dec= ","))
df <- df[,c(1,2,4)]

# Keep only columns 1, 2 and 4 ("ID", "Time" and "Reading")
names(df) <- c("ID", "Time", "Reading") 
df$Reading <- as.numeric(df$Reading)

# Exclude meters with less than 121 records
count_list <- count(df, ID)
count_list <- count_list[count_list$n < 121,]

for (j in 1:nrow(count_list)){
  df<- df[!(df$ID == count_list$ID[j]), ]
}

# Interpolation 
library(stringr)
df$Time <- as.POSIXct(strptime(df$Time, format = "%d-%m-%Y %H.%M"))

split_data = split(df, df$ID)
for (i in split_data){
  x <- i
  x <- x[order(x$Time),]
  readings <- x[,c(3)]
  ex <- x[,c(2)]
  day <- data.frame(str_split_fixed(x$Time, " ", 2))
  date <- day[,1]
  hour <- day[,2]
  plot(readings~ex, xlab="Date", ylab="Readings", col=("Red"))
  x.inter <- list()
  for (element in as.character(date)){
    dat <- as.POSIXct(paste(element, "23:59:00"), format="%Y-%m-%d %H:%M:%S")
    x.inter <- append(x.inter, dat)
  }
  inter.result <- approx(x = ex, y = readings, xout=x.inter)
  points(inter.result$x, inter.result$y, pch = 2)
  legend("topleft", legend = c("data", "interpolated"), pch = c(1,2), col=c("Red", "Black"))
}

# Merge with WU

day_2 <- data.frame(str_split_fixed(df$Time, " ", 2))
date <- day_2[,1]
df <- cbind(date, df)
df <- df[,c(-3)]

names(df) <- c("day", "ID", "Reading")

merged_df <- full_join(mean_mode, df, by = "day")

# Load Data ---------------------------------------------------------------
# Load CampusNet Merged Data
df <- read_csv("~/Github/02441_Applied_Statistics/Case2/2_Data/merged_data.csv")

# Set new directory for output files
setwd("~/Github/02441_Applied_Statistics/Case2/4_Images")

# Analysis ----------------------------------------------------------------
# Inspect Data
str(df)
summary(df)
sum_df <- summary(df[9:14])
print(xtable(sum_df, type = "latex"), file = "summary_df.tex")

# Date to workweek and weekend, per month
df$date <- as.Date(df$date)
df$month <- months(df$date)
df$day <- weekdays(df$date)
workweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")
for (i in workweek) {
  df$day[df$day == i] <- "Wrk"
}
for (i in weekend) {
  df$day[df$day == i] <- "Wknd"
}

df$seasonality <- paste(df$month,df$day)
df <- df[,-c(14,15)]

# Factorize variables
df$seasonality <- factor(df$seasonality)
df$ID <- factor(df$ID)
df$dir <- factor(df$dir)
df$cond <- factor(df$cond)
df$date <- factor(df$date)


# Factorize variables
df$date <- factor(df$date)
df$ID <- factor(df$ID)
df$dir <- factor(df$dir)
df$cond <- factor(df$cond)

# Removing direction, visibility, condition, and fog
plot(fog~cond, df)
plot(rain~cond, df) #it doesn't seem that condition gives interpretable info
df <- df[,-c(8,9,11,12)]

# Split building type from HTK file 
type <- data.frame(str_split_fixed(htk$Anvendelse, " ", 2))

# Get a building type df only
type_building <-type
type_building <- unique(type_building)
colnames(type_building) <- c("type", "name") # rename columns
type_building <- type_building[order(type_building$type),]
zero_type <- data.frame("000", "not defined") # add the row not defined for some buildings
names(zero_type) <- c("type", "name") 
type_building <- rbind(type_building,zero_type)

# data frame with only type code
type <- type[,-2]
id_type <- cbind.data.frame(htk$Målernr, type)  # merge ID and type 
colnames(id_type) <- c("ID", "type") # rename columns

df_missing <- data.frame(setdiff(unique(df$ID),unique(htk$M?lernr)),rep("000",6))
colnames(df_missing) <- c("ID", "type") # rename columns
df_type <- rbind(df_type,df_missing)

# Table of building types for appendix
print(xtable(type_building, type = "latex"), file = "type_building.tex")

# Now add new type column to the df 
df$ID <- factor(df$ID)


# Outlier investigation
plot(df$temp, df$consumption, type="p", col=df$ID, pch=19)
plot(consumption~temp, subset(df, ID==78185925), pch=19, col=2)
outliers <- df[c(3282,3357),] 
df <- df[-c(3282,3357),] # Removing outliers 3282 and 3357

# Calculating insulation
lm_u <- lm(consumption~ID*I(21-temp),df)
Anova(lm_u)
summary(lm_u)
df_u <- data.frame(data.frame(lm_u$coefficients)[c(84:166),],row.names=levels(df$ID))
colnames(df_u) <- "u"
df_u$u <- df_u$u+df_u$u[1]
#df_u$ID <- levels(df$ID)

# Check dew collinearity
pairs(subset(df, select=c(4:6)))
cor.test(df$temp, df$dew_pt)
df <- df[,-5] # Correlation very high at 0.95, thus remove dew_pt

# Data visualization ------------------------------------------------------
# list of 25 colors for type 
mix_cols = c("#D8B70A", "#02523B", "#A2A475", "#81A88D", "#000000","#899DA4", "#98E3DD", "#FAEFD1", "#DC863B","#F1BB7B", "#FD6467", "#5B1A18", "#D67236","#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20","#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E","#E6A0C4", "#0C1707")

# Pairs plot
temp_interval <- cut(df$temp, 4) # divide temperature in intervals to colour
df$month <- as.numeric(as.character(as.Date(df$date, format = "%Y-%m-%d"), format="%m"))
df$month <- factor(df$month)
p0 <- ggpairs(df[, c(3,4,5,6,7,8)], aes(colour = temp_interval))
for(i in 1:p0$nrow) {
  for(j in 1:p0$ncol){
    p0[i,j] <- p0[i,j] + 
      scale_fill_manual(values=wes_palette(n=4, name="Royal1")) +
      scale_color_manual(values=wes_palette(n=4, name="Royal1"))  
  }
}
p0 + theme_classic()
#pairs(subset(df, select=c(3:8), col=df$ID))

# Consumption - Temp by type
p1 <- ggplot() + geom_point(data=df, aes(x=temp, y=consumption, col= type),alpha=0.4)
p1 + scale_color_manual(guide = guide_legend(),values=mix_cols, name="Building type") +  xlab("Temperature ºC") + ylab("Consumption") + theme_classic() + theme(legend.position ="bottom",legend.box = 'horizontal', )

#Building plot type 032 / ID 78185925
# select type and make also building subset
type032 <- subset(df, col=df$type, type =="032", select = ID:type)
id78185925 <- subset(type032, col=type032$ID, ID =="78185925", select = ID:type)

#png(filename="78185925.png", width=1750, height=1050, res=300)
p2 <- ggplot() + geom_point(data=type032, aes(x=temp, y=consumption, col="ID 78185925",alpha=0.3)) + geom_point(data=id78185925, aes(x=temp, y=consumption, col="Other",alpha=0.3)) + geom_point(data=outliers, aes(x=temp, y=consumption, col="Outliers", alpha=0.3))
p2 + scale_color_manual(guide = guide_legend(),values=c("#899DA4","#DC863B","#C93312"), name="Sports and swimming (type = 032)") +  xlab("Temperature ºC") + ylab("Consumption") + theme_classic() + theme(legend.position ="bottom",legend.box = 'horizontal', )
#dev.off()

# Plot consumption sum vs type of building
# aggregate consumption SUM
consumption_sum <- aggregate(df$consumption,list(df$type), sum)
colnames(consumption_sum) <- c("type", "cons") # rename columns

#png(filename="consum_type.png", width=1750, height=1050, res=300)
p3 <- ggplot(data=consumption_sum, aes(x=type, y=cons, fill=type, alpha=0.3)) + geom_bar(stat="identity",show.legend = FALSE)
p3 + scale_fill_manual(values=mix_cols) + xlab("Type of building") + ylab("Consumption") + theme_classic()
#dev.off()

# plot Consumption - date
cons_date_sum <- aggregate(df$consumption,list(id11= df$type, id12= df$date), sum)
colnames(cons_date_sum) <- c("type", "date", "cons") # rename columns
cons_date_sum$date <- as.numeric(cons_date_sum$date)
cons_date_sum$rank <- rank(cons_date_sum$date)

#png(filename="consum_type_date.png", width=1750, height=1050, res=300)
p4 <- ggplot(data=cons_date_sum, aes(x=date, y=cons, col=type)) + geom_line()
p4 + scale_color_manual(values=mix_cols) + xlab("Date") + ylab("Consumption") + theme_classic()
#dev.off()


# Models ------------------------------------------------------------------

# Test simple model
lm_test <- lm(consumption~ID+date+I(21-temp), df)
Anova(lm_test)
summary(lm_test)
par(mfrow=c(2,2))
plot(lm_test)

lm1 <- step(lm(consumption~.-date, df), scope=~.^2, k=log(nrow(df)), trace=FALSE)
Anova(lm1)
plot(lm1)
summary(lm1)