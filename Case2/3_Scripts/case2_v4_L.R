#########################################################################
## Case 2:                                                             ##
## HTK Case: Energy performance of buildings                           ##
#########################################################################

# Authors: Bego?a Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load Packages ---------------------------------------------------------------

require("car")
require("tidyverse")
library("stringr")
require("readxl")
require("lubridate")
library("dplyr")

# Visualization packages
require("xtable")
require("ggpubr")
require("ggplot2")
require("ggExtra")
require("GGally")
require('ggcorrplot')
require("gridExtra")


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
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create a data frame for the means and the modes
mean_mode <- cbind.data.frame(day)
# it has only unique values, remove repeated dates
mean_mode <- unique(mean_mode)

# get the column names
names <- colnames(data)

# Empty cells --> NA
data$cond[data$cond==""]  <- NA
data$dir [data$dir ==""]  <- NA

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

# Read all data into a single dataframe
df_raw <- do.call(rbind, lapply(files, read.table, sep=";", dec= ","))
df_raw <- df_raw[,c(1,2,4)]

# Keep only columns 1, 2 and 4 ("ID", "Time" and "Reading")
names(df_raw) <- c("ID", "Time", "Reading") 
df_raw$Reading <- as.numeric(df_raw$Reading)


# Exclude meters with less than 121 records
count_list <- count(df_raw, ID)
count_list <- count_list[count_list$n < 121,]

for (j in 1:nrow(count_list)){
  df_raw<- df_raw[!(df_raw$ID == count_list$ID[j]), ]
}

# Interpolation 
library(stringr)
df_raw$Time <- as.POSIXct(strptime(df_raw$Time, format = "%d-%m-%Y %H.%M"))

# df split in IDs
split_data = split(df_raw, df_raw$ID)

# define the df with Date and ID
day_2 <- data.frame(str_split_fixed(df_raw$Time, " ", 2))
date <- day_2[,1]
df <- cbind(date, df_raw)
df <- df[,c(-3)]

df_new <- data.frame("ID"= 0,"date" = 0, "consumption" = 0)

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
  diff_buil <- diff(inter.result$y)
  date_2 <- date[1:120]
  id_2 <- x$ID[1:120]
  date_diff = data.frame(id_2,date_2, diff_buil)
  colnames(date_diff) <- c('ID','date','consumption')
  df_new <- rbind.data.frame(df_new, date_diff)
  date_diff = data.frame()
}
df_new <-df_new[-1,]
# Merge with WU
colnames(mean_mode) <- c("date", "temp","dew_pt","hum","wind_spd","dir","vis","pressure","cond","fog","rain","snow")  
merged_df <- merge(mean_mode, df_new, by = "date")

summary(merged_df)

# Load Data ---------------------------------------------------------------
# Load CampusNet Merged Data
df <- read_csv("~/Github/02441_Applied_Statistics/Case2/2_Data/merged_data.csv")
summary(df)
# Set new directory for output files
setwd("~/Github/02441_Applied_Statistics/Case2/4_Images")

# Analysis ----------------------------------------------------------------
# Inspect Data
str(df)
summary(df)
sum_df <- summary(df)
print(xtable(sum_df, type = "latex"), file = "summary_df.tex")

# Date to workweek and weekend, per month
df$date <- as.Date(df$date)
df$day <- weekdays(df$date)
df$week <- week(df$date)
df$daytype <- weekdays(df$date)
workweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")
for (i in workweek) {
  df$daytype[df$day == i] <- "Workweek"
}
for (i in weekend) {
  df$daytype[df$day == i] <- "Weekend"
}

# Factorize variables
df$day <- factor(df$day)
df$daytype <- factor(df$daytype)
df$week <- factor(df$week)
df$ID <- factor(df$ID)
df$dir <- factor(df$dir)
df$cond <- factor(df$cond)
df$date <- factor(df$date)
str(df)

# Removing direction, visibility, condition, fog, and rain
plot(fog~cond, df)
plot(rain~cond, df) #it doesn't seem that condition gives interpretable info
df <- df[,-c(8,9,11,12,13)]

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

df_missing <- data.frame(setdiff(unique(df$ID),unique(htk$Målernr)),rep("000",6))
colnames(df_missing) <- c("ID", "type") # rename columns
id_type <- rbind(id_type,df_missing)

# Table of building types for appendix
print(xtable(type_building, type = "latex"), file = "type_building.tex")

# Outlier investigation
outliers <- df[c(3282,3357),] 
#plot(df$temp, df$consumption, type="p", col=df$ID, pch=19)
#plot(consumption~temp, subset(df, ID==78185925), pch=19, col=2)

# Now add new type column to the df 
df$ID <- factor(df$ID)
df <- merge(df, id_type ,by="ID")

# Calculating insulation
lm_u <- lm(consumption~ID*I(21-temp),df)
Anova(lm_u)
summary(lm_u)
# Store in LaTex table
lm_u_a <- Anova(lm_u)
print(xtable(lm_u_a, type = "latex"), file = "lm_u_anova.tex")
lm_u_sum <- summary(lm_u)
print(xtable(lm_u_sum, type = "latex"), file = "lm_u_summary.tex")

df_u <- data.frame(data.frame(lm_u$coefficients)[c(84:166),],row.names=levels(df$ID))
colnames(df_u) <- "u"
df_u$u <- df_u$u+df_u$u[1]
df_u$u[1] <- df_u$u[1]/2
df_u$ID <- levels(df$ID)
u_sum <- summary(df_u)
print(xtable(u_sum, type = "latex"), file = "u_summary.tex")

# Check dew collinearity
#pairs(subset(df, select=c(4:6)))
cor.test(df$temp, df$dew_pt)
temp_interval <- cut(df$temp, 4) # divide temperature in intervals to colour

png(filename="corr_dewpt.png", width=1750, height=1750, res=300)
p00 <- ggpairs(df[, c(4,5,6)], aes(colour = temp_interval),upper = list(continuous = wrap("cor", size = 2.5)))
for(i in 1:p00$nrow) {
  for(j in 1:p00$ncol){
    p00[i,j] <- p00[i,j] + 
      scale_fill_manual(values = rainbow(4)) +
      scale_color_manual(values = rainbow(4))  
  }
}
p00 + theme_classic()
dev.off()

df <- df[,-5] # Correlation very high at 0.95, thus remove dew_pt

# Data visualization ------------------------------------------------------
# Pairs plot
temp_interval <- cut(df$temp, 4) # divide temperature in intervals to colour
df$month <- as.numeric(as.character(as.Date(df$date, format = "%Y-%m-%d"), format="%m"))
df$month <- factor(df$month)
png(filename="pair_plot_whole.png", width=2500, height=1750, res=300)
p0 <- ggpairs(df[, c(3,4,5,6,7,10)], aes(colour = temp_interval),upper = list(continuous = wrap("cor", size = 2.5)))
for(i in 1:p0$nrow) {
  for(j in 1:p0$ncol){
    p0[i,j] <- p0[i,j] + 
      scale_fill_manual(values = rainbow(4))+
      scale_color_manual(values =  rainbow(4))  
  }
}
p0 + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
dev.off()
#pairs(subset(df, select=c(3:8), col=df$ID))


# Consumption - Temp by type
png(filename="cons-temp.png", width=2250, height=1050, res=300)
p1 <- ggplot() + geom_point(data=df, aes(x=(21-temp), y=consumption, col= type))
p1 + scale_color_manual(guide = guide_legend(),values=rainbow(26), name="Building type") +  xlab("Temperature ºC") + ylab("Consumption") + theme_classic() + theme(plot.margin = margin(1, 1,0.1, 1, "cm"),panel.border = element_rect(colour = "black", fill=NA, size=1))
dev.off()

#Building plot type 032 / ID 78185925
# select type and make also building subset
type032 <- subset(df, col=df$type, type =="032", select = ID:type)
id78185925 <- subset(type032, col=type032$ID, ID =="78185925", select = ID:type)

# Outlier investigation
#outliers <- df[c(3282,3357),] 
#plot(df$temp, df$consumption, type="p", col=df$ID, pch=19)
#plot(consumption~temp, subset(df, ID==78185925), pch=19, col=2)

png(filename="78185925.png", width=1750, height=1050, res=300)
p2 <- ggplot() + geom_point(data=type032, aes(x=(21-temp), y=consumption, col="ID 78185925")) + geom_point(data=id78185925, aes(x=(21-temp), y=consumption, col="Other")) + geom_point(data=outliers, aes(x=(21-temp), y=consumption, col="Outliers"))
p2 + scale_color_manual(guide = guide_legend(),values=c("#808080","#00FF00FF", "#FF0000FF" ), name="Sports and swimming (type = 032)") +  xlab("Temperature ºC") + ylab("Consumption") + theme_classic() + theme(legend.position ="bottom",legend.box = 'horizontal',panel.border = element_rect(colour = "black", fill=NA, size=1) )
dev.off()

# After visualizing, remove outliers
df <- df[-c(3282,3357),] # Removing outliers 3282 and 3357

# Plot consumption sum vs type of building
# aggregate consumption SUM
consumption_sum <- aggregate(df$consumption,list(df$type), sum)
colnames(consumption_sum) <- c("type", "cons") # rename columns

png(filename="consum_type.png", width=2050, height=1050, res=300)
p3 <- ggplot(data=consumption_sum, aes(x=type, y=cons, fill=type)) + geom_bar(stat="identity",show.legend = FALSE)
p3 + scale_fill_manual(values=rainbow(25)) + xlab("Type of building") + ylab("Sum of consumption") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
dev.off()

# plot Consumption - date
cons_date_sum <- aggregate(df$consumption,list(id11= df$type, id12= df$date), sum)
colnames(cons_date_sum) <- c("type", "date", "cons") # rename columns
cons_date_sum$date <- as.numeric(cons_date_sum$date)
cons_date_sum$rank <- rank(cons_date_sum$date)

png(filename="consum_type_date.png", width=2050, height=1050, res=300)
p4 <- ggplot(data=cons_date_sum, aes(x=date, y=cons, col=type)) + geom_line()
p4 + scale_color_manual(values=rainbow(25)) + xlab("Date") + ylab("Consumption") + theme_classic() +theme(plot.margin = margin(1, 1,0.1, 1, "cm"),panel.border = element_rect(colour = "black", fill=NA, size=1))
dev.off()

# Models ------------------------------------------------------------------

# Test simple model
lm1 <- lm(consumption~(ID+week)*I(21-temp), df)
Anova(lm1)
summary(lm1) # Comment in the report

# Store in latex table
lm1_a <- Anova(lm1)
print(xtable(lm1_a, type = "latex"), file = "lm1_anova.tex")
lm1_sum <- summary(lm1)
print(xtable(lm1_sum, type = "latex"), file = "lm1_summary.tex")

# Residual plots lm1
#plot1 <- qplot(1)
#plot2 <- qplot(1)
#grid.arrange(plot1, plot2, ncol=2)
png(filename="lm1_14residuals.png", width=2050, height=1750, res=300)
par(mfrow=c(2,2))
plot(lm1, col=df$ID, pch=19,cex = 0.6)
dev.off()

png(filename="lm1_residuals.png", width=2050, height=1750, res=300)
par(mfrow=c(1,1))
plot(lm1$residuals~I(21-temp),df, col=df$ID, pch=19,cex = 0.6, ylab="lm1 Residuals")
dev.off()
# residuals have different variance per building


# Calculate residuals variance per building
df_variance <- aggregate(unname(lm1$residuals), list(df$ID), var)
colnames(df_variance) <- c("ID","variance")
df <- merge(df, df_variance, by = "ID")

# Calculate mean consumption by building
df_mean <- aggregate(df$consumption, list(df$ID), mean)
colnames(df_mean) <- c("ID","mean")
df <- merge(df, df_mean, by = "ID")

# Plot variance per building against mean consumption per building
png(filename="variance_vs_mean_building.png", width=1750, height=1750, res=300)
plot(variance ~ mean, df, col=df$ID, pch=19, cex = 0.6,ylab="Variance", xlab="Mean")
lines(seq(-1,4,length.out=100),rep(0.03,100), col=2) # we propose a threshold of 0.03 variance to identify odd buildings
dev.off()

# Data frame of odd buildings (10) at variance greater than 0.03
df_oddvariance1 <- data.frame(subset(df, variance>0.03))
png(filename="oddbuildings10_var003.png", width=1750, height=1750, res=300)
plot(consumption~I(21-temp), type="p", df_oddvariance1, col=df$ID, pch=19,cex=0.6, ylab="Consumption")
dev.off()
df_oddvariance1 <- droplevels(df_oddvariance1)
oddvariance1 <- unique(df_oddvariance1$ID)

# Plot variance per building against mean consumption per building at lower variance
png(filename="var_mean_zoom.png", width=1750, height=1750, res=300)
plot(variance ~ mean, df,col=df$ID, cex=0.6, pch=19, xlim=c(0,1), ylim=c(0,0.03),ylab="Variance", xlab="Mean")
lines(seq(0,2,length.out=100),rep(0.0075,100), col=2) # 10+3 more outliers with a proposed threshold of 0.0075 variance to identify odd buildings
dev.off()

# Data frame of odd buildings (13) at variance greater than 0.0075
df_oddvariance2 <- data.frame(subset(df, variance>0.0075))
png(filename="oddbuildings13_var00075.png", width=1750, height=1750, res=300)
plot(consumption~I(21-temp), type="p", df_oddvariance2, col=df$ID, cex=0.6, pch=19, ylab="Consumption")
dev.off()
df_oddvariance2 <- droplevels(df_oddvariance2)
oddvariance2 <- unique(df_oddvariance2$ID)

# Plot mean consumption vs. ID
png(filename="mean_ID.png", width=1750, height=1550, res=300)
plot(mean~ID, df, col=df$ID, ylab="Consumption")
lines(df$ID,rep(0.5,length(df$ID)), col=2) # We propose a threshold at mean of 0.5 to "label" odd-buildings
dev.off()

# Data frame of odd buildings (16) at mean greater than 0.5
df_oddmean <- data.frame(subset(df, mean>0.5))
png(filename="oddbuildings16_mean05.png", width=1750, height=1750, res=300)
plot(consumption~I(21-temp), df_oddmean, col=ID, pch=19, ylab="Consumption", cex=0.6) 
dev.off()
df_oddmean <- droplevels(df_oddmean)
oddmeanID5 <- unique(df_oddmean$ID)

# Create data frames to check
df_minus13 <- data.frame(subset(df, variance<0.0075))
df_minus13 <- droplevels(df_minus13)

# Plot consumption against date of odd variance buildings
png(filename="odd_var_vs_date.png", width=1750, height=1550, res=300)
par(mfrow=c(1,1), oma =c(1,2,0,4.5), mar = c(3,2,2,2))
plot(consumption~as.numeric(date), df_oddvariance2, col=ID, pch=19, cex=0.6, xlab="Date", ylab="Consumption")
par(xpd=NA)
legend(x=130, y=4.80, legend=levels(df_oddvariance2$ID), pch=19, col=unique(df_oddvariance2$ID), cex=0.8)
dev.off()
# There are some weird buildings and outliers that we could check

# Plot consumption against date of normal variance buildings
png(filename="normal_var_vs_date.png", width=1750, height=1750, res=300)
par(mfrow=c(1,1))
plot(consumption~as.numeric(date), df_minus13, col=ID, pch=19, cex=0.6, xlab="Temperature ºC", ylab="Consumption")
dev.off()
# There are some weird buildings and outliers that we could check

### Same as before but with temperature

# Plot consumption against temp of odd variance buildings
png(filename="odd_var_vs_temp.png", width=1750, height=1550, res=300)
par(mfrow=c(1,1), oma =c(0,0,0,4.5), mar = c(5,5,2,2))
plot(y=df_oddvariance2$consumption, x=(21-df_oddvariance2$temp), col=df_oddvariance2$ID, pch=19, cex=0.6, xlab="Temperature ºC", ylab="Consumption")
par(xpd=NA)
legend(x=25, y=5.2, legend=levels(df_oddvariance2$ID), pch=19, col=unique(df_oddvariance2$ID), cex=0.8)
dev.off()
# There are some weird buildings and outliers that we could check

# Plot consumption against date of normal variance buildings
png(filename="normal_var_vs_temp.png", width=1750, height=1750, res=300)
par(mfrow=c(1,1))
plot(y=df_minus13$consumption, x=(21-df_minus13$temp), col=df_minus13$ID, pch=19, cex=0.6, xlab="Temperature ºC", ylab="Consumption")
dev.off()
# There are some weird buildings and outliers that we could check





# Plot each building with odd variance
par(mfrow=c(3,5))
for (i in unique(df_oddvariance2$ID)) {
  plot(consumption~as.numeric(date),subset(df,ID==i), pch=19, col=ID, main=paste("ID: ",i), xlab="date")
}
# Here we can identify some weird behaving buildings like 69478883 and 69999051, also remove some outliers and adjust factor to day or week or month
# the consumption has some peaks during the metered period, how can we 'tell' our statistical model to adjust for this

# Plot each building with odd variance per day
par(mfrow=c(3,5))
for (i in unique(df_oddvariance2$ID)) {
  set1 <- subset(df,ID==i)
  plot(consumption~as.numeric(date),set1, pch=19, col=ID, main=paste("ID: ",i), type="n", xlab="date")
  z <- 1
  for (j in unique(df_oddvariance2$day)) {
    set2 <- subset(set1,day==j)
    points(consumption~as.numeric(date),set2, pch=19, col=z)
    z <- z+1
  }
}
# per day doesn't really give us a real difference

# Plot each building with odd variance per day type
par(mfrow=c(3,5))
for (i in unique(df_oddvariance2$ID)) {
  set1 <- subset(df,ID==i)
  plot(consumption~as.numeric(date), set1, pch=19, col=ID, main=paste("ID: ",i), type="n", xlab="date")
  z <- 1
  for (j in unique(df_oddvariance2$daytype)) {
    set2 <- subset(set1,daytype==j)
    points(consumption~as.numeric(date), set2, pch=19, col=z)
    z <- z+1
  }
}
# Doesn't really fixes the peaks but helps to identify that maybe per week is the best factor

# Plot each building with odd variance per week
par(mfrow=c(3,5))
for (i in unique(df_oddvariance2$ID)) {
  set1 <- subset(df,ID==i)
  plot(consumption~as.numeric(date), set1, pch=19, col=ID, main=paste("ID: ",i), type="n", xlab="date")
  z <- 1
  for (j in unique(df_oddvariance2$week)) {
    set2 <- subset(set1,week==j)
    points(consumption~as.numeric(date), set2, pch=19, col=z)
    z <- z+1
  }
}
# Great fit for weekly peaks
# two outliers and two odd buildings in this model




# Adjust consumption to eliminate 'size' of buildings in the model
df$adjconsumption <- df$consumption/df$mean

# Set final data frame
df_model <- df[,-c(2,3,4,8,10,11,12)]
df_model$temp <- 21-df$temp




# Full model with interactions (with adjusted consumption and date as week)
lm2 <- step(lm(adjconsumption~., df_model), scope=~.^3, k=log(nrow(df_model)), trace=FALSE)
anova2 <- Anova(lm2)
par(mfrow=c(2,2))
plot(lm2, col=df$ID, pch=19)
sum2 <- summary(lm2, correlation=TRUE)
corr2 <- data.frame(sum2$correlation)



# Clean df variance and mean again
df <- df[,-c(11,12)]

# Calculate residuals variance per building
df_variance <- aggregate(unname(lm2$residuals), list(df$ID), var)
colnames(df_variance) <- c("ID","variance")
df <- merge(df, df_variance, by = "ID")

# Calculate mean consumption by building
df_mean <- aggregate(df$consumption, list(df$ID), mean)
colnames(df_mean) <- c("ID","mean")
df <- merge(df, df_mean, by = "ID")

# Plot variance per building against mean consumption per building
par(mfrow=c(1,1))
plot(variance ~ mean, df, col=df$ID, pch=19)
lines(seq(-1,4,length.out=100),rep(0.1,100), col=2) # we propose a threshold of 0.1 variance to identify odd buildings

# Data frame of odd buildings (10) at variance greater than 0.1
df_oddvariance <- data.frame(subset(df, variance>0.1))
plot(consumption~I(21-temp), type="p", df_oddvariance, col=ID, pch=19)
df_oddvariance <- droplevels(df_oddvariance)
oddvariance <- unique(df_oddvariance$ID)

# Maybe add 529800 and 7072241 too


# Create data frames to check
df_minus6 <- data.frame(subset(df, variance<0.1))
df_minus6 <- droplevels(df_minus6)





# Plot consumption against date of odd variance buildings
par(mfrow=c(1,1))
plot(consumption~as.numeric(date), df_oddvariance, col=ID, pch=19)
legend("topleft", legend=levels(df_oddvariance$ID), pch=19, col=unique(df_oddvariance$ID), cex=0.8)
# There are some weird buildings and outliers that we could check

# Plot consumption against date of normal variance buildings
plot(consumption~as.numeric(date), df_minus6, col=ID, pch=19)

# Plot each building with odd variance per day
par(mfrow=c(2,3))
for (i in unique(df_oddvariance$ID)) {
  set1 <- subset(df,ID==i)
  plot(consumption~as.numeric(date),set1, pch=19, col=ID, main=paste("ID: ",i), type="n", xlab="date")
  legend("topleft", legend=levels(df_oddvariance$day), pch=19, col=unique(df_oddvariance$day))
  z <- 1
  for (j in unique(df_oddvariance$day)) {
    set2 <- subset(set1,day==j)
    points(consumption~as.numeric(date),set2, pch=19, col=z)
    z <- z+1
  }
}



# Remove outliers

par(mfrow=c(1,1))

plot(consumption~as.numeric(date),subset(df,ID==6392172), col=day, pch=19)
legend("topleft", legend=levels(df$day), pch=19, col=unique(df$day))

plot(consumption~as.numeric(date),data=subset(df,ID==65118755), col=day, pch=19)
legend("topleft", legend=levels(df$day), pch=19, col=unique(df$day))

plot(consumption~as.numeric(date),subset(df,ID==65118764), col=day, pch=19)
legend("topleft", legend=levels(df$day), pch=19, col=unique(df$day))

outliers <- c(1558,1552,1544,1611,1592,1568,1600,1629,1612,1603,3061,3045,3065,2971,2961,2955,3044,2995,3034,3055,3184,3133,3169,3123)

df <- df[-outliers,]

# Remove zeros (because of shutting down?)
df <- subset(df, consumption!=0)

# Remove odd buildings
df <- subset(df, ID!=65118812)
df <- subset(df, ID!=69999051)
df <- droplevels(df)

# Remove september observations
# df <- subset(df,date>)

# Set final data frame
df_model <- df[,-c(2,3,4,8,12,13)]
df_model$temp <- 21-df$temp
df_model <- droplevels(df_model)




# Full model with interactions and without pressure
lm3 <- step(lm(adjconsumption~., df_model), scope=~.^3, k=log(nrow(df_model)), trace=FALSE)
anova3 <- Anova(lm3)
alias(lm3)
par(mfrow=c(2,2))
plot(lm3, col=df$ID, pch=19)
sum3 <- summary(lm3, correlation=TRUE)
corr3 <- data.frame(sum3$correlation)






# Clean df variance and mean again
df <- df[,-c(12,13)]

# Calculate residuals variance per building
df_variance <- aggregate(unname(lm3$residuals), list(df$ID), var)
colnames(df_variance) <- c("ID","variance")
df <- merge(df, df_variance, by = "ID")

# Calculate mean consumption by building
df_mean <- aggregate(df$consumption, list(df$ID), mean)
colnames(df_mean) <- c("ID","mean")
df <- merge(df, df_mean, by = "ID")

# Plot variance per building against mean consumption per building
par(mfrow=c(1,1))
plot(variance ~ mean, df, col=df$ID, pch=19)
lines(seq(-1,4,length.out=100),rep(0.05,100), col=2) # we propose a threshold of 0.1 variance to identify odd buildings

# Data frame of odd buildings (10) at variance greater than 0.1
df_oddvariance <- data.frame(subset(df, variance>0.05))
plot(consumption~I(21-temp), type="p", df_oddvariance, col=ID, pch=19)
df_oddvariance <- droplevels(df_oddvariance)
oddvariance <- unique(df_oddvariance$ID)

# Maybe add 529800 and 7072241 too


# Create data frames to check
df_minus7 <- data.frame(subset(df, variance<0.05))
df_minus7 <- droplevels(df_minus7)





# Plot consumption against date of odd variance buildings
par(mfrow=c(1,1))
plot(consumption~as.numeric(date), df_oddvariance, col=ID, pch=19)
legend("topleft", legend=levels(df_oddvariance$ID), pch=19, col=unique(df_oddvariance$ID), cex=0.8)
# There are some weird buildings and outliers that we could check

# Plot consumption against date of normal variance buildings
plot(consumption~as.numeric(date), df_minus6, col=ID, pch=19)

# Plot each building with odd variance per day
par(mfrow=c(2,4))
for (i in unique(df_oddvariance$ID)) {
  set1 <- subset(df,ID==i)
  plot(consumption~as.numeric(date),set1, pch=19, col=ID, main=paste("ID: ",i), type="n", xlab="date")
  #legend("topleft", legend=levels(df_oddvariance$day), pch=19, col=unique(df_oddvariance$day))
  z <- 1
  for (j in unique(df_oddvariance$day)) {
    set2 <- subset(set1,day==j)
    points(consumption~as.numeric(date),set2, pch=19, col=z)
    z <- z+1
  }
}



# Remove outliers

par(mfrow=c(1,1))

plot(consumption~as.numeric(date),subset(df,ID==6392172), col=day, pch=19)
legend("topleft", legend=levels(df$day), pch=19, col=unique(df$day))

plot(consumption~as.numeric(date),data=subset(df,ID==65118755), col=day, pch=19)
legend("topleft", legend=levels(df$day), pch=19, col=unique(df$day))

plot(consumption~as.numeric(date),subset(df,ID==65118764), col=day, pch=19)
legend("topleft", legend=levels(df$day), pch=19, col=unique(df$day))

outliers <- c(1558,1552,1544,1611,1592,1568,1600,1629,1612,1603,3061,3045,3065,2971,2961,2955,3044,2995,3034,3055,3184,3133,3169,3123)

df <- df[-outliers,]

# Remove zeros (because of shutting down?)
df <- subset(df, consumption!=0)

# Remove odd buildings
df <- subset(df, ID!=65118812)
df <- subset(df, ID!=69999051)


# Set final data frame
df_model <- df[,-c(2,3,4,8,12,13)]
df_model$temp <- 21-df$temp













match <- grep(pattern = "pressure", x = rownames(corr2))
(corr2)[match,match]

# Check wind speed, temperature, and pressure - check correlation between those 3
pairs(subset(df, select=c(4,6,7)))
coplot(wind_spd~temp|pressure,df,panel=panel.smooth)
