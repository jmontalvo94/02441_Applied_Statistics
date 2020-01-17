#########################################################################
## Case 2:                                                             ##
## HTK Case: Energy performance of buildings                           ##
#########################################################################

# Authors: Begona Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load Data ---------------------------------------------------------------
require("car")
require("dplyr")
require("xtable")
library("stringr")
library("ggpubr")
require("ggplot2")
library("ggExtra")
require("GGally")
require('ggcorrplot')


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
data_1 <- Filter(function(x) length(unique(x))!=1, data_0)

# Check removed columns
setdiff(names(data_0),names(data))

# Change full date to short date
day <- data.frame(str_split_fixed(data_1$date, " ", 2))
day <- day[,-2]
data_1 <- cbind(day, data_1)
data_1 <- data_1[,-2]

# Check summary and structure of data
summary(data_1)
str(data_1)

# Factorize fog, rain, snow, cond, and dir
#data_1$fog <- factor(data_1$fog)
#data_1$rain <- factor(data_1$rain)
#data_1$snow <- factor(data_1$snow)
data_1$cond <- factor(data_1$cond)
data_1$dir <- factor(data_1$dir)

# Sanity-check
str(data_1)

# Calculate mean value for continuous and mode for factor variables
# Create a mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create a data frame for the means and the modes
data_2 <- cbind.data.frame(day)
# it has only unique values, remove repeated dates
data_2 <- unique(data_2)

# get the column names
names <- colnames(data_1)

# Calculate the mean and mode for each colum of the df
for (i in 2:ncol(data_1)){
  if (is.numeric(data_1[,i]) == FALSE){
    values <- cbind.data.frame(data_1$day, data_1[,i])
    colnames(values) <- c("date","value")
    mode_value<- aggregate(values$value,list(values$date), getmode)
    data_2 <- cbind.data.frame(data_2, mode_value)
  }
  if (is.numeric(data_1[,i]) == TRUE){
    values <- cbind.data.frame(data_1$day, data_1[,i])
    colnames(values) <- c("date","value")
    mean_value <- aggregate(values$value,list(values$date), mean)
    data_2 <- cbind.data.frame(data_2, mean_value)
  } 
}

# Erase duplicate dates
data_2 <- data_2[!duplicated(as.list(data_2))]
# Erase an extra column 
data_2 <- data_2[,-1]
# change column names
colnames(data_2) <- names

# Meter -------------------------------------------------------------------



# Analysis ----------------------------------------------------------------
# Load CampusNet Merged Data
df <- read.csv("~/Github/02441_Applied_Statistics/Case2/2_Data/merged_data.csv", header=TRUE, sep=",")

# Set new directory for output files
setwd("~/Github/02441_Applied_Statistics/Case2/4_Images")

# Inspect Data
str(df)
summary(df)
sum_df <- summary(df)
print(xtable(sum_df, type = "latex"), file = "summary_df.tex")

# Data Visualization ------------------------------------------------------

# Pairs plot
temp_interval <- cut(df$temp, 3) # divide temperature in intervals to colour
df_2 <- df[,-c(1,5,8,9)] # erase attributes that are not important
df_3 <- df[,-c(1,2,5,8,9,10,11)] # only numerical attributes
pairs(df_2, col=temp_interval)
pairs(df_2, col=df_2$ID)

# same on ggplot
p1 <- ggpairs(df_2, aes(col = df_2$cond, alpha = 0.4), )
p1 + theme_classic() 

## make them again with more informative attributes 

# Correlation of numerical attributes
df_3 <- df[,-c(1,2,5,8,9,10,11)]
corr <- round(cor(df_3),1)
ggcorrplot(corr)

# a pairs plot of consumption temp and hum
df_4 <- df[,-c(1,2,5,7,8,9,10,11,12,13)] # stay only with consum, temp, hum
p2 <- ggpairs(df_4, aes(colour = as.factor(temp_interval),alpha = 0.4))
str(p2)

# Consumption - Temperature
#scatter
p3 <- ggscatter(df, x="temp",y="consumption",
               col = "blue",
               add = "reg.line", conf.int = TRUE,
               add.params = list(color = "red", fill = "pink"),
               size = 2, alpha = 0.4,
               xlab ="Temperature (ºC)", ylab = "Consumption")
ggMarginal(p3, type = "boxplot")


# scatter coloured by other
p3_2 <- ggplot(df, aes(x = temp, y = consumption, colour=rain))
p3_2 + geom_point() + geom_smooth(method = lm, se = FALSE) + theme_classic() + labs (x ="Temperature (ºC)", y = "Consumption")
 
# Consumption - humidity
p4 <- ggscatter(df, y="consumption", x="hum",
                col = "blue",
                add = "reg.line", conf.int = TRUE,
                add.params = list(color = "red"),
                size = 2, alpha = 0.4,
                ylab ="Consumption", xlab = "Humidity")
ggMarginal(p4, type = "boxplot")

# Fog - humidity
p5 <- ggscatter(df, y="fog", x="hum",
               col = "blue",
               add = "reg.line", conf.int = TRUE,
               add.params = list(color = "red"),
               size = 2, alpha = 0.4,
               ylab ="Fog", xlab = "Humidity")
ggMarginal(p5, type = "boxplot")


##

lm2 <- step(lm(consumption~(.+ I(21-temp) -temp)^2, data = df_2), scope = ~.^4, k = log(nrow(df_2)), trace = FALSE)
drop1(lm2, test = "F")
Anova(lm2)
par(mfrow=c(2,2))
plot(lm2)
bc2 <-boxCox(lm2, lambda = seq(0, 5, by = 0.05))
bc2$x[ which.max(bc2$y) ]

