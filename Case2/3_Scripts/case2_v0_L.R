#########################################################################
## Case 2:                                                             ##
## HTK Case: Energy performance of buildings                           ##
#########################################################################

# Authors: Begoña Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load Data ---------------------------------------------------------------
load("~/Github/02441_Applied_Statistics/Case2/2_Data/WUndergroundHourly.RData")
# Remove NA columns
data_0 <- Filter(function(x)!all(is.na(x)), WG)
# Remove columns with fixed values
data <- Filter(function(x) length(unique(x))!=1, data_0)

library(stringr)
day <- data.frame(str_split_fixed(data$date, " ", 2))
day <- day[,-2]
data<- cbind(day, data)
data <- data[,-2]

# Factorize fog, rain, snow. it is a binary attribute
data$fog <- factor(data$fog)
data$rain <- factor(data$rain)
data$snow <- factor(data$snow)
data$cond <- factor(data$cond)
data$dir <- factor(data$dir)

str(data)

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
  
