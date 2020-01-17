#########################################################################
## Case 2:                                                             ##
## HTK Case: Energy performance of buildings                           ##
#########################################################################

# Authors: Begoña Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load Data ---------------------------------------------------------------

library(dplyr)
files <- dir("~/Github/02441_Applied_Statistics/Case2/2_Data/meterdata", pattern = "*.txt", full.names=TRUE)
df <- do.call(rbind, lapply(files, read.table, sep=";", dec= ","))
df <- df[,c(1,2,4)]
names(df) <- c("ID", "Time", "Reading") 
df$Reading <- as.numeric(df$Reading)

count_list <- count(df, ID)
count_list <- count_list[count_list$n < 121,]
#count_good <- subset(count_list, count_list$n == 121)
#common <- intersect(df$ID, count_good$ID)
#df_2 <- df[which(rownames(df$ID) %in% rownames(count_good$ID)),]
 
for (j in 1:nrow(count_list)){
  df<- df[!(df$ID == count_list$ID[j]), ]
}

# 
# for (i in 1:nrow(df)){
#   for (j in common){
#     if (common[j] == df[i,1]){
#       df_2[j] <- df[i,] 
#     } 
#   }
# }

# newdata <- subset(mydata, age >= 20 | age < 10,
#                   select=c(ID, Weight))
# 
# for (i in 1:nrow(countlist)){
#   if (count_list[i,2] > 121){
#     count_list_new 
#   }
# }

library(stringr)
# day <- data.frame(str_split_fixed(df$Time, " ", 2))
# date <- day[,1]
# time <- day[,2]
# df <- cbind(date, df)
# df <- cbind(time, df)
# df <- df[,-4]

df$Time <- as.POSIXct(strptime(df$Time, format = "%d-%m-%Y %H.%M"))

# ### Example with numeric x
# # Make some data
# x <- c(10,20,30,40,50)
# y <- x*4 + rnorm(length(x), 0 , 0.8)
# df <- data.frame(x, y)
# plot(y~x, df)
# # Interpolate at x.inter
# x.inter <- c(15,25,35,45)
# inter.result <- approx(df$x, df$y, xout = x.inter)
# points(inter.result$x, inter.result$y, pch = 17)
# legend("topleft", legend = c("data", "interpolated"), pch = c(1,17))


# plot(Reading~Time, df)
# x.inter <- c('23:00:00', '02:00:00')
# inter.result <- approx(df$Time, df$Reading, xout = x.inter)
# points(inter.result$Time, inter.result$Reading, pch=17)

#df <- df[order(df$ID), ]
#uniques <- unique(df$ID)
split_data = split(df, df$ID)
# x_inter <- list()
# 
# for (d in as.character(dates)) {
#   dat <- as.POSIXct(paste(d, "23:59:00"), format="%Y-%m-%d %H:%M:%S")
#   x_inter <- append(x_inter, dat)
# }

for (i in split_data){
  x <- i
  x <- x[order(x$Time),]
  readings <- x[,c(3)]
  ex <- x[,c(2)]
  #b <- substr(x[,c(2)],1,19)
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


