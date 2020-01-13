#########################################################################
## Case 1:                                                             ##
## Effect of hardness and detergent on enzymatic catalysis             ##
#########################################################################

# Authors: Begoña Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load Data ---------------------------------------------------------------

require("car")
require("ggplot2")

# Load data and clean
data <- read.table("~/Github/02441_Applied_Statistics/Case1/2_Data/SPR.txt", header = TRUE, sep="\t")
data <- data[,-c(1,2)]


# Tests ---------------------------------------------------------

# Testing
par(mfrow=c(1,1))
z <- sort(unique(data$EnzymeConc))
y <- sort(unique(data$EnzymeConc))
x <- 0:3
plot(y~x, pch=19, ylim=c(0,20))
lines(x,exp(x))
par(mfrow=c(1,1))
y <- sqrt(y)
y <- c(y[1],log(y[y>0]))
plot(x,y, col=2, type="l")
lm <- lm(y~x)
lines(x, predict(lm), col=3)

# could be square root or log, log gives a lower residual

# Summary Statistics ------------------------------------------------------

# Structure and summary of both data frames
str(data)
summary(data)


# Data Visualization ------------------------------------------------------

# setwd("~/Github/02441_Applied_Statistics/Case1/4_Images")

# Pairs plot
pairs(data, col=as.numeric(data$Enzyme)+1, pch=19)
par(mfrow=c(1,2))
plot(data$Response~data$DetStock, ylab="Response", xlab="Detergent")
plot(data$Response~data$CaStock, ylab="Response", xlab="Hardness")

cols <- c("black","red", "blue", "green")
col_bg <- adjustcolor(cols, alpha = 0.2) 
cols2 <- c("black","red", "blue", "green",6)
col_bg2 <- adjustcolor(cols2, alpha = 0.2) 
cols3 <- c("black","red")
col_bg3 <- adjustcolor(cols3, alpha = 0.2) 

par(mfrow=c(1,1))

# Response - Enzyme
boxplot(Response~Enzyme, data=data, xlab="Enzyme type", ylab="Protein removal (RU)", 
        col=col_bg2, medcol=cols2, whiskcol=cols2, staplecol=cols2, boxcol=cols2, outcol=cols2, outbg=cols2)
# Response - concentration
boxplot(Response~Conc, data=data, xlab="Enzyme Concentration (nM)", ylab="Protein removal (RU)", 
        col=col_bg, medcol=cols, whiskcol=cols, staplecol=cols, boxcol=cols, outcol=cols,outbg=cols )

# Response - Enzyme - Concentration
par(mfrow = c(1,1))
b <- boxplot(Response ~  EnzymeConc + Enzyme, data = data, xaxt = "n", xlab="Enzyme type",
             col= col_bg, medcol=cols, whiskcol=cols, staplecol=cols, boxcol=cols, outcol=cols,outbg=cols, 
             names =c("","","A","","","","B","","","","C","","","","D","","","","E",""))
axis(side= 1, at=seq_along(b$names), tick = FALSE, labels = b$names)
legend("topright",title="Enzyme concentration", legend = c(0, 2.5, 7.5, 15), fill =cols, horiz =TRUE, cex=0.8)

#png(filename="Response per concentration.png", width=750, height=750)
par(mfrow=c(2,2))
for (i in y){
  plot(data$Response[data$EnzymeConc==i], pch=as.numeric(data$DetStock[data$EnzymeConc==i])+14, col=as.numeric(data$Enzyme[data$EnzymeConc==i]), ylab="Response", xlab="Observations", main=paste("Enzyme concentration: ",exp(i)))
}


for (i in y){
  plot(data$Response[data$EnzymeConc==i], pch=as.numeric(data$DetStock[data$EnzymeConc==i])+14, col=as.numeric(data$Enzyme[data$EnzymeConc==i]), ylab="Response", xlab="Observations", main=paste("Enzyme concentration: ",exp(i)))
}
#dev.off()

plot(data$Response~data$EnzymeConc, pch=19, col=as.numeric(data$Enzyme))

# Model selection (1) ------------------------------------------------------

# Testing response given detergent and hardness for data 
lm1a <- lm(data$Response~data$DetStock)
Anova(lm1a)

lm1b <- lm(data$Response~data$CaStock)
Anova(lm1b)

lm1c <- lm(data$Response~data$DetStock+data$CaStock)
Anova(lm1c)

lm1d <- lm(data$Response~data$DetStock*data$CaStock)
Anova(lm1d)
step(lm1d)

# Hardness doesn't seem to be significant, thus we keep increasing the complexity of the model without it by adding enzyme type
lm1e <- lm(data$Response~data$DetStock+data$Enzyme)
Anova(lm1e)

lm1f <- lm(data$Response~data$DetStock*data$Enzyme)
Anova(lm1f)
step(lm1f)

# Interaction between detergent and enzyme doesn't seem to be significant, add enzyme concentration
lm1g <- lm(data$Response~data$DetStock+data$Enzyme+data$EnzymeConc)
Anova(lm1g)

lm1h <- lm(data$Response~(data$DetStock+data$Enzyme)*data$EnzymeConc)
Anova(lm1h)

lm1i <- lm(data$Response~data$DetStock*(data$Enzyme+data$EnzymeConc))
Anova(lm1i)

drop1(lm1h, test="F")
drop1(lm1i, test="F")

BIC(lm1h, lm1i)
AIC(lm1h, lm1i)

# Full interactions without hardness
lm1j <- lm(data$Response~data$DetStock*data$Enzyme*data$EnzymeConc)
Anova(lm1j)

drop1(lm1j, test="F")

lm1k <- update(lm1j, ~.-data$DetStock:data$Enzyme:data$EnzymeConc)
Anova(lm1k)

drop1(lm1k, test="F")

step(lm1j,k=3.8) # We get the same model by backward selection, thus lm1k is our selected model for the complete dataframe

# Full interactions with hardness
lm1l <- lm(data$Response~data$Enzyme*data$EnzymeConc*data$DetStock*data$CaStock)
step(lm1l, k=3.8)

AIC(lm1a, lm1b, lm1c, lm1d, lm1e, lm1f, lm1g, lm1h, lm1i, lm1k)
BIC(lm1a, lm1b, lm1c, lm1d, lm1e, lm1f, lm1g, lm1h, lm1i, lm1k)

anova(lm1k, lm1h)
# P-value is less than 0.05, the fit is significantly improved for the complex model, hence, we keep the complex model lm1k


# Testing the model (1) -----------------------------------------------------

Anova(lm1k)
summary(lm1k)
#png(filename="LinearModel_Transformed.png", width=750, height=750)
par(mfrow=c(2,2))
plot(lm1k, col=as.numeric(data$Enzyme)+1, pch=19)
#dev.off()

par(mfrow=c(1,1))
plot(lm1k$residuals~data$EnzymeConc, col=as.numeric(data$Enzyme), pch=19)

par(mfrow=c(1,1))
bc <-boxCox(lm1k, lambda = seq(0, 1, by = 0.05))
lam <- bc$x[which.max(bc$y)]

# Transforming the response
#data$Response <- ((data$Response^lam)-1)/lam
data$Response <- data$Response^lam


# Model selection (2) ------------------------------------------------------

# Testing response given detergent and hardness for data 
lm2a <- lm(data$Response~data$DetStock)
Anova(lm2a)

lm2b <- lm(data$Response~data$CaStock)
Anova(lm2b)

lm2c <- lm(data$Response~data$DetStock+data$CaStock)
Anova(lm2c)

lm2d <- lm(data$Response~data$DetStock*data$CaStock)
Anova(lm2d)
step(lm2d)

# Hardness doesn't seem to be significant, thus we keep increasing the complexity of the model without it by adding enzyme type
lm2e <- lm(data$Response~data$DetStock+data$Enzyme)
Anova(lm2e)

lm2f <- lm(data$Response~data$DetStock*data$Enzyme)
Anova(lm2f)
step(lm2f)

# Interaction between detergent and enzyme doesn't seem to be significant, add enzyme concentration
lm2g <- lm(data$Response~data$DetStock+data$Enzyme+data$EnzymeConc)
Anova(lm2g)

lm2h <- lm(data$Response~(data$DetStock+data$Enzyme)*data$EnzymeConc)
Anova(lm2h)

lm2i <- lm(data$Response~data$DetStock*(data$Enzyme+data$EnzymeConc))
Anova(lm2i)

drop1(lm2h, test="F")
drop1(lm2i, test="F")

lm2h <- update(lm2h, ~.-data$DetStock:data$EnzymeConc)
Anova(lm2h)

lm2i <- update(lm2h, ~.-data$DetStock:data$EnzymeConc)
Anova(lm2h)

BIC(lm2h, lm2i)
AIC(lm2h, lm2i)

# Both are the same

# Full interactions without hardness
lm2j <- lm(data$Response~data$DetStock*data$Enzyme*data$EnzymeConc)
Anova(lm2j)

drop1(lm2j, test="F")

lm2k <- update(lm2j, ~.-data$DetStock:data$Enzyme:data$EnzymeConc)
Anova(lm2k)

drop1(lm2k, test="F")

lm2k <- update(lm2k, ~.-data$DetStock:data$EnzymeConc)
Anova(lm2k)

drop1(lm2k, test="F")

lm2k <- update(lm2k, ~.-data$DetStock:data$Enzyme)
Anova(lm2k)

step(lm2j,k=3.8) # We get the same model by backward selection, thus lm1k is our selected model for the complete dataframe

# Full interactions with hardness
lm2l <- lm(data$Response~data$Enzyme*data$EnzymeConc*data$DetStock*data$CaStock)
step(lm2l, k=3.8)

AIC(lm2a, lm2b, lm2c, lm2d, lm2e, lm2f, lm2g, lm2h, lm2i, lm2k)
BIC(lm2a, lm2b, lm2c, lm2d, lm2e, lm2f, lm2g, lm2h, lm2i, lm2k)


# Testing the model -------------------------------------------------------

Anova(lm2k)
summary(lm2k)
#png(filename="LinearModel_Transformed.png", width=750, height=750)
par(mfrow=c(2,2))
plot(lm2k, col=as.numeric(data$Enzyme)+1, pch=19)
#dev.off()

par(mfrow=c(1,1))
plot(lm2k$residuals~data$EnzymeConc, col=as.numeric(data$Enzyme), pch=19)

#data <- data[-14,]

par(mfrow=c(1,1))
bc2 <-boxCox(lm2k, lambda = seq(0, 2, by = 0.05))
lam <- bc2$x[which.max(bc2$y)]

# Transforming the enzyme concentration
data$EnzymeConc <- sqrt(data$EnzymeConc)
# data$EnzymeConc <- log(data$EnzymeConc)
# data$EnzymeConc[data$EnzymeConc=="-Inf"] <- 0


# Model selection (3) ------------------------------------------------------

# Testing response given detergent and hardness for data 
lm3a <- lm(data$Response~data$DetStock)
Anova(lm3a)

lm3b <- lm(data$Response~data$CaStock)
Anova(lm3b)

lm3c <- lm(data$Response~data$DetStock+data$CaStock)
Anova(lm3c)

lm3d <- lm(data$Response~data$DetStock*data$CaStock)
Anova(lm3d)
step(lm3d)

# Hardness doesn't seem to be significant, thus we keep increasing the complexity of the model without it by adding enzyme type
lm3e <- lm(data$Response~data$DetStock+data$Enzyme)
Anova(lm3e)

lm3f <- lm(data$Response~data$DetStock*data$Enzyme)
Anova(lm3f)
step(lm3f)

# Interaction between detergent and enzyme doesn't seem to be significant, add enzyme concentration
lm3g <- lm(data$Response~data$DetStock+data$Enzyme+data$EnzymeConc)
Anova(lm3g)

lm3h <- lm(data$Response~(data$DetStock+data$Enzyme)*data$EnzymeConc)
Anova(lm3h)

lm3i <- lm(data$Response~data$DetStock*(data$Enzyme+data$EnzymeConc))
Anova(lm3i)

drop1(lm3h, test="F")
drop1(lm3i, test="F")

lm3h <- update(lm3h, ~.-data$DetStock:data$EnzymeConc)
Anova(lm3h)

lm3i <- update(lm3h, ~.-data$DetStock:data$EnzymeConc)
Anova(lm3h)

BIC(lm3h, lm3i)
AIC(lm3h, lm3i)

# Both are the same

# Full interactions without hardness
lm3j <- lm(data$Response~data$DetStock*data$Enzyme*data$EnzymeConc)
Anova(lm3j)

drop1(lm3j, test="F")

lm3k <- update(lm3j, ~.-data$DetStock:data$Enzyme:data$EnzymeConc)
Anova(lm3k)

drop1(lm3k, test="F")

lm3k <- update(lm3k, ~.-data$DetStock:data$EnzymeConc)
Anova(lm3k)

drop1(lm3k, test="F")

step(lm3j,k=3.8) # We get the same model by backward selection, thus lm1k is our selected model for the complete dataframe

# Full interactions with hardness
lm3l <- lm(data$Response~data$Enzyme*data$EnzymeConc*data$DetStock*data$CaStock)
step(lm3l, k=3.8)

AIC(lm3a, lm3b, lm3c, lm3d, lm3e, lm3f, lm3g, lm3h, lm3i, lm3k)
BIC(lm3a, lm3b, lm3c, lm3d, lm3e, lm3f, lm3g, lm3h, lm3i, lm3k)


# Testing the model -------------------------------------------------------

Anova(lm3k)
summary(lm3k)
#png(filename="LinearModel_Transformed.png", width=750, height=750)
par(mfrow=c(2,2))
plot(lm3k, col=as.numeric(data$Enzyme)+1, pch=19)
#dev.off()

par(mfrow=c(1,1))
plot(lm3k$residuals~data$EnzymeConc, col=as.numeric(data$Enzyme), pch=19)

# compare residuals between log and sqrt

par(mfrow=c(1,1))
bc3 <-boxCox(lm3k, lambda = seq(0, 2, by = 0.05))
lam <- bc3$x[which.max(bc3$y)]

par(mfrow=c(1,1))
qqPlot(lm3k)

# Should we remove the outliers?
# data <- data[-147,]
# data <- data[-159,]
# data <- data[-c(147,159),]
# lm3k <- lm(data$Response~data$DetStock*data$Enzyme*data$EnzymeConc)
# lm3k <- update(lm2k, ~.-data$DetStock:data$Enzyme:data$EnzymeConc)
# lm3k <- update(lm2k, ~.-data$DetStock:data$EnzymeConc)