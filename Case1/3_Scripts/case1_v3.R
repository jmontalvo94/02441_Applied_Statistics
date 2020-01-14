#########################################################################
## Case 1:                                                             ##
## Effect of hardness and detergent on enzymatic catalysis             ##
#########################################################################

# Authors: Begoña Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load Data ---------------------------------------------------------------

require("car")
require("ggplot2")
require("xtable")

# Load data and clean
data <- read.table("~/Github/02441_Applied_Statistics/Case1/2_Data/SPR.txt", header = TRUE, sep="\t")
df <- data
df$Stock <- as.factor(paste(as.character(df$DetStock),as.character(df$CaStock)))


# Tests ---------------------------------------------------------

# Testing
#png(filename="test1_1.png", width=1750, height=1550, res=300)
par(mfrow=c(1,1))
y <- sort(unique(data$EnzymeConc))
x <- 0:3
par(mfrow=c(1,1))
y2 <- sqrt(y)
plot(x,y2, col=2, type="l", lwd=1, ylab='y', xlab='x',ylim=c(0,20),cex.axis=1)
lm <- lm(y2~x)
lines(x, predict(lm), col=3,lwd=1)
lines(x,x^2, col=4, lty= 2, lwd=1)
lines(x,exp(x), col=6, lty= 2, lwd=1)
points(y~x, pch=19,cex=0.7)
legend("topleft", legend = c("Transformed data - sqrt(y)", "Predicted linear model", "Square Root Function", "Logarithmic Function","Raw data"),
       col = c(2,3,4,6,1), lty=c(1,1,2,2,NA),lwd=1,pch=c(NA,NA,NA,NA,19), cex=0.8)
#dev.off()

# Could be square root or log, log gives a lower residual in this graph but in the model the residual is better with the sqrt

# Summary Statistics ------------------------------------------------------

# Structure and summary of both data frames
str(data)
sum1 <- summary(data)
print(xtable(sum1, type = "latex"), file = "summary1.tex")
data <- data[,-1]

# Data Visualization ------------------------------------------------------

# setwd("~/Github/02441_Applied_Statistics/Case1/4_Images")

#Set up colors
cols <- c("black","red", "blue", "green")
col_bg <- adjustcolor(cols, alpha = 0.2) 
cols2 <- c("black","red", "blue", "green",6)
col_bg2 <- adjustcolor(cols2, alpha = 0.2) 
cols3 <- c("black","red")
col_bg3 <- adjustcolor(cols3, alpha = 0.2) 

# Pairs plot
#png(filename="pairs_1.png", width=1750, height=1750, res=300)
pairs(data, col=round(as.numeric(data$EnzymeConc))+2, pch=as.numeric(data$DetStock)+16)
#dev.off

#png(filename="bp_response_stock_1.png", width=1750, height=1750, res=300)
par(mfrow=c(1,2))
plot(data$Response~data$DetStock, ylab="Response", xlab="Detergent",
     col=col_bg3, medcol=cols3, whiskcol=cols3, staplecol=cols3, boxcol=cols3, outcol=cols3,outbg=cols3)
plot(data$Response~data$CaStock, ylab="Response", xlab="Hardness",
     col=col_bg3, medcol=cols3, whiskcol=cols3, staplecol=cols3, boxcol=cols3, outcol=cols3,outbg=cols3)
#dev.off()

# Response  - Stock
#png(filename="bp_response_stock_2.png", width=1750, height=1750, res=300)
par(mfrow=c(1,1))
boxplot(Response~Stock , data=df, xlab="Conditions (Detergent and Ca2++ combinations)", ylab="Protein removal (RU)", 
        col=col_bg3, medcol=cols3, whiskcol=cols3, staplecol=cols3, boxcol=cols3, outcol=cols3,outbg=cols3 )
#dev.off()

# Response - Enzyme
#png(filename="bp_response_enzyme.png", width=1750, height=1750, res=300)
boxplot(Response~Enzyme, data=df, xlab="Enzyme type", ylab="Protein removal (RU)", 
        col=col_bg2, medcol=cols2, whiskcol=cols2, staplecol=cols2, boxcol=cols2, outcol=cols2, outbg=cols2)
#dev.off()

# Response - concentration
#png(filename="bp_response_conc.png",width=1750, height=1750, res=300)
a <- boxplot(Response~EnzymeConc, data=data, xlab="Enzyme Concentration log(nM)", ylab="Protein removal (RU)", 
             col=col_bg, medcol=cols, whiskcol=cols, staplecol=cols, boxcol=cols, outcol=cols,outbg=cols,
             names=c(0,2.5, 7.5,15))
axis(side= 1, at=seq_along(a$names), tick = FALSE, labels = a$names)
#dev.off()

# Response - Enzyme - Concentration
#png(filename="bp_response_enzyme_conc.png", width=1750, height=1750, res=300)
par(mfrow = c(1,1))
b <- boxplot(Response ~  EnzymeConc + Enzyme, data = data, xaxt = "n", xlab="Enzyme type",
             col= col_bg, medcol=cols, whiskcol=cols, staplecol=cols, boxcol=cols, outcol=cols,outbg=cols, 
             names =c("","","A","","","","B","","","","C","","","","D","","","","E",""))
axis(side= 1, at=seq_along(b$names), tick = FALSE, labels = b$names)
legend("topright",title="Enzyme concentration", legend = c(0, 2.5, 7.5, 15), fill =cols, horiz =TRUE, cex=0.8)
#dev.off()

y <- sort(unique(data$EnzymeConc))

#png(filename="responseXconcentration.png", width=1750, height=1750, res=300)
par(mfrow=c(2,2))
for (i in y){
  plot(data$Response[data$EnzymeConc==i], pch=as.numeric(data$DetStock[data$EnzymeConc==i])+14, col=as.numeric(data$Enzyme[data$EnzymeConc==i]), ylab="Response", xlab="Observations", main=paste("Enzyme concentration: ",i))
  #legend("topleft", legend = as.character(data$Enzyme), cex=0.8)
  }
#dev.off()

par(mfrow=c(1,1))
plot(data$Response~as.numeric(data$Enzyme), pch=as.numeric(as.factor(data$EnzymeConc))+14, col=as.numeric(data$DetStock))
plot(data$Response~data$EnzymeConc, pch=19, col=as.numeric(data$Enzyme))

# BoxCox of maximal model -------------------------------------------------

# Maximal model with full interactions
lm1 <- lm(data$Response~data$Cycle*data$Enzyme*data$EnzymeConc*data$DetStock*data$CaStock)

# Diagnostics
Anova(lm1)
summary(lm1)
#png(filename="LinearModel_Transformed.png", width=750, height=750)
par(mfrow=c(2,2))
plot(lm1, col=as.numeric(data$EnzymeConc)+1, pch=19)
#dev.off()

# Residuals
par(mfrow=c(1,1))
plot(lm1$residuals~data$EnzymeConc, col=as.numeric(data$DetStock)+1, pch=19)

# BoxCos Transformation
par(mfrow=c(1,1))
bc <- boxCox(lm1, lambda = seq(0, 1, by = 0.05))
lam1 <- bc$x[which.max(bc$y)]

data$Response <- data$Response^lam1


# Model selection (2) ------------------------------------------------------

# Model with full interactions
lm2a <- lm(data$Response~data$Cycle*data$Enzyme*data$EnzymeConc*data$DetStock*data$CaStock)
step(lm2a, k=3.8)

# Reduced model
lm2b <- lm(formula = data$Response ~ data$Enzyme + data$EnzymeConc + data$DetStock + data$Enzyme:data$EnzymeConc)

# Testing the model -------------------------------------------------------

# Model diagnostics
Anova(lm2b)
summary(lm2b)
#png(filename="LinearModel_Transformed.png", width=750, height=750)
par(mfrow=c(2,2))
plot(lm2b, col=as.numeric(data$Enzyme)+1, pch=19)
#dev.off()

# Residuals
par(mfrow=c(1,1))
plot(lm2b$residuals~data$EnzymeConc, col=as.numeric(data$Enzyme), pch=19)

# Checking BoxCox
par(mfrow=c(1,1))
bc2 <- boxCox(lm2b, lambda = seq(0, 2, by = 0.05))
lam <- bc2$x[which.max(bc2$y)]

# Transforming the enzyme concentration
data$EnzymeConc <- sqrt(data$EnzymeConc)


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

# Hardness doesn't seem to be significant, thus we keep increasing the complexity of the model without it by adding enzyme concentration

lm3e <- lm(data$Response~data$EnzymeConc)
Anova(lm3e)

lm3f <- lm(data$Response~data$DetStock+data$EnzymeConc)
Anova(lm3f)

lm3g <- lm(data$Response~data$DetStock*data$EnzymeConc)
Anova(lm3g)
step(lm3g)

# Interaction between detergent and enzyme concentration doesn't seem to be significant, add enzyme type

lm3h <- lm(data$Response~data$DetStock+data$Enzyme+data$EnzymeConc)
Anova(lm3h)

lm3i <- lm(data$Response~(data$DetStock+data$Enzyme)*data$EnzymeConc)
Anova(lm3i)

lm3j <- lm(data$Response~data$DetStock*(data$Enzyme+data$EnzymeConc))
Anova(lm3j)

drop1(lm3i, test="F")
drop1(lm3j, test="F")

lm3i <- update(lm3i, ~.-data$DetStock:data$EnzymeConc)
Anova(lm3i)

lm3j <- update(lm3j, ~.-data$DetStock:data$EnzymeConc)
Anova(lm3j)

BIC(lm3i, lm3j)
AIC(lm3i, lm3j)

# lm3i is better

# Full interactions without hardness
lm3k <- lm(data$Response~data$DetStock*data$Enzyme*data$EnzymeConc)
step(lm3k, test="F")

lm3l <- lm(formula = data$Response ~ data$DetStock + data$Enzyme + data$EnzymeConc + data$DetStock:data$Enzyme + data$Enzyme:data$EnzymeConc)
Anova(lm3l)

lm3m <- lm(data$Response~data$Cycle)
Anova(lm3m)

anova(lm3l, lm3i)
#p value is less than 0.05 so model lm3l is selected

# Testing the model -------------------------------------------------------

# Diagnostics
Anova(lm3l)
summary(lm3l)
#png(filename="LinearModel_Transformed.png", width=750, height=750)
par(mfrow=c(2,2))
plot(lm3l, col=as.numeric(data$Enzyme)+1, pch=19)
#dev.off()

# Residuals
par(mfrow=c(1,1))
plot(lm3l$residuals~data$EnzymeConc, col=as.numeric(data$DetStock), pch=19)

# Testing BoxCox
par(mfrow=c(1,1))
bc3 <-boxCox(lm3l, lambda = seq(0, 2, by = 0.05))
lam <- bc3$x[which.max(bc3$y)]

par(mfrow=c(1,1))
qqPlot(lm3l)

# Should we remove the outliers?
data <- data[-c(147,160),]
lm3l <- lm(formula = data$Response ~ data$DetStock + data$Enzyme + data$EnzymeConc + data$DetStock:data$Enzyme + data$Enzyme:data$EnzymeConc)
Anova(lm3l)
summary(lm3l)

par(mfrow=c(1,1))
qqPlot(lm3l$residuals)
shapiro.test(lm3l$residuals)
# Residuals are normally distributed since p-value > 0.05

#png(filename="LinearModel_Transformed.png",width=1750, height=1750, res=300)
par(mfrow=c(2,2))
plot(lm3l, col=as.numeric(as.factor(data$EnzymeConc))+1, pch=19)
#dev.off()


# Confidence Interval -----------------------------------------------------

lm3l <- lm(formula = Response ~ DetStock + Enzyme + EnzymeConc + DetStock:Enzyme + Enzyme:EnzymeConc, data=data)

# New x-data
new_data <- sqrt(seq(0, 15, length.out = 100))
new_data_grid <- expand.grid(EnzymeConc = new_data, Enzyme = levels(data$Enzyme), DetStock = levels(data$DetStock))

# Predictor plots
# Det0
par(mfrow=c(1,1))
det0 <- data[data$DetStock == 'Det0',]
new <- (det0$EnzymeConc)^2
plot((det0$Response^(1/lam1))~new, col= det0$Enzyme, pch=19, main="Det0", xlab="sqrt Enzyme Concentration", ylab="Response (BoxCox transformed)")

for (Enzyme in c(1,2,3,4,5)) {
  x0 <- new_data_grid[new_data_grid$Enzyme==levels(new_data_grid$Enzyme)[Enzyme]&new_data_grid$DetStock=="Det0",]
  
  pred0 <- predict(lm3l,
                   newdata = x0,
                   interval = "prediction")
  matlines (x0$EnzymeConc^2, pred0^(1/lam1), lty = c(1,2,2), lw = 1, col = Enzyme)
}
legend("topleft", legend=levels(data$Enzyme), col=1:nlevels(data$Enzyme), 
       title="Enzyme", pch = 19, cex = 0.8)

# Predictor plots
# Det+
par(mfrow=c(1,1))
detplus <- data[data$DetStock == 'Det+',]
new <- (detplus$EnzymeConc)^2
plot((detplus$Response^(1/lam1))~new, col= detplus$Enzyme, pch=19, main="Det+", xlab="sqrt Enzyme Concentration", ylab="Response (BoxCox transformed)")

for (Enzyme in c(1,2,3,4,5)) {
  x0 <- new_data_grid[new_data_grid$Enzyme==levels(new_data_grid$Enzyme)[Enzyme]&new_data_grid$DetStock=="Det+",]
  
  pred0 <- predict(lm3l,
                   newdata = x0,
                   interval = "prediction")
  matlines (x0$EnzymeConc^2, pred0^(1/lam1), lty = c(1,2,2), lw = 1, col = Enzyme)
}
legend("topleft", legend=levels(data$Enzyme), col=1:nlevels(data$Enzyme), 
       title="Enzyme", pch = 19, cex = 0.8)



# Adding time -------------------------------------------------------------

data <- read.table("~/Github/02441_Applied_Statistics/Case1/2_Data/SPR.txt", header = TRUE, sep="\t")
data <- data[data$EnzymeConc==0,]
data <- data[,-c(4,5)]
data$RunDate <- as.factor(data$RunDate)

# We can't just add run time because it's 1-to-1 with enzyme type

pairs(data, col=as.numeric(data$RunDate), pch=19)

# Maximal model
lm4a <- lm(data$Response~data$RunDate*data$Cycle*data$DetStock)
Anova(lm4a)

# Diagnostics
par(mfrow=c(2,2))
plot(lm4a, pch=19)

# BoxCox
par(mfrow=c(1,1))
bc4 <- boxCox(lm4a, lambda = seq(0, 1, by = 0.05))
lam <- bc4$x[which.max(bc4$y)]

# Transforming the response
data$Response <- data$Response^lam

# Adding RunDate
lm4b <- lm(data$Response~data$DetStock*data$RunDate)
step(lm4b, k=3.8)
drop1(lm4b, test="F")

#lmtest <- lm(data$Response~data$DetStock+data$RunDate+data$CaStock)
#Anova(lmtest)

lm4b <- update(lm4b, ~.-data$DetStock:data$RunDate)
Anova(lm4b)

par(mfrow=c(2,2))
plot(lm4b, pch=19, col=as.numeric(data$RunDate))

par(mfrow=c(1,1))
plot(lm4b$residuals, pch=19, col=as.numeric(data$RunDate))

shapiro.test(lm4b$residuals)
qqPlot(lm4b$residuals)
kruskal.test(data$Response ~ data$RunDate)