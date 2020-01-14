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
data <- data[,-c(1,2)]

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

lm1e <- lm(data$Response~data$EnzymeConc)
Anova(lm1e)

lm1f <- lm(data$Response~data$DetStock+data$EnzymeConc)
Anova(lm1f)

lm1g <- lm(data$Response~data$DetStock*data$EnzymeConc)
Anova(lm1g)
step(lm1g)

# Interaction between detergent and enzyme doesn't seem to be significant, add enzyme concentration
lm1h <- lm(data$Response~data$DetStock+data$Enzyme+data$EnzymeConc)
Anova(lm1h)

lm1i <- lm(data$Response~(data$DetStock+data$Enzyme)*data$EnzymeConc)
Anova(lm1i)

lm1j <- lm(data$Response~data$DetStock*(data$Enzyme+data$EnzymeConc))
Anova(lm1j)

drop1(lm1i, test="F")
drop1(lm1j, test="F")

BIC(lm1i, lm1j)
AIC(lm1i, lm1j)

# Full interactions without hardness
lm1k <- lm(data$Response~data$DetStock*data$Enzyme*data$EnzymeConc)
Anova(lm1k)

drop1(lm1k, test="F")

lm1l <- update(lm1k, ~.-data$DetStock:data$Enzyme:data$EnzymeConc)
Anova(lm1l)

drop1(lm1l, test="F")

step(lm1k,k=3.8) # We get the same model by backward selection, thus lm1k is our selected model for the complete dataframe

# Full interactions with hardness
lm1m <- lm(data$Response~data$Enzyme*data$EnzymeConc*data$DetStock*data$CaStock)
step(lm1m, k=2)

AIC(lm1a, lm1b, lm1c, lm1d, lm1e, lm1f, lm1g, lm1h, lm1i, lm1k, lm1l)
BIC(lm1a, lm1b, lm1c, lm1d, lm1e, lm1f, lm1g, lm1h, lm1i, lm1k, lm1l)

anova(lm1l, lm1i)
# P-value is less than 0.05, the fit is significantly improved for the complex model, hence, we keep the complex model lm1l


# Testing the model (1) -----------------------------------------------------

Anova(lm1l)
summary(lm1l)
#png(filename="LinearModel_Transformed.png", width=750, height=750)
par(mfrow=c(2,2))
plot(lm1l, col=as.numeric(data$EnzymeConc)+1, pch=19)
#dev.off()

par(mfrow=c(1,1))
plot(lm1l$residuals~data$EnzymeConc, col=as.numeric(data$DetStock)+1, pch=19)

par(mfrow=c(1,1))
bc <-boxCox(lm1l, lambda = seq(0, 1, by = 0.05))
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

lm2e <- lm(data$Response~data$EnzymeConc)
Anova(lm2e)

lm2f <- lm(data$Response~data$DetStock+data$EnzymeConc)
Anova(lm2f)

lm2g <- lm(data$Response~data$DetStock*data$EnzymeConc)
Anova(lm2g)
step(lm2g)

# Interaction between detergent and enzyme concentration doesn't seem to be significant, add enzyme
lm2h <- lm(data$Response~data$DetStock+data$Enzyme+data$EnzymeConc)
Anova(lm2h)

lm2i <- lm(data$Response~(data$DetStock+data$Enzyme)*data$EnzymeConc)
Anova(lm2i)

lm2j <- lm(data$Response~data$DetStock*(data$Enzyme+data$EnzymeConc))
Anova(lm2j)

drop1(lm2i, test="F")
drop1(lm2j, test="F")

lm2i <- update(lm2i, ~.-data$DetStock:data$EnzymeConc)
Anova(lm2i)

lm2j <- update(lm2j, ~.-data$DetStock:data$EnzymeConc)
Anova(lm2j)

drop1(lm2j, test="F")

lm2j <- update(lm2j, ~.-data$DetStock:data$Enzyme)
Anova(lm2j)

drop1(lm2j, test="F")

BIC(lm2i, lm2j)
AIC(lm2i, lm2j)

# We prefer lm2i

# Full interactions without hardness
lm2k <- lm(data$Response~data$DetStock*data$Enzyme*data$EnzymeConc)
Anova(lm2k)

drop1(lm2k, test="F")

lm2l <- update(lm2k, ~.-data$DetStock:data$Enzyme:data$EnzymeConc)
Anova(lm2l)

drop1(lm2l, test="F")

lm2l <- update(lm2l, ~.-data$DetStock:data$EnzymeConc)
Anova(lm2l)

drop1(lm2l, test="F")

lm2l <- update(lm2l, ~.-data$DetStock:data$Enzyme)
Anova(lm2l)

step(lm2k,k=3.8) # We get the same model by backward selection, thus lm1k is our selected model for the complete dataframe

# Full interactions with hardness
lm2m <- lm(data$Response~data$Enzyme*data$EnzymeConc*data$DetStock*data$CaStock)
step(lm2m, k=3.8)

AIC(lm2a, lm2b, lm2c, lm2d, lm2e, lm2f, lm2g, lm2h, lm2i, lm2k, lm2l)
BIC(lm2a, lm2b, lm2c, lm2d, lm2e, lm2f, lm2g, lm2h, lm2i, lm2k, lm2l)


# Testing the model -------------------------------------------------------

Anova(lm2l)
summary(lm2l)
#png(filename="LinearModel_Transformed.png", width=750, height=750)
par(mfrow=c(2,2))
plot(lm2l, col=as.numeric(data$Enzyme)+1, pch=19)
#dev.off()

par(mfrow=c(1,1))
plot(lm2l$residuals~data$EnzymeConc, col=as.numeric(data$Enzyme), pch=19)

#data <- data[-14,]

par(mfrow=c(1,1))
bc2 <-boxCox(lm2l, lambda = seq(0, 2, by = 0.05))
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
Anova(lm3k)

drop1(lm3k, test="F")

lm3l <- update(lm3k, ~.-data$DetStock:data$Enzyme:data$EnzymeConc)
Anova(lm3l)

drop1(lm3l, test="F")

lm3l <- update(lm3l, ~.-data$DetStock:data$EnzymeConc)
Anova(lm3l)

drop1(lm3l, test="F")

step(lm3k,k=3.8) # We get the same model by backward selection, thus lm1k is our selected model for the complete dataframe

# Full interactions with hardness
lm3m <- lm(data$Response~data$Enzyme*data$EnzymeConc*data$DetStock*data$CaStock)
step(lm3m, k=3.8)

AIC(lm3a, lm3b, lm3c, lm3d, lm3e, lm3f, lm3g, lm3h, lm3i, lm3k, lm3l)
BIC(lm3a, lm3b, lm3c, lm3d, lm3e, lm3f, lm3g, lm3h, lm3i, lm3k, lm3l)

AIC(lm3i, lm3l)
BIC(lm3i, lm3l)


# Testing the model -------------------------------------------------------

Anova(lm3i)
summary(lm3i)
#png(filename="LinearModel_Transformed.png", width=750, height=750)
par(mfrow=c(2,2))
plot(lm3i, col=as.numeric(data$Enzyme)+1, pch=19)
#dev.off()

par(mfrow=c(1,1))
plot(lm3i$residuals~data$EnzymeConc, col=as.numeric(data$DetStock), pch=19)

# compare residuals between log and sqrt

par(mfrow=c(1,1))
bc3 <-boxCox(lm3i, lambda = seq(0, 2, by = 0.05))
lam <- bc3$x[which.max(bc3$y)]

par(mfrow=c(1,1))
qqPlot(lm3i)

# Should we remove the outliers?
# data <- data[-147,]
# data <- data[-159,]
data <- data[-c(147,160),]
lm3i <- lm(data$Response~(data$DetStock+data$Enzyme)*data$EnzymeConc)
lm3i <- update(lm3i, ~.-data$DetStock:data$EnzymeConc)

par(mfrow=c(1,1))
qqPlot(lm3i$residuals)
shapiro.test(lm3i$residuals)
# Residuals are normally distributed since p-value > 0.05

#png(filename="LinearModel_Transformed.png",width=1750, height=1750, res=300)
par(mfrow=c(2,2))
plot(lm3i, col=as.numeric(as.factor(data$EnzymeConc))+1, pch=19)
#dev.off()

##################
#this is the model
lm3i <- lm(data$Response~(data$DetStock+data$Enzyme)*data$EnzymeConc-data$DetStock:data$EnzymeConc)
#new_data <- seq(min(data$EnzymeConc), max(data$EnzymeConc), length.out = 100)
new_data <- seq(0, 15, length.out = 100)
new_data_grid <- expand.grid(EnzymeConc = new_data, Enzyme = levels(data$Enzyme), DetStock = levels(data$DetStock))
color_codes <- as.character(levels(data$Enzyme))

#predictor plots
#Det0
par(mfrow=c(1,1))
det0 <- data[data$DetStock == 'Det0',]
plot(Response~EnzymeConc, det0, col= det0$Enzyme, pch=19) # cambiad variable enzyme conc a la raw!!!
legend("topleft", legend = levels(det0$Enzyme), col = 1:nlevels(det0$Enzyme),
       pch = 19, cex = 0.8)

for (Enzyme in c(1,2,3,4,5)) {
  x0 <- new_data_grid$EnzymeConc[new_data_grid$Enzyme==color_codes[Enzyme]&new_data_grid$DetStock=="Det0"]
  
  pred0 <- predict(lm3i,
                   new_data_grid[new_data_grid$Enzyme==color_codes[Enzyme]&new_data_grid$DetStock=="Det0", ],
                   interval = "prediction")
  # la prediction no la hace bien por algo del linear model
  # resultado: para predecir 100 valores, dice que el modelo tiene 158 rows!(?)
  
  matlines (x0, pred0, lty = c(1,2,2), lw = 1, col = 1)
}
legend("bottomright", legend=levels(data$Enzyme), col=1:nlevels(data$Enzyme), 
       title="Enzyme", pch = 19, cex = 0.8)


#predic.x <- expand.grid(levels(factor1), levels(factor2), x.range)
#individual conbinations between factor1 and 2. check forbes example
#enzymeconc is x

# Confidence Interval -----------------------------------------------------


# Adding time -------------------------------------------------------------

data <- read.table("~/Github/02441_Applied_Statistics/Case1/2_Data/SPR.txt", header = TRUE, sep="\t")
data <- data[data$EnzymeConc==0,]
data <- data[,-c(2,4,5,7)]

pairs(data)

lm4a <- lm(data$Response~data$RunDate*data$DetStock)
Anova(lm4a)

drop1(lm4a, test="F")

lm4a <- update(lm4a, ~.-data$RunDate:data$DetStock)
Anova(lm4a)

par(mfrow=c(2,2))
plot(lm4a, pch=19)

par(mfrow=c(1,1))
bc4 <-boxCox(lm4a, lambda = seq(0, 1, by = 0.05))
lam <- bc$x[which.max(bc$y)]

# Transforming the response
#data$Response <- ((data$Response^lam)-1)/lam
data$Response <- data$Response^lam

lm4a <- lm(data$Response~data$RunDate*data$DetStock)
Anova(lm4a)

drop1(lm4a, test="F")

lm4a <- update(lm4a, ~.-data$RunDate:data$DetStock)
Anova(lm4a)

par(mfrow=c(2,2))
plot(lm4a, pch=19)

kruskal.test(data$Response ~ data$RunDate+data$RunDate)
