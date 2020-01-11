#########################################################################
## Case 1:                                                             ##
## Effect of hardness and detergent on enzymatic catalysis             ##
#########################################################################

# Authors: Begoña Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu

require("car")
require("ggplot2")

# Load data and clean
data <- read.table("~/Github/02441_Applied_Statistics/Case1/2_Data/SPR.txt", header = TRUE, sep="\t")
data <- data[,-c(1,2)]

# Alternative: copy dataframe and merge detergent with hardness
df <- data
df$Stock <- as.factor(paste(as.character(df$DetStock),as.character(df$CaStock)))
df <- df[,-c(4,5)]

# Summary Statistics ------------------------------------------------------

# Structure and summary of both data frames
str(data)
summary(data)
str(df)
summary(df)


# Data Visualization ------------------------------------------------------

# Data visualization of summary statistics of both data frames
pairs(data, col=as.numeric(data$Enzyme)+1, pch=19)
par(mfrow=c(1,2))
plot(data$Response~data$DetStock)
plot(data$Response~data$CaStock)

pairs(df, col=as.numeric(df$Enzyme)+1, pch=19)
par(mfrow=c(1,1))
plot(df$Response~df$Stock)


# Transformations ---------------------------------------------------------

par(mfrow=c(1,1))
y <- sort(unique(data$EnzymeConc))
x <- 0:3
plot(y~x)
lm <- lm(y~x)
lines(x,predict(lm))
summary(lm)


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

# Add enzyme concentration
lm1g <- lm(data$Response~data$DetStock+data$Enzyme+data$EnzymeConc)
Anova(lm1g)

lm1h <- lm(data$Response~(data$DetStock+data$Enzyme)*data$EnzymeConc)
Anova(lm1h)

lm1i <- lm(data$Response~data$DetStock*(data$Enzyme+data$EnzymeConc))
Anova(lm1i)

drop1(lm1h, test="F")
drop1(lm1i, test="F")
BIC(lm1h, lm1i) # We prefer lm1h since BIC is better
AIC(lm1h, lm1i) # We prefer lm1h since AIC is better

# Full interactions without hardness
lm1j <- lm(data$Response~data$DetStock*data$Enzyme*data$EnzymeConc)
Anova(lm1j)

drop1(lm1j, test="F")

lm1k <- update(lm1j, ~.-data$DetStock:data$Enzyme:data$EnzymeConc)
Anova(lm1k)

drop1(lm1k, test="F")
step(lm1j,k=2) # We get the same model by backward selection, thus lm1k is our selected model for the complete dataframe

# Full interactions with hardness
lm1l <- lm(data$Response~data$Enzyme*data$EnzymeConc*data$DetStock*data$CaStock)
step(lm1l)

AIC(lm1a, lm1b, lm1c, lm1d, lm1e, lm1f, lm1g, lm1h, lm1i, lm1k)
BIC(lm1a, lm1b, lm1c, lm1d, lm1e, lm1f, lm1g, lm1h, lm1i, lm1k)

# Model Selection (2) -----------------------------------------------------

# Testing response given stock
lm2a <- lm(df$Response~df$Stock, df)
Anova(lm2a)

lm2b <- lm(df$Response~df$Enzyme, df)
Anova(lm2b)

# Testing response given stock and enzyme
lm2c <- lm(df$Response~df$Enzyme+df$Stock, df)
Anova(lm2c)

lm2d <- lm(df$Response~df$Enzyme*df$Stock, df)
Anova(lm2d)
step(lm2d)

# Testing response given stock, enzyme and enzyme concentration (with interactions)
lm2e <- lm(df$Response~df$Enzyme+df$Stock+df$EnzymeConc, df)
Anova(lm2e)

lm2f <- lm(df$Response~(df$Enzyme+df$Stock)*df$EnzymeConc, df)
Anova(lm2f)

lm2g <- lm(df$Response~df$Enzyme*(df$Stock+df$EnzymeConc), df)
Anova(lm2g)

drop1(lm2f, test="F")
drop1(lm2g, test="F")
BIC(lm2f, lm2g) # We prefer lm2f since BIC is better
AIC(lm2f, lm2g) # We prefer lm2f since AIC is better

lm2h <- lm(df$Response~df$Enzyme*df$Stock*df$EnzymeConc, df)
Anova(lm2h)

drop1(lm2h, test="F")

lm2i <- update(lm2h, ~.-df$Enzyme:df$Stock:df$EnzymeConc)
Anova(lm2i)
drop1(lm2i, test="F")

step(lm2h,k=2) # We get the same model by backward selection, thus lm2i is our selected model for the complete dataframe

AIC(lm1a, lm1b, lm1c, lm1d, lm1e, lm1f, lm1g, lm1h, lm1i, lm1k, lm2a, lm2b, lm2c, lm2d, lm2e, lm2f, lm2g, lm2h, lm2i)
BIC(lm1a, lm1b, lm1c, lm1d, lm1e, lm1f, lm1g, lm1h, lm1i, lm1k, lm2a, lm2b, lm2c, lm2d, lm2e, lm2f, lm2g, lm2h, lm2i)

AIC(lm1k, lm2i)
BIC(lm1k, lm2i)

# It seems that lm1k is better (without the hardness of the water)

# Testing the model -------------------------------------------------------

Anova(lm1k)
summary(lm1k, correlation=TRUE)
par(mfrow=c(2,2))
plot(lm1k, col=as.numeric(data$Enzyme), pch=19)