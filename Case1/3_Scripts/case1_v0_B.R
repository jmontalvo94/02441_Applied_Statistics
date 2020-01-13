#########################################################################
## Case 1:                                                             ##
## Effect of hardness and detergent on enzymatic catalysis             ##
#########################################################################

# Authors: Begoña Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load data
df <- read.table("~/Github/02441_Applied_Statistics/Case1/2_Data/SPR.txt", header = TRUE, sep="\t")
df1 <- df

#add one colum for Det and CaS and eliminate the columns that are not interesting
df1$Stock <- paste(df1$DetStock, df1$CaStock)
df1 <- df1[,-c(1,2, 6,7)]
df1
pairs(df1)

#How does hardness & detergent influence the catalytic activity?
#Factor the Stock
df1$Stock <- factor(df1$Stock)
plot(Response ~ Stock, df1)
lm1 <- lm(Response ~ Stock, df1)
library(car)
Anova(lm1) #the stock has a significant impact in the response.
#The catalitic activity is higher when the detergent is present. 
summary(lm1)

#Is the catalytic activity dependent on the amount of enzyme present?
boxplot(Response ~ EnzymeConc + Enzyme, df1)#laura's plot
lm2 <- lm(Response ~ EnzymeConc + Stock, df1)
Anova(lm2) #the EnzymeConc has a signicant impact in the response
#The catalitic response increses when the concentration is higher.
summary(lm2)

#Are there any differences in performance among the enzymes in this study regarding the
#factors mentioned above?
lm3 <- lm(Response ~ EnzymeConc + Stock + Enzyme, df1)
Anova(lm3) #the type of Enzyme has impact in the response
#check for interactions
lm4 <- lm(Response ~ EnzymeConc*Stock*Enzyme, df1)
Anova(lm4)
par(mfrow=c(2,2))
plot(lm4)
summary(lm4)

lm5 <- lm(Response ~ EnzymeConc*(Stock+Enzyme), df1)
Anova(lm5)
par(mfrow=c(2,2))
plot(lm5)
summary(lm5)
lm6 <- lm(Response ~ EnzymeConc*(Stock+Enzyme) + Stock:Enzyme, df1)
Anova(lm6)
par(mfrow=c(2,2))
plot(lm6)
summary(lm6)

AIC(lm3, lm5, lm6)
