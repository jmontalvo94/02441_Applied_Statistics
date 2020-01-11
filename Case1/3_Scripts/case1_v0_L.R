#########################################################################
## Case 1:                                                             ##
## Effect of hardness and detergent on enzymatic catalysis             ##
#########################################################################

# Authors: Begoña Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu
library(ggplot2)
library(car)

# Load data
df_raw <- read.table("~/Github/02441_Applied_Statistics/Case1/2_Data/SPR.txt", header = TRUE, sep="\t")
# Concatenate the last two columns in a new df
df <- df_raw[,-c(1,2,6,7)]
df$DetStock_CaStock = paste(df_raw$DetStock,df_raw$CaStock)
cols <- c("black","red", "blue", "green")
col_bg <- adjustcolor(cols, alpha = 0.2) 
cols2 <- c("black","red", "blue", "green",6)
col_bg2 <- adjustcolor(cols2, alpha = 0.2) 
cols3 <- c("black","red")
col_bg3 <- adjustcolor(cols3, alpha = 0.2) 
# Data visualization
# Response- Enzyme
boxplot(Response~Enzyme, data=df, xlab="Enzyme", ylab="protein removal (RU)", 
        col=col_bg2, medcol=cols2, whiskcol=cols2, staplecol=cols2, boxcol=cols2, outcol=cols2, outbg=cols2)
# Response- concentration
boxplot(Response~EnzymeConc, data=df, xlab="Enzyme Concentration (nM)", ylab="protein removal (RU)", 
        col=col_bg, medcol=cols, whiskcol=cols, staplecol=cols, boxcol=cols, outcol=cols,outbg=cols )

# Response - enzyme - concentration
par(mfrow = c(1,1))
b <- boxplot(Response ~  EnzymeConc +Enzyme, data = df, xaxt = "n",
             col= col_bg, medcol=cols, whiskcol=cols, staplecol=cols, boxcol=cols, outcol=cols,outbg=cols, 
             names =c("","","A","","","","B","","","","C","","","","D","","","","E",""))
axis(side= 1, at=seq_along(b$names), tick = FALSE, labels = b$names)
legend("topright",title="Enzyme concentration", legend = c(0, 2.5, 7.5, 15), fill =cols, horiz =TRUE, cex=0.8)

# response  - stock
par(mfrow = c(1,1))
boxplot(Response~DetStock_CaStock , data=df, xlab="Conditions (detergent and Ca2++)", ylab="protein removal (RU)", 
        col=col_bg3, medcol=cols3, whiskcol=cols3, staplecol=cols3, boxcol=cols3, outcol=cols3,outbg=cols3 )

# how hardness and detergent affect the catalytic activity
boxplot(Response~DetStock_CaStock, data=df, xlab="Conditions (detergent and Ca2++)", ylab="protein removal (RU)")
#ANCOVA 
lm1 = lm(Response ~ DetStock_CaStock, df)
Anova(lm1)
par(mfrow = c(2,2))
plot(lm1, which = 1:4)
summary(lm1)

#ANCOVA 2
lm2 = lm(Response ~ DetStock_CaStock + EnzymeConc, df)
Anova(lm2)
par(mfrow = c(2,2))
plot(lm2, which = 1:4)
summary(lm2)

#ANCOVA 3
lm3 = lm(Response ~ DetStock_CaStock + EnzymeConc + Enzyme, df)
Anova(lm3)
par(mfrow = c(2,2))
plot(lm3, which = 1:4)
summary(lm3)

#ANCOVA 4
lm4 = lm(Response ~ DetStock_CaStock*EnzymeConc*Enzyme, df)
Anova(lm4)
par(mfrow = c(2,2))
plot(lm4, which = 1:4)
summary(lm4)

#UPDATE
lm4test <- update(lm4,.~.-DetStock_CaStock:EnzymeConc:Enzyme)
Anova(lm4test)
par(mfrow = c(2,2))
plot(lm4test, which = 1:4)
summary(lm4test)

#ANCOVA 5
lm5 = lm(Response ~ EnzymeConc*(DetStock_CaStock+Enzyme), df)
Anova(lm5)
par(mfrow = c(2,2))
plot(lm5, which = 1:4)
summary(lm5)
