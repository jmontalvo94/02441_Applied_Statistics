#########################################################################
## Case 1:                                                             ##
## Effect of hardness and detergent on enzymatic catalysis             ##
#########################################################################

# Authors: Begoña Bolos Sierra, Laura Sans Comerma, Jorge Montalvo Arvizu


# Load data and clean
df <- read.table("~/Github/02441_Applied_Statistics/Case1/2_Data/SPR.txt", header = TRUE, sep="\t")
df <- df[,-c(1,2)]
df$Stock <- as.factor(paste(as.character(df$DetStock),as.character(df$CaStock)))
df <- df[,-c(4,5)]

# Summary statistics
str(df)
