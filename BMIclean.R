source("file:///C:/Users/hhen0001/Documents/Stability_BMI_56Lakes/Scripts/SpUseDD.R")
library("reshape")

Full <- read.csv("file:///C:/Users/hhen0001/Documents/Stability_BMI_56Lakes/Data Files/BiologicalData/BiologicalData_BMI_CSV_woCommas.csv", sep=",", header=TRUE)
Full$Year <- as.factor(Full$Provtagningsår)
Full$Month <- as.factor(Full$Provtagningsmånad)
Full$Lake <- Full$Stationsnamn
Full2 <- Full[Full$Month=="10" | Full$Month=="9"| Full$Month=="11"| Full$Month=="8",]
Full3 <- Full2[Full2$Vattenzon.P.L.SP.=="Litoral",]
TM <- cast(Full3, Lake + Year + Stationskoordinat.N.X ~ Taxonnamn, value="Medelantal.per.prov", fun.aggregate=mean)
TM2 <- as.data.frame(TM)
##above is forming data into species matrix for further analysis

SY <- table(TM2$Stationskoordinat.N.X, TM2$Year)
SY2 <- as.data.frame.matrix(SY)
SY3 <- SY2[apply(SY2, MARGIN = 1, function(x) all(x > 0, x < 3)), ] ##No lakes sampled in all years because of 2016
SY4 <- SY2[rowSums(SY2 > 0) >= 21,] ##56 lakes sampled in 1995-2015
##above to figure out which lakes were sampled in all years

AYsmp <- as.factor(rownames(SY4))
AYSmp2 <- TM2[TM2$Stationskoordinat.N.X %in% AYsmp,]
AYSmp2$Lake <- factor(AYSmp2$Lake)
SpUse <- append(SpUse, c("Lake", "Year", "Stationskoordinat.N.X"))
AYSmp3 <- AYSmp2[,which(names(AYSmp2) %in% SpUse)]
AYSmp3[is.na(AYSmp3)] <- 0
rownames(AYSmp3) <- paste(AYSmp3$Lake, AYSmp3$Year)
##above to subset by lakes sampled in all years and taxa screened by David

AYSmp4 <- AYSmp3[,c(TRUE, TRUE, TRUE, colSums(AYSmp3[,4:length(AYSmp3)]) > 10)]
AYSmp5 <- AYSmp3[,c(TRUE, TRUE, TRUE, colSums(AYSmp3[,4:length(AYSmp3)] == 0) < 1117)]
AYSmp6 <- AYSmp5[rowSums(AYSmp5[,4:length(AYSmp5)]) > 0,]
AYSmp6 <- droplevels(AYSmp6)
