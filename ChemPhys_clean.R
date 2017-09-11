library("plyr")
library("dplyr")
library("reshape")
CP <- read.csv("file:///C:/Users/hhen0001/Documents/Stability_BMI_56Lakes/Data Files/PhysChemData/PhysChem_CSVwoCommas.csv", sep=",", header=TRUE)
CP$Year <- as.factor(CP$Provtagningsår)
CP$Month <- as.factor(CP$Provtagningsmånad)
CP$Lake <- CP$Stationsnamn
CP2 <- CP[CP$Year=="1995" | CP$Year=="1996" | CP$Year=="1997" |CP$Year=="1998" |CP$Year=="1999" |CP$Year=="2000" |CP$Year=="2001" |CP$Year=="2002" |CP$Year=="2003" |CP$Year=="2004" |CP$Year=="2005" |CP$Year=="2006" |CP$Year=="2007" |CP$Year=="2008" |CP$Year=="2009" |CP$Year=="2010" |CP$Year=="2011" |CP$Year=="2012" |CP$Year=="2013" |CP$Year=="2014" |CP$Year=="2015",]
CP3 <- CP2[CP2$Month=="8" | CP2$Month=="10" | CP2$Month=="9"| CP2$Month=="11",]
##mean values for months 8,9,10,11 and when there are multiple months represented
CP4<- CP3[CP3$Stationskoordinat.N.X %in% AYSmp3$Stationskoordinat.N.X,]
CP4$Measurements <- as.numeric(levels(CP4$Värde.Koncentration))[CP4$Värde.Koncentration]
CP5 <- cast(CP4, Lake + Year + Stationskoordinat.N.X + Stationskoordinat.E.Y ~ Parameter, value="Measurements", fun.aggregate=mean)
colnames(CP5) <- c("Lake", "Year", "Latitude", "Longitude", "Chlorophyll", "pH", "VisibilityDepth", "Sulfate", "TOC", "TotalP", "WaterTemp.")

cor(CP5[,3:11], use = "complete.obs")

CP6 <- CP5[,c(5:7,9:11)]
CP6$Chlorophyll<- c(3.2, na.approx(CP5$Chlorophyll))
CP6$VisibilityDepth<- na.approx(CP5$VisibilityDepth)
CP6$WaterTemp.<- na.approx(CP5$WaterTemp.)

yr <- as.numeric(CP5[,2])
#Means for chem/phys measurements across months and years per lake
#meanpH <-tapply(CP5$pH, CP5$Lake, mean, na.rm=TRUE)
#meanpH <- data.frame(meanpH[!is.na(meanpH)])
#meanpH$Lake <- rownames(meanpH)

#meanVD <-tapply(CP5$Siktdjup, CP5$Lake, mean, na.rm=TRUE)
#meanVD <- data.frame(meanVD[!is.na(meanVD)])
#meanVD$Lake <- rownames(meanVD)

#meanTOC <-tapply(CP5$TOC, CP5$Lake, mean, na.rm=TRUE)
#meanTOC <- data.frame(meanTOC[!is.na(meanTOC)])
#meanTOC$Lake <- rownames(meanTOC)

#meanTP <-tapply(CP5$'Tot-P', CP5$Lake, mean, na.rm=TRUE)
#meanTP <- data.frame(meanTP[!is.na(meanTP)])
#meanTP$Lake <- rownames(meanTP)

#meanT <-tapply(CP5$Vattentemperatur, CP5$Lake, mean, na.rm=TRUE)
#meanT <- data.frame(meanT[!is.na(meanT)])
#meanT$Lake <- rownames(meanT)

#meanCHLA <-tapply(CP5$Kfyll, CP5$Lake, mean, na.rm=TRUE)
#meanCHLA <- data.frame(meanCHLA[!is.na(meanCHLA)])
#meanCHLA$Lake <- rownames(meanCHLA)

#meanSO4 <-tapply(CP5$SO4, CP5$Lake, mean, na.rm=TRUE)
#meanSO4 <- data.frame(meanSO4[!is.na(meanSO4)])
#meanSO4$Lake <- rownames(meanSO4)

