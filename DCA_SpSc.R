source("file:///C:/Users/hhen0001/Documents/Stability_BMI_56Lakes/Scripts/BMIclean.R")
source("file:///C:/Users/hhen0001/Documents/Stability_BMI_56Lakes/Scripts/ChemPhys_clean.R")
source("file:///C:/Users/hhen0001/Documents/Stability_BMI_56Lakes/Scripts/BMI_DCA_Movements.R")
library("vegan")
library("lme4")
library("zoo")
library("arm")
library("labdsv")
library("plyr")
library("dplyr")
library("reshape")
#BI3$LatNum <- as.numeric(BI3$Latitude)
#lm1 <- lmer(BI3$DCA2~BI3$pH+BI3$LatNum+BI3$TOC+BI3$TotalP+(1|BI3$Lake)+(1|BI3$Year))

DCA <- decorana(AYSmp3[,4:length(AYSmp3)], iweigh=1)
envfit2 <- envfit(DCA, BI4[,c(7,10,11,13,14,15,16)], permutations = 999)
SpSc <- as.data.frame(scores(decorana(AYSmp4[,4:length(AYSmp4)]), choices=c(1,2), display="sites"))
SpScDCA1 <- SpSc[order(-abs(SpSc[,1])),]
SpScDCA2 <- SpSc[order(-abs(SpSc[,2])),]

#RDABMIPC <- rda(AYSmp3[,4:length(AYSmp4)] ~ CP5$Latitude + CP5$pH + CP5$TOC + CP5$TotalP + CP5$WaterTemp. + CP5$VisibilityDepth, na.action = na.exclude)
CCABMIPC <- cca(AYSmp4[,4:ncol(AYSmp4)] ~ CP5$Latitude + CP5$pH + CP5$TOC + CP5$TotalP + CP5$WaterTemp. + CP5$VisibilityDepth, na.action = na.exclude)
CCASscores <-  scores(cca(AYSmp4[,4:ncol(AYSmp4)], choices=c(1,2), display="sites"))

#CCAscores <- as.data.frame(scores(cca(AYSmp4[,4:length(AYSmp4)] ~ CP5$Latitude + CP5$pH + CP5$TOC + CP5$TotalP + CP5$WaterTemp. + CP5$VisibilityDepth, na.action = na.exclude), choices=c(1,2), display="species"))
#SpScCCA <- CCAscores[order(-abs(CCAscores[,1])),]

NMDS <- metaMDS(AYSmp4[,4:length(AYSmp4)], k=2, maxit=100, sratmax=0.999999, noshare=0.1)
NMDSscores <- as.data.frame(scores(NMDS, choices=c(1,2), display="sites"))
BI4 <- cbind(NMDSscores, CP5)
BI4$Chlorophyll2<- c(3.2, na.approx(BI4$Chlorophyll))
BI4$VisibilityDepth2<- na.approx(BI4$VisibilityDepth)
BI4$WaterTemp2<- na.approx(BI4$WaterTemp.)
BI4$Latitude <- as.numeric(BI4$Latitude)
BI4$pH2 <- scale(BI4$pH)
BI4$Lat2 <- scale(BI4$Latitude)
BI4$TOC2 <- scale(BI4$TOC)
BI4$TotalP2 <- scale(BI4$TotalP)
BI4$Chl2 <- scale(BI4$Chlorophyll2)
BI4$VD2 <- scale(BI4$VisibilityDepth2)
BI4$WT2 <- scale(BI4$WaterTemp2)
cor(BI4[,16:22])
fit <- envfit(NMDS, BI4[,18:24])

lm2 <- lmer(NMDS2 ~ pH2 + Lat2 + TotalP2 + TOC2 + Chl2 + VD2 + WT2 + (1|Lake) + (1|Year), data=BI4)
slm2 <- sim(lm2, 1000)
fixef <- slm2@fixef
fixefmean <- apply(fixef, 2, mean)
fixefQ1 <- apply(fixef, 2, quantile, 0.025)
fixefQ2 <- apply(fixef, 2, quantile, 0.975)
CIs <- cbind(fixefmean, fixefQ1, fixefQ2)
CIs <- as.data.frame(CIs)
CIs$Variable <- c("Intercept","pH", "Latitude", "Total P", "TOC", "Chl", "VD", "Temp.")
CI2 <- CIs[2:8, 1:4]
colnames(CI2) <- c("Mean", "Lower CI", "Upper CI", "Env. Var.")

par(mar=c(3,6,3,2))
plot(CI2$Mean, 1:7, xlim=c(-0.35,0.3), yaxt="n", ylab='', pch=19, xlab='', main="95% Credible Intervals")
segments(CI2$`Lower CI`, 1:7, CI2$`Upper CI`, 1:7, lwd=2.5)
abline(v=0, lty=2)
axis(2, 1:7, CI2$`Env. Var.`, las=2)

plot(NMDS, display="species", type="text")
plot(envfit2)