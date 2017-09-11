BI4$Lake <- droplevels(BI4$Lake)
BI4$Lake2 <- as.numeric(BI4$Lake)

list <- c(1:56)
varDCChem <- rep(NA, 56)
varDCChem <- matrix(NA, 56, 8)

for (i in 1:length(list)) {
  varDC1 <- var(BI4[BI4$Lake2==i,][,1])
  varDCChem[i,1] <- varDC1
  varDC2 <- var(BI4[BI4$Lake2==i,][,2])
  varDCChem[i,2] <- varDC2
  varchl <- var(BI4[BI4$Lake2==i,][,21])
  varDCChem[i,3] <- varchl
  varvd <- var(BI4[BI4$Lake2==i,][,22])
  varDCChem[i,4] <- varvd
  varwt <- var(BI4[BI4$Lake2==i,][,23])
  varDCChem[i,5] <- varwt
  varph <- var(BI4[BI4$Lake2==i,][,17])
  varDCChem[i,6] <- varph
  vartoc <- var(BI4[BI4$Lake2==i,][,19])
  varDCChem[i,7] <- vartoc
  vartp <- var(BI4[BI4$Lake2==i,][,20])
  varDCChem[i,8] <- vartp
  }

colnames(varDCChem) <- c("DCA1", "DCA2", "Chl", "VD", "WT", "pH", "TOC", "TP")
varDCChem <- data.frame(varDCChem)

lm <- lm(DCA2 ~ pH + TP + TOC + Chl + VD + WT, data=varDCChem)

endo.pcnm <- pcnm(dist(CP5[,3:4]))
cap.env <- capscale(AYSmp3[,4:length(AYSmp3)] ~., data=CP6, distance='bray')
cap.pcnm <- capscale(AYSmp3[,4:length(AYSmp3)] ~ ., data=as.data.frame(scores(endo.pcnm)), distance='bray')
mod0.env <- capscale(AYSmp3[,4:length(AYSmp3)] ~ 1, data=CP6, distance='bray')
mod0.pcnm <- capscale(AYSmp3[,4:length(AYSmp3)] ~ 1, data=as.data.frame(scores(endo.pcnm)), distance='bray')
step.env <- ordistep(mod0.env, scope=formula(cap.env))
step.pcnm <- ordistep(mod0.pcnm, scope=formula(cap.pcnm), direction="forward")
endo.pcnm.sub <- scores(endo.pcnm, choices=c(1:10))

par(mfrow=c(3, 2))
ordisurf(CP5[,3:4],scores(endo.pcnm,choi=1),bubble=4,
         main='PCNM 1')
ordisurf(CP5[,3:4],scores(endo.pcnm,choi=2),bubble=4,
         main='PCNM 2')
ordisurf(CP5[,3:4],scores(endo.pcnm,choi=3),bubble=4,
         main='PCNM 3')
ordisurf(CP5[,3:4],scores(endo.pcnm,choi=4),bubble=4,
         main='PCNM 4')
ordisurf(CP5[,3:4],scores(endo.pcnm,choi=5),bubble=4,
         main='PCNM 5')
ordisurf(CP5[,3:4],scores(endo.pcnm,choi=6),bubble=4,
         main='PCNM 6')

mod <- varpart(AYSmp3[,4:length(AYSmp3)], ~., yr, endo.pcnm.sub, data=CP6, transfo="hellinger")
plot(mod, bg=1:3, text='')
mtext("Variance partitioning")
text(1.8, 0.6, "Temporal")
text(-0.9, 0.6, "Environmental")
text(-0.15, -1.5, "Spatial")

#~pH2, ~TOC2, ~TotalP2, ~Chl2, ~VD2, WT2 
