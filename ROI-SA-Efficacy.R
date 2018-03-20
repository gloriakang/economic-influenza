#Catastrophic

ROI.static.cat <- c(1.3696131
                    ,3.7392263
                    ,6.108839
                    ,8.478453
                    ,10.848066
                    ,13.217679)
ROI.dynamic.cat <- c(5.647222
                     ,12.63354
                     ,19.85475
                     ,26.86709
                     ,33.08647
                     ,38.31664)

ROI.total <- rbind(ROI.static.cat,ROI.dynamic.cat)
colnames(ROI.total) <- c("10%","20%","30%","40%","50%","60%")

tiff("Figure9.tiff",width = 7.5, height = 2.5,units = 'in',res=400,family="Arial")
par(mfrow=c(1,3))
barplot(ROI.total,beside=TRUE,ylab="Retrun on investment",xlab="Vaccine efficacy",main="Catastrophic Influenza",col=c(1,2),ylim=c(0,35))
#legend("top",legend=c("Static model","Dynamic model"),pch=15,col=c(1,2),cex=0.8)

#dev.off()

#Strong
ROI.static.str <- c(0.7006146
                    ,2.4012293
                    ,4.101844
                    ,5.802459
                    ,7.503073
                    ,9.203688)
ROI.dynamic.str <- c(19.961504
                     ,22.9615
                     ,29.81129
                     ,34.58221
                     ,37.38607
                     ,39.02382)

ROI.total <- rbind(ROI.static.str,ROI.dynamic.str)
colnames(ROI.total) <- c("10%","20%","30%","40%","50%","60%")

#png("ROIEfficacyStr.png",width = 6, height = 6,units = 'in',res=600)

barplot(ROI.total,beside=TRUE,ylab="Retrun on investment",xlab="Vaccine efficacy",main="Strong Influenza",col=c(1,2),ylim=c(0,max(ROI.dynamic.str)+10))
#legend("top",legend=c("Static model","Dynamic model"),pch=15,col=c(1,2),cex=0.8)

#dev.off()

#Moderate

ROI.static.mod <- c(-0.1083176
                    ,0.7833648
                    ,1.675047
                    ,2.56673
                    ,3.458412
                    ,4.350094)
ROI.dynamic.mod <- c(13.607972
                     ,20.04605
                     ,20.82265
                     ,20.99478
                     ,21.0794
                     ,21.09044)

ROI.total <- rbind(ROI.static.mod,ROI.dynamic.mod)
colnames(ROI.total) <- c("10%","20%","30%","40%","50%","60%")

#png("ROIEfficacyMod.tiff",width = 6, height = 6,units = 'in',res=600)

barplot(ROI.total,beside=TRUE,ylab="Retrun on investment",xlab="Vaccine efficacy",main="Moderate Influenza",col=c(1,2),ylim=c(0,max(ROI.dynamic.mod)+10))
legend("top",legend=c("Static model","Dynamic model"),pch=15,col=c(1,2),cex=0.8,bty="n")

dev.off()