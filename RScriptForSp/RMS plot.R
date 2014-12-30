library(ggplot2)
rm(list=ls())


PVDF <- c(2.46,2.36,1.68,1.32)
C8BTBT <- c(2.73,2.64,1.66,1.45)
PEN <- c(11.83,12.67,14.71,10.98)
DeviceId <- c("Sample WOP","Sample WP","Sample P2", "Sample P3")
DeviceP <- rep("PVDF-TrFE",times=4)
DeviceC <- rep("C8-BTBT",times=4)
DevicePEN <- rep("PENTACENE",times=4)

PVDFALL <- data.frame(DeviceId,PVDF,DeviceP)
colnames(PVDFALL)[2] <- "RMS"
colnames(PVDFALL)[3] <- "Material"

C8ALL <- data.frame(DeviceId,C8BTBT,DeviceC)
colnames(C8ALL)[2] <- "RMS"
colnames(C8ALL)[3] <- "Material"

PENALL <- data.frame(DeviceId,PEN,DevicePEN)
colnames(PENALL)[2] <- "RMS"
colnames(PENALL)[3] <- "Material"


SlopeAll <- rbind(PVDFALL[1:2,],C8ALL[1:2,])

ymaj <- c(2.0,2.5,3.0)


PenPic <- qplot(data=SlopeAll,x=DeviceId,y=RMS ,colour = Material, main = "Surface roughness", xlab = "Sample ID", ylab = "RMS (nm)")+ theme_set(theme_bw())



PenPic + scale_y_log10(limits=c(1.5,3.3),breaks= ymaj,labels=ymaj)+geom_point(position = "dodge",alpha =0.8,size=20) + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )
#PenPic + geom_bar(ymin=1000,position = "dodge",alpha =0.8) + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )
