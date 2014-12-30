library(ggplot2)
rm(list=ls())


C8BTBT <- c(1741,2079,1928,1794,1753,2148,1741,2079,1928,1506,1585,1708)

PEN <- c(1316,1305,1265,1185,1316,1305,1038,1189)

C8BTBT <- C8BTBT/11.5
PEN <- PEN/11.5

DeviceId <- c("Sample WOP","Sample WOP","Sample WOP","Sample WP","Sample WP","Sample WP","Sample FP","Sample FP","Sample FP", "Sample P3", "Sample P3", "Sample P3")
DeviceId2 <- c("Sample WOP","Sample WOP","Sample WP","Sample WP","Sample FP","Sample FP",  "Sample P3", "Sample P3")


C8ALL <- data.frame(DeviceId,C8BTBT)
colnames(C8ALL)[2] <- "AE"
C8ALL <- C8ALL[1:6,]

PENALL <- data.frame(DeviceId2,PEN)
colnames(PENALL)[2] <- "AE"
colnames(PENALL)[1] <- "DeviceId"
PENALL <- PENALL[1:6,]
ymaj <- c(150,160,180)

PenPic <- qplot( data=C8ALL, x= DeviceId, y = AE , geom=("boxplot"), colour = DeviceId, size = I(1), main = "Activation energy of Devices", xlab = "Sample ID", ylab = "Activation Energy (meV)")+ theme_set(theme_bw())


PenPic + scale_y_log10(limits=c(140,200),breaks= ymaj,labels=ymaj)+theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )

# 
# PenPic + geom_bar(ymin=1000,position = "dodge",alpha =0.8) + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )
