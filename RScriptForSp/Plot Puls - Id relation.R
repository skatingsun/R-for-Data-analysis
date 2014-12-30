library(ggplot2)
library(gridExtra)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/pulse test")
getwd

## Working path setting

PulseN <- read.csv("N 100u 20v.csv", header=T)
PulseP <- read.csv("p1 100u 20v.csv", header=T)
DeviceIDN <- rep("Device withtout PMMA", times=565)
DeviceIDP <- rep("Device with PMMA", times=565)
IDVG <- rep("Gate Voltage", times=565)
IDID <- rep("Drain Current", times=565)


DataN <- data.frame(PulseN$Drain.Time.1. , PulseN$Gate.Voltage.1. ,  PulseN$Drain.Current.1. , PulseN$Gate.Current.1. , DeviceIDN)
colnames(DataN)[1] <- "time"
colnames(DataN)[2] <- "Vg"
colnames(DataN)[3] <- "Id"
colnames(DataN)[4] <- "Ig"
colnames(DataN)[5] <- "DeviceID"


DataP <- data.frame(PulseP$Drain.Time.1. , PulseP$Gate.Voltage.1. ,  PulseP$Drain.Current.1. , PulseP$Gate.Current.1. , DeviceIDP)
colnames(DataP)[1] <- "time"
colnames(DataP)[2] <- "Vg"
colnames(DataP)[3] <- "Id"
colnames(DataP)[4] <- "Ig"
colnames(DataP)[5] <- "DeviceID"

DeviceData <- rbind(DataN,DataP)

DeviceNVG <- data.frame(DataN$time,DataN$Vg,IDVG)
names(DeviceNVG) <- c("time","Vg","facel")

DeviceNID <- data.frame(DataN$time,DataN$Id,IDID)
names(DeviceNID) <- c("time","Vg","facel")

NAll <- rbind(DeviceNVG,DeviceNID)

ymajP <- c(10^-10,10^-8,10^-7,10^-6,10^-5)
ymajN <- c(10^-9,10^-8,10^-6)
xmaj <- c(2.5,2.6,2.7,2.8,2.9,3.0)

# 
# 
# IdPic1 <- qplot(time-1.06,abs(Id),data=DataP,geom=("point"),size = I(2), xlab = "Time (s)", ylab = expression(paste("I"["d"]," (A)")))+ theme_set(theme_bw()) 
# IdPic1 <- IdPic1+ geom_path(alpha=0.5,size=2)  + scale_x_continuous(limits=c(-0.01,7.01))+scale_y_log10(limits=c(4*10^-9,1*10^-5),breaks= ymajP,labels=c(expression(paste("10"^"-10")),expression(paste("10"^"-8")),expression(paste("10"^"-7")),expression(paste("10"^"-6")),expression(paste("10"^"-5"))))+ theme(axis.title.x=element_text(size=0), axis.title.y=element_text(size=24), plot.title=element_text(size=30), axis.text.x=element_text(size=0), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(1,0.7)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))+ theme(panel.background=element_rect(fill='transparent', color='grey'),panel.grid=element_blank())
# IdPic2 <- qplot(time-1.06,Vg,data=DataP, geom=("point"),size = I(2), xlab = "Time (s)", ylab = expression(paste("V"["g"]," (V)")))+ theme_set(theme_bw()) 
# IdPic2 <- IdPic2 + scale_x_continuous(limits=c(-0.01,7.01))+scale_y_continuous(limits=c(-25,25))+geom_path(alpha=1,size=1) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=24), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(1,0.7)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))+ theme(panel.background=element_rect(fill='transparent', color='grey'),panel.grid=element_blank())




IdPic1 <- qplot(time-1.12,abs(Id),data=DataN, geom=("point"),size = I(2), ylab = expression(paste("I"["d"]," (A)")))+ theme_set(theme_bw()) 
IdPic1 <- IdPic1 + scale_x_continuous(limits=c(-0.01,7.01))+scale_y_log10(limits=c(10^-10,1*10^-7), breaks= ymajN,labels=c(expression(paste("10"^"-9")),expression(paste("10"^"-8")),expression(paste("10"^"-6"))))+geom_line(alpha=0.5,size=2) + theme(axis.title.x=element_text(size=0), axis.title.y=element_text(size=24), plot.title=element_text(size=30), axis.text.x=element_text(size=0), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(1,0.7)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.background=element_rect(fill='transparent', color='gray'),panel.grid=element_blank())


IdPic2 <- qplot(time-1.12,Vg,data=DataN, geom=("point"),size = I(2), xlab = "Time (s)", ylab = expression(paste("V"["g"]," (V)")))+ theme_set(theme_bw()) 
IdPic2 <- IdPic2 + scale_x_continuous(limits=c(-0.01,7.01))+scale_y_continuous(limits=c(-25,25))+geom_path(alpha=1,size=1) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=24), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(1,0.7)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.background=element_rect(fill='transparent', color='grey'),panel.grid=element_blank())



gp1<- ggplot_gtable(ggplot_build(IdPic1))
gp2<- ggplot_gtable(ggplot_build(IdPic2))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
grid.arrange(gp1, gp2)





