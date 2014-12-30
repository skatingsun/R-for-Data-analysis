library(ggplot2)
library(gridExtra)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Figures new")
getwd

## Working path setting

PulseN <- read.csv("N 100u 20v.csv", header=T)
PulseP <- read.csv("p1 100u 20v.csv", header=T)


DataN <- data.frame(PulseN$gate.time[1:39], PulseN$drain.current[1:39])
colnames(DataN)[1] <- "time"
colnames(DataN)[2] <- "Id"
DataN$Id[]

DataN <- na.omit(DataN)

DataP <- data.frame(PulseP$gate.time[1:39], PulseP$drain.current[1:39])
colnames(DataP)[1] <- "time"
colnames(DataP)[2] <- "Id"




ymajP <- c(0,10^-7,2*10^-7)
ymajN <- c(-1*10^-8,0,1*10^-8,2*10^-8)
xmaj <- c(2.5,2.6,2.7,2.8,2.9,3.0)

# 
# 
# IdPic1 <- qplot(time,abs(Id),data=DataP,geom=("point"),size = I(2), xlab = "Time (s)", ylab = expression(paste("I"["d"]," (A)")))+ theme_set(theme_bw()) 
# IdPic1 <- IdPic1+ geom_path(alpha=0.5,size=2)  + scale_x_continuous(limits=c(1.05,8.06))+scale_y_log10(limits=c(4*10^-9,1*10^-5),breaks= ymajP,labels=c(expression(paste("10"^"-10")),expression(paste("10"^"-8")),expression(paste("10"^"-7")),expression(paste("10"^"-6")),expression(paste("10"^"-5"))))+ theme(axis.title.x=element_text(size=0), axis.title.y=element_text(size=24), plot.title=element_text(size=30), axis.text.x=element_text(size=0), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(1,0.7)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))
# IdPic2 <- qplot(time,Vg,data=DataP, geom=("point"),size = I(2), xlab = "Time (s)", ylab = expression(paste("V"["g"]," (V)")))+ theme_set(theme_bw()) 
# IdPic2 <- IdPic2 + scale_x_continuous(limits=c(1.06,8.06))+scale_y_continuous(limits=c(-25,25))+geom_path(alpha=1,size=1) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=24), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(1,0.7)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))




# IdPic1 <- qplot(time,Id,data=DataN, geom=("point"),size = I(3),xlab = "Time (s)", ylab = expression(paste("I"["d"]," (A)")))+ theme_set(theme_bw()) 
# IdPic1 <- IdPic1 + scale_x_continuous()+scale_y_continuous(limits=c(-0.1*10^-8,2.2*10^-8), breaks= ymajN,labels=c(expression(paste("-10"^"-8")),"0",expression(paste("10"^"-8")),expression(paste("2x10"^"-8"))))+geom_path(alpha=0.8,size=2) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=20)) + theme(legend.justification=c(1,0), legend.position=c(1,0.7)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))
# 
# IdPic1


IdPic1 <- qplot(time,Id,data=DataP, geom=("point"),size = I(3),xlab = "Time (s)", ylab = expression(paste("I"["d"]," (A)")))+ theme_set(theme_bw()) 
IdPic1 <- IdPic1 + scale_x_continuous()+scale_y_continuous(limits=c(-0.1*10^-8,2.05*10^-7), breaks= ymajP,labels=c("0",expression(paste("10"^"-7")),expression(paste("2x10"^"-7"))))+geom_path(alpha=0.8,size=2) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=20)) + theme(legend.justification=c(1,0), legend.position=c(1,0.7)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))

IdPic1


