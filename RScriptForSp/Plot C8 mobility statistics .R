library(ggplot2)
library(xlsx)
library(rJava)
library(xlsxjars)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Data/Edited data/Data analysis/interlayer effect 1/Sample N")
getwd

DataN <- read.xlsx("statistic.xlsx",1)

DataN <- DataN[1:7,]

setwd("/Users/sunhuabin/Dropbox/Data/Edited data/Data analysis/interlayer effect 1/Sample P1")
getwd

DataP <- read.xlsx("statistic.xlsx",1)
DataP <- DataP[1:7,]


setwd("/Users/sunhuabin/Dropbox/Data/Edited data/Data analysis/interlayer effect 1/Sample P2")
getwd

DataP2 <- read.xlsx("statistic.xlsx",1)
DataP2 <- DataP2[1:7,]

setwd("/Users/sunhuabin/Dropbox/Data/Edited data/Data analysis/interlayer effect 1/Sample P3")
getwd

DataP3 <- read.xlsx("statistic.xlsx",1)
DataP3 <- DataP3[1:7,]

#set the working path and read the Data of the sample
DeviceIDN <- rep("Without PMMA",times=7)
DeviceIDP <- rep("With PMMA",times=7)
DeviceIDP2 <- rep("Sample P2",times=7)
DeviceIDP3 <- rep("Sample FP",times=7)

StatN <- data.frame(DataN$DeviceID,DataN$mobility.2v.go, DataN$mobility.5v.go ,DataN$mobility.10v.go ,DataN$window.2 ,DataN$window.5, DataN$window.10 ,DeviceIDN)

colnames(StatN)[1] <- "ChannelLength"
colnames(StatN)[2] <- "Mobility2V"
colnames(StatN)[3] <- "Mobility5V"
colnames(StatN)[4] <- "Mobility10V"
colnames(StatN)[5] <- "Window2V"
colnames(StatN)[6] <- "Window5V"
colnames(StatN)[7] <- "Window10V"
colnames(StatN)[8] <- "DeviceID"

StatP <- data.frame(DataP$DeviceID,DataP$mobility.2v.go, DataP$mobility.5v.go ,DataP$mobility.10v.go ,DataP$window.2 ,DataP$window.5, DataP$window.10 ,DeviceIDP)

colnames(StatP)[1] <- "ChannelLength"
colnames(StatP)[2] <- "Mobility2V"
colnames(StatP)[3] <- "Mobility5V"
colnames(StatP)[4] <- "Mobility10V"
colnames(StatP)[5] <- "Window2V"
colnames(StatP)[6] <- "Window5V"
colnames(StatP)[7] <- "Window10V"
colnames(StatP)[8] <- "DeviceID"


StatP2 <- data.frame(DataP2$DeviceID,DataP2$mobility.2v.go, DataP2$mobility.5v.go ,DataP2$mobility.10v.go ,DataP2$window.2 ,DataP2$window.5, DataP2$window.10 ,DeviceIDP2)

colnames(StatP2)[1] <- "ChannelLength"
colnames(StatP2)[2] <- "Mobility2V"
colnames(StatP2)[3] <- "Mobility5V"
colnames(StatP2)[4] <- "Mobility10V"
colnames(StatP2)[5] <- "Window2V"
colnames(StatP2)[6] <- "Window5V"
colnames(StatP2)[7] <- "Window10V"
colnames(StatP2)[8] <- "DeviceID"

StatP3 <- data.frame(DataP3$DeviceID,DataP3$mobility.2v.go, DataP3$mobility.5v.go ,DataP3$mobility.10v.go ,DataP3$window.2 ,DataP3$window.5, DataP3$window.10 ,DeviceIDP3)

colnames(StatP3)[1] <- "ChannelLength"
colnames(StatP3)[2] <- "Mobility2V"
colnames(StatP3)[3] <- "Mobility5V"
colnames(StatP3)[4] <- "Mobility10V"
colnames(StatP3)[5] <- "Window2V"
colnames(StatP3)[6] <- "Window5V"
colnames(StatP3)[7] <- "Window10V"
colnames(StatP3)[8] <- "DeviceID"

Nmean <- colMeans(StatN[,1:4])
Pmean <- colMeans(StatP[,1:4])
StatData <- rbind(StatN,StatP)
ymaj <- c(0.1,0.5,1,2,4)
ymaj2 <- c(8,10,12,14,16)

#set plot types datas and x title and y title
IdPic <- qplot(DeviceID,Mobility10V,data=StatData, geom=("jitter"), colour = DeviceID, size = I(8), xlab = " ", ylab = expression(paste(mu["FET"]," (", "cm"^"2","V"^"-1","s"^"-1",")")))+ theme_set(theme_bw())
#set the scale ticks and lables position
IdPic <- IdPic+ scale_y_log10(limits=c(0.1,5),breaks= ymaj,labels=ymaj)
#set the text size in the pictures
IdPic <- IdPic+theme(axis.title.x=element_text(size=0), axis.title.y=element_text(size=24), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=20), legend.title=element_text(size=0))
#set the legend position in the picture none-> no legend
IdPic <- IdPic+ theme(legend.justification=c(1,0), legend.position="none")
#Draw a blank panel border
IdPic <-  IdPic+ theme(panel.border = element_rect(size= 1, colour = "black")) 
#remove the legend background
IdPic <- IdPic+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))
#remove grid lines
IdPic <- IdPic + theme(panel.background=element_rect(fill='transparent', color='grey'),panel.grid=element_blank())

IdPic

# IdPic <- qplot(DeviceID,Window10V,data=StatData, geom=("boxplot"), colour = DeviceID, size = I(1), main = "Memory window statistics of the Samples", xlab = "Sample ID", ylab = "Memory Window (V)")

#IdPic + scale_y_log10(limits=c(0.1,10)) + geom_path(alpha=0.1,size=3) + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )

# 
# IdPic <- qplot(DeviceID,Window10V,data=StatData, geom=("jitter"), colour = DeviceID, size = I(8), xlab = " ", ylab = "Memory Window (V)")+ theme_set(theme_bw())
# IdPic+  scale_y_continuous(limits=c(8,16),breaks= ymaj2,labels=ymaj2)+theme(axis.title.x=element_text(size=0), axis.title.y=element_text(size=24), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=20), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position="none") + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))

