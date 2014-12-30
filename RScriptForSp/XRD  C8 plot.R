library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Data/XRD")
getwd()

PVDF <- read.table("PVDF.txt", header = F, sep=",")
Sub <- PVDF[,2]
names(PVDF) <- c("Angle","Counts")

DataN <- read.table("C8-WOP.txt", header = F, sep=",")
DeviceID <- rep("Film without PMMA",times=1268)

SampleN <- data.frame(DataN[,1:2],DeviceID)
names(SampleN) <- c("Angle","Counts","DeviceID")


DataP <- read.table("C8-WP.txt", header = F, sep=",")
DeviceID2 <- rep("Film with PMMA",times=1268)

SampleP <- data.frame(DataP[,1:2],DeviceID2)
names(SampleP) <- c("Angle","Counts","DeviceID")


Ncount<- SampleN$Count +60000
Pcount<- SampleP$Count +45000
PVDFcount <- Sub +30000
Ncountnew<- SampleN$Count -Sub+15000
Pcountnew<- SampleP$Count -Sub

DeviceID3 <- rep("Film with PMMA (caculated)",times=1268)
DeviceID4 <- rep("Film without PMMA (caculated)",times=1268)
DeviceIDP <- rep("Film Sub",times=1268)

SampleN1 <- data.frame(SampleN$Angle,Ncount,DeviceID)
names(SampleN1) <- c("Angle","Counts","DeviceID")
SampleP1 <- data.frame(SampleP$Angle,Pcount,DeviceID2)
names(SampleP1) <- c("Angle","Counts","DeviceID")

SampleN1new <- data.frame(SampleN$Angle,Ncountnew,DeviceID3)
names(SampleN1new) <- c("Angle","Counts","DeviceID")

SampleSub <- data.frame(SampleP$Angle,PVDFcount,DeviceIDP)
names(SampleSub) <- c("Angle","Counts","DeviceID")

SampleP1new <- data.frame(SampleP$Angle,Pcountnew,DeviceID4)
names(SampleP1new) <- c("Angle","Counts","DeviceID")

DataAll <- rbind(SampleN1,SampleP1,SampleSub,SampleN1new,SampleP1new)

ymaj <- c(0,20000,40000,60000)
xmaj <- c(-40,-30,-20,-10,0)
#xlab =expression(paste("2",theta," (degree)")),

IdPic <- qplot(Angle,Counts,data=DataAll, geom=("point"),size = I(2), ylab = "Intensity (arb. unit)",xlab =expression(paste("2",theta," (degree)")))+ theme_set(theme_bw()) 


IdPic + geom_path(alpha=1,size=1) + scale_y_continuous(breaks= ymaj,labels=ymaj) + scale_x_continuous(limits=c(5,30)) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=0), legend.text=element_text(size=24), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(0.75,0.65)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed")) + scale_colour_grey()


#IdPic + geom_path(alpha=0.4,size=1)+scale_x_continuous(limits=c(5,10)) + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20))