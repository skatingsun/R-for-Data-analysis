library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/pingban/N")
getwd

CVDataN <- read.table("N-4-30v.dat", header = T, sep=",")
# every frequency step have 162 points of data

setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/pingban/P1")
getwd

CVDataP <- read.table("P1-4-30v.dat", header = T, sep=",")

setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/pingban/P2")
getwd

CVDataP2 <- read.table("P2-4-30v.dat", header = T, sep=",")

setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/pingban/P3")
getwd

CVDataP3 <- read.table("P3-4-30v.dat", header = T, sep=",")

CVDataN20 <- CVDataN[1:162,]
DeviceIdN <- rep("Metal/P(VDF-TrFE)/Metal",times=162)
CVDataN20 <- data.frame(CVDataN20,DeviceIdN)
colnames(CVDataN20)[6] <- "DeviceID"

CVDataP20 <- CVDataP[1:162,]
DeviceIdP <- rep("Metal/P(VDF-TrFE)/PMMA/Metal",times=162)
CVDataP20 <- data.frame(CVDataP20,DeviceIdP)
colnames(CVDataP20)[6] <- "DeviceID"

CVDataP220 <- CVDataP2[1:162,]
DeviceIdP2 <- rep("Metal/P(VDF-TrFE)/thick PMMA/Metal",times=162)
CVDataP220 <- data.frame(CVDataP220,DeviceIdP2)
colnames(CVDataP220)[6] <- "DeviceID"

CVDataP320 <- CVDataP3[1:162,]
DeviceIdP3 <- rep("Sample P3",times=162)
CVDataP320 <- data.frame(CVDataP320,DeviceIdP3)
colnames(CVDataP320)[6] <- "DeviceID"
# get the data of freq=20Hz

#CVData <- rbind(CVDataN20,CVDataP20,CVDataP220)
CVData <- rbind(CVDataN20,CVDataP20)
ymaj <- c(10*10^-9,20*10^-9,30*10^-9)
xmaj <- c(-30,-20,-10,0,10,20,30)

IdPic <- qplot( Bias, Cp/0.01, data=CVData ,colour = DeviceID, geom=("point"),size = I(2), xlab = "Bias Voltage (V)", ylab=expression(paste("Capacitance"," (", "nF/cm"^"-2",")")))+ theme_set(theme_bw()) 
IdPic  +geom_path(alpha=1,size=2) + scale_y_continuous(limits=c(15*10^-10,2.6*10^-8),breaks=ymaj,labels=c(expression(paste("10")),expression(paste("20")),expression(paste("30")))) + scale_x_continuous(breaks= xmaj,labels=xmaj) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=16), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(0.9,0)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))