library(ggplot2)
library(baseline)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/N")
getwd

CVDataN <- read.table("100uCf.dat", header = T, sep=",")
# every frequency step have 102 points of data

setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P1")
getwd

CVDataP <- read.table("100uCf.dat", header = T, sep=",")


setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P2")
getwd

CVDataP2 <- read.table("CF100u.dat", header = T, sep=",")

setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P3")
getwd

CVDataP3 <- read.table("Cf100u.dat", header = T, sep=",")





CVDataN20 <- CVDataN[1:501,]
DeviceIdN <- rep("Without PMMA Buffering",times=501)
CVDataN20 <- data.frame(CVDataN20,DeviceIdN)
colnames(CVDataN20)[6] <- "DeviceID"

CVDataP20 <- CVDataP[1:501,]
DeviceIdP <- rep("With PMMA Buffering",times=501)
CVDataP20 <- data.frame(CVDataP20,DeviceIdP)
colnames(CVDataP20)[6] <- "DeviceID"

CVDataP220 <- CVDataP2[1:501,]
DeviceIdP2 <- rep("Sample P2",times=501)
CVDataP220 <- data.frame(CVDataP220,DeviceIdP2)
colnames(CVDataP220)[6] <- "DeviceID"

CVDataP320 <- CVDataP3[1:501,]
DeviceIdP3 <- rep("Sample P3",times=501)
CVDataP320 <- data.frame(CVDataP320,DeviceIdP3)
colnames(CVDataP320)[6] <- "DeviceID"

ymaj <- c(1*10^-6,1*10^-4,1*10^-2,1)
xmaj <- c(1*10^2,1*10^3,1*10^4)

# get the data of freq=20Hz

CVData <- rbind(CVDataN20,CVDataP20)
IdPic <- qplot(Freq,Cp/G,log=("xy"),data=CVData,colour=DeviceID, geom=("point"),size = I(2), xlab = "Frequency (Hz)", ylab = "Capacitance/Conductance")+ theme_set(theme_bw()) 
IdPic + geom_path(alpha=1,size=2) + scale_y_log10(limits=c(10^-7,100),breaks= ymaj,labels=c(expression(paste("10"^"-6")),expression(paste("10"^"-4")),expression(paste("10"^"-2")),"1")) + scale_x_log10(limits=c(80,1.2*10^4),breaks= xmaj,labels=c(expression(paste("10"^"2")),expression(paste("10"^"3")),expression(paste("10"^"4")))) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=20), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(0.85,0.1)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))




#scale_y_log10(breaks= ymaj,labels=c(expression(paste("10"^"-6")),expression(paste("10"^"-4")),expression(paste("10"^"-2"))))
