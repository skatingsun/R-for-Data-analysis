library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/N")
getwd

CVDataN <- read.table("100u30V.dat", header = T, sep=",")
# every frequency step have 102 points of data

setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P1")
getwd

CVDataP <- read.table("100u30v.dat", header = T, sep=",")

setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P2")
getwd

CVDataP2 <- read.table("100u30v.dat", header = T, sep=",")


setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P3")
getwd

CVDataP3 <- read.table("100u30v.dat", header = T, sep=",")



CVDataN20 <- CVDataN[205:306,]
DeviceIdN <- rep("Without PMMA Buffering",times=102)
CVDataN20 <- data.frame(CVDataN20,DeviceIdN)
colnames(CVDataN20)[6] <- "DeviceID"

CVDataP20 <- CVDataP[205:306,]
DeviceIdP <- rep("With PMMA Buffering",times=102)
CVDataP20 <- data.frame(CVDataP20,DeviceIdP)
colnames(CVDataP20)[6] <- "DeviceID"

CVDataP220 <- CVDataP2[1:102,]
DeviceIdP2 <- rep("Sample FP",times=102)
CVDataP220 <- data.frame(CVDataP220,DeviceIdP2)
colnames(CVDataP220)[6] <- "DeviceID"

CVDataP320 <- CVDataP3[1:102,]
DeviceIdP3 <- rep("Sample P3",times=102)
CVDataP320 <- data.frame(CVDataP320,DeviceIdP3)
colnames(CVDataP320)[6] <- "DeviceID"

# get the data of freq=20Hz

CVData <- rbind(CVDataN20,CVDataP20)

ymaj <- c(1*10^-10,1.2*10^-10,1.4*10^-10,1.6*10^-10)

IdPic <- qplot(Bias,Cp,data=CVData,colour=DeviceID, geom=("point"),size = I(3), xlab=expression(paste("V"["g"]," (V)")), ylab = "Capacitrance (pF)")+ theme_set(theme_bw()) 
IdPic + geom_path(alpha=1,size=2) + scale_y_continuous(breaks= ymaj,labels= c("100","120","140","160"))+ theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(0.6,0)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))