library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Transfer and output")
getwd

## Working path setting

FeFETN <- read.table("13-03-01 N 200u 01 Transistor 10 30.txt", header=T, sep= '\t', skip=2)
FeFETP <- read.table("13-03-01 P1 200u 01 Transistor 10 30.txt", header=T, sep= '\t', skip=2)
FeFETP2 <- read.table("13-03-01 P2 200u 01 Transistor 10 30.txt", header=T, sep= '\t', skip=2)
FeFETP3 <- read.table("13-03-01 P3 200u 01 Transistor 10 30.txt", header=T, sep= '\t', skip=2)
DeviceIDN <- rep("Without PMMA Buffering", times=162)
DeviceIDP <- rep("With PMMA Buffering", times=162)
DeviceIDP2 <- rep("Sample P2", times=162)
DeviceIDP3 <- rep("Sample P3", times=162)
TransferDataN <- data.frame(FeFETN$Vg,FeFETN$I_D,FeFETN$Ig,DeviceIDN)
colnames(TransferDataN)[1] <- "Vg"
colnames(TransferDataN)[2] <- "Id"
colnames(TransferDataN)[3] <- "Ig"
colnames(TransferDataN)[4] <- "DeviceID"

TransferDataP <- data.frame(FeFETP$Vg,FeFETP$I_D,FeFETP$Ig,DeviceIDP)
colnames(TransferDataP)[1] <- "Vg"
colnames(TransferDataP)[2] <- "Id"
colnames(TransferDataP)[3] <- "Ig"
colnames(TransferDataP)[4] <- "DeviceID"

TransferDataP2 <- data.frame(FeFETP2$Vg,FeFETP2$I_D,FeFETP2$Ig,DeviceIDP2)
colnames(TransferDataP2)[1] <- "Vg"
colnames(TransferDataP2)[2] <- "Id"
colnames(TransferDataP2)[3] <- "Ig"
colnames(TransferDataP2)[4] <- "DeviceID"

TransferDataP3 <- data.frame(FeFETP3$Vg,FeFETP3$I_D,FeFETP3$Ig,DeviceIDP3)
colnames(TransferDataP3)[1] <- "Vg"
colnames(TransferDataP3)[2] <- "Id"
colnames(TransferDataP3)[3] <- "Ig"
colnames(TransferDataP3)[4] <- "DeviceID"

DeviceData <- rbind(TransferDataN,TransferDataP)


ymaj <- c(1*10^-10,1*10^-8,1*10^-6,1*10^-4)
ymaj2 <- c(-15*10^-9,-10*10^-9,-5*10^-9,0)
xmaj <- c(-30,-20,-10,0,10,20,30)
# 
# IdPic <- qplot(Vg,Id,data=DeviceData,colour=DeviceID, shape=DeviceID, geom=("point"),size = I(1), xlab=expression(paste("V"["g"]," (V)")), ylab=expression(paste("I"["d"]," (A)")))+ theme_set(theme_bw()) 
# IdPic <- IdPic + geom_path(alpha=1,size=2) + scale_y_log10(breaks= ymaj,labels=c(expression(paste("10"^"-10")),expression(paste("10"^"-8")),expression(paste("10"^"-6")),expression(paste("10"^"-4")))) + scale_x_continuous(breaks= xmaj,labels= xmaj) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=20), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(1,0.78)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))
# 
# IdPic+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))


# previous setting

# IdPic <- qplot(Vg,Id,log=("y"),data=DeviceData,colour=DeviceID, geom=("path"),size = I(2), main = "FeFET Transfer", xlab = "Vg (V)", ylab = "Id (A)")+ theme_set(theme_bw()) 
# IdPic + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=28)) + scale_colour_grey()




#leakage current plot


# 
# IdPic <- qplot(Vg,Ig,data=DeviceData,colour=DeviceID, shape=DeviceID, geom=("point"),size = I(1), xlab=expression(paste("V"["g"]," (V)")), ylab=expression(paste("I"["g"]," (nA)")))+ theme_set(theme_bw()) 
# IdPic <- IdPic + geom_path(alpha=1,size=2) + scale_y_continuous(breaks=ymaj2,labels=c("-15","-10","-5","0")) + scale_x_continuous(breaks= xmaj,labels= xmaj) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=20), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(1,0.1)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))
# 
# IdPic+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))

setwd("/Users/sunhuabin/Dropbox/")
write.csv(TransferDataP,file = "Transfer.csv")

IdPic <- qplot(Vg,Id,data=TransferDataP, geom=("point"),size = I(3), xlab=expression(paste("V"["g"]," (V)")), ylab=expression(paste("I"["d"]," (A)")))+ theme_set(theme_bw()) 
IdPic <- IdPic + geom_path(alpha=0.8,size=2,colour="blue") + scale_y_log10(breaks= ymaj,labels=c(expression(paste("10"^"-10")),expression(paste("10"^"-8")),expression(paste("10"^"-6")),expression(paste("10"^"-4")))) + scale_x_continuous(breaks= xmaj,labels= xmaj) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=20), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(1,0.78)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))

IdPic+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))