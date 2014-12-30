library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Recently Data/PEN/Pen bianwen 3 and 4")
getwd

## Set the working path and clear the parameter

Temp <- seq(220,300,by=20)
ColNumber <- 3
ChannelLength <- 200
DeviceID <- paste("Sample4 ",Temp,"k ",ChannelLength,"u ","C3  Saturation Transfer.txt",sep="") #批量组合文件名
SatTemp <- paste("PENSat",Temp, sep = "")
IdTemp <- paste("ID",Temp,sep="")
for (i in 1:5){assign(SatTemp[i],read.table(DeviceID[i],header=T,sep= '\t'))}

Vbg <- PENSat300$Vg

for (i in 1:5) {
        TempID <- rep(paste("Temp = ",Temp[i],"k"),times=122)
        Id <- eval(parse(text=SatTemp[i]))$Id
        assign(IdTemp[i],data.frame(Vbg,Id,TempID))
        #colnames(eval(parse(IdTemp[i])))[2] <- "Id" 
}

C8T <- rbind(ID220,ID240,ID260,ID280,ID300)

C8Pic <- qplot(Vbg,Id,data=C8T,colour=TempID, geom="point",size = I(4), main = "Pentacene Id Temperature Relation", xlab = "Vg (V)", ylab = "Id (A)")+ theme_set(theme_bw()) 
C8Pic + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )