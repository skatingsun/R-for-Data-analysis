library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Recently Data/PEN")
getwd

## Set the working path and clear the parameter

Temp <- seq(210,280,by=10)
SampleID <- 1:4 
SampleName <- paste("PENSample",SampleID," corrected with Vth and Capacity W=200",sep="")
Dataname <- paste("SampleData",SampleID,sep="")
for (i in 1:4) {assign(Dataname[i], read.csv(SampleName[i],header=T))}

#PenTemp <- data.frame(Temp,SampleData1$Usat,SampleData2$Usat,SampleData3$Usat,SampleData4$Usat)

DeviceA <- rep("a",times=8)
DeviceB <- rep("b",times=8)
DeviceC <- rep("c",times=8)
DeviceD <- rep("d",times=8)


PenTempA <- data.frame(Temp,SampleData1$Usat[1:8],DeviceA)
colnames(PenTempA)[2] <- "Usat"
colnames(PenTempA)[3] <- "DeviceID"


PenTempB <- data.frame(Temp,SampleData2$Usat[1:8],DeviceB)
colnames(PenTempB)[2] <- "Usat"
colnames(PenTempB)[3] <- "DeviceID"

PenTempC <- data.frame(Temp,SampleData3$Usat[1:8],DeviceC)
colnames(PenTempC)[2] <- "Usat"
colnames(PenTempC)[3] <- "DeviceID"

PenTempD <- data.frame(Temp,SampleData4$Usat[1:8],DeviceD)
colnames(PenTempD)[2] <- "Usat"
colnames(PenTempD)[3] <- "DeviceID"

yA <- log(PenTempA$Usat[1:8]*Temp[1:8])
xA <- 1/PenTempA$Temp[1:8]
lmA <- lm(yA ~ xA)
slopeA <- coef(lmA)[2]

yB <- log(PenTempB$Usat[1:8]*Temp[1:8])
xB <- 1/PenTempA$Temp[1:8]
lmB <- lm(yB ~ xB)
slopeB <- coef(lmB)[2]

yC <- log(PenTempC$Usat[1:8]*Temp[1:8])
xC <- 1/PenTempC$Temp[1:8]
lmC <- lm(yC ~ xC)
slopeC <- coef(lmA)[2]

yD <- log(PenTempD$Usat[1:8]*Temp[1:8])
xD <- 1/PenTempD$Temp[1:8]
lmD <- lm(yD ~ xD)
slopeD <- coef(lmD)[2]


PenT <- rbind(PenTempA,PenTempB,PenTempC,PenTempD)


#colnames(PenTemp)[2] <- "UsatA"
#colnames(PenTemp)[3] <- "UsatB"
#colnames(PenTemp)[4] <- "UsatC"
#colnames(PenTemp)[4] <- "UsatD"

## Data prepare for ploting pic

#PenPic <- qplot(Temp,Usat,data=PenT,colour=DeviceID,log="y", geom="point",size = I(4), main = "Pentacene Mobility-Temperature Relation", xlab = "1/Temp (1/K)", ylab = expression(paste("Usat ","(", "cm"^"2","V"^"-1","s"^"-1",")")))+ theme_set(theme_bw())

PenPic <- qplot(1/Temp,Usat*Temp,data=PenT,colour=DeviceID,log="y", geom="point",size = I(4), main = "Pentacene Mobility-Temperature Relation All", xlab = "1/Temp (1/K)", ylab = expression(paste("Usat*Temp ","(", "cm"^"2","V"^"-1","s"^"-1","K)")))+ theme_set(theme_bw())

PenPic + geom_smooth(method="lm", size=0.5, alpha=0.1)+ theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )