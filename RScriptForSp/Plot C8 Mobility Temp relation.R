library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Recently Data/C8/Bianwen")
getwd

## Set the working path and clear the parameter

Temp <- seq(200,250,by=10)
SampleID <- 1:4 
SampleName <- paste("C8Sample",SampleID," C3 Capacity and Vth correction",sep="")
Dataname <- paste("SampleData",SampleID,sep="")
for (i in 1:4) {assign(Dataname[i], read.csv(SampleName[i],header=T))}

#C8Temp <- data.frame(Temp,SampleData1$Usat,SampleData2$Usat,SampleData3$Usat,SampleData4$Usat)

DeviceA <- rep("Without PMMA Buffering",times=6)
DeviceB <- rep("With PMMA Buffering",times=6)
DeviceC <- rep("Sample P2",times=6)
DeviceD <- rep("Sample P3",times=6)


C8TempA <- data.frame(Temp,SampleData1$Usat[1:6],DeviceA)
colnames(C8TempA)[2] <- "Usat"
colnames(C8TempA)[3] <- "DeviceID"


C8TempB <- data.frame(Temp,SampleData2$Usat[1:6],DeviceB)
colnames(C8TempB)[2] <- "Usat"
colnames(C8TempB)[3] <- "DeviceID"

C8TempC <- data.frame(Temp,SampleData3$Usat[1:6],DeviceC)
colnames(C8TempC)[2] <- "Usat"
colnames(C8TempC)[3] <- "DeviceID"

C8TempD <- data.frame(Temp,SampleData4$Usat[1:6],DeviceD)
colnames(C8TempD)[2] <- "Usat"
colnames(C8TempD)[3] <- "DeviceID"


#linear fitting
yA <- log(C8TempA$Usat[1:6]*Temp[1:6])
xA <- 1/C8TempA$Temp[1:6]
lmA <- lm(yA ~ xA)
slopeA <- coef(lmA)[2]

yB <- log(C8TempB$Usat[1:6]*Temp[1:6])
xB <- 1/C8TempA$Temp[1:6]
lmB <- lm(yB ~ xB)
slopeB <- coef(lmB)[2]

yC <- log(C8TempC$Usat[1:6]*Temp[1:6])
xC <- 1/C8TempC$Temp[1:6]
lmC <- lm(yC ~ xC)
slopeC <- coef(lmA)[2]

yD <- log(C8TempD$Usat[1:6]*Temp[1:6])
xD <- 1/C8TempD$Temp[1:6]
lmD <- lm(yD ~ xD)
slopeD <- coef(lmD)[2]

C8T <- rbind(C8TempA[1:6,],C8TempB[1:6,])


#colnames(C8Temp)[2] <- "UsatA"
#colnames(C8Temp)[3] <- "UsatB"
#colnames(C8Temp)[4] <- "UsatC"
#colnames(C8Temp)[4] <- "UsatD"

## Data prepare for ploting pic

C8Pic <- qplot(1000/Temp,Usat*Temp,data=C8T,colour=DeviceID,log="y", geom="point",size = I(5),  xlab = "1000/T (1/K)", ylab = expression(paste(mu["FET"],"·T ","(", "cm"^"2","V"^"-1","s"^"-1","·K)")))+ theme_set(theme_bw()) 
C8Pic + geom_smooth(method="lm", size=2,alpha=0,linetype="dashed")+ theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=16), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(0.85,0.45)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))