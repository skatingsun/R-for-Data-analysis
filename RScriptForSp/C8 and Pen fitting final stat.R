library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Recently Data/C8/Bianwen")
getwd

## Set the working path and clear the parameter

Temp <- seq(200,300,by=10)
SampleID <- 1:4 
SampleName <- paste("C8Sample",SampleID," C3 Capacity and Vth correction",sep="")
Dataname <- paste("SampleData",SampleID,sep="")
for (i in 1:4) {assign(Dataname[i], read.csv(SampleName[i],header=T))}

#C8Temp <- data.frame(Temp,SampleData1$Usat,SampleData2$Usat,SampleData3$Usat,SampleData4$Usat)

DeviceA <- rep("a",times=11)
DeviceB <- rep("b",times=11)
DeviceC <- rep("c",times=11)
DeviceD <- rep("d",times=11)


C8TempA <- data.frame(Temp,SampleData1$Usat[1:11],DeviceA)
colnames(C8TempA)[2] <- "Usat"
colnames(C8TempA)[3] <- "DeviceID"


C8TempB <- data.frame(Temp,SampleData2$Usat[1:11],DeviceB)
colnames(C8TempB)[2] <- "Usat"
colnames(C8TempB)[3] <- "DeviceID"

C8TempC <- data.frame(Temp,SampleData3$Usat[1:11],DeviceC)
colnames(C8TempC)[2] <- "Usat"
colnames(C8TempC)[3] <- "DeviceID"

C8TempD <- data.frame(Temp,SampleData4$Usat[1:11],DeviceD)
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

SlopC3 <- c(slopeA,slopeB,slopeC,slopeD)


SampleName <- paste("C8Sample",SampleID," C2 Capacity and Vth correction",sep="")
Dataname <- paste("SampleData",SampleID,sep="")
for (i in 1:4) {assign(Dataname[i], read.csv(SampleName[i],header=T))}

#C8Temp <- data.frame(Temp,SampleData1$Usat,SampleData2$Usat,SampleData3$Usat,SampleData4$Usat)

DeviceA <- rep("a",times=11)
DeviceB <- rep("b",times=11)
DeviceC <- rep("c",times=11)
DeviceD <- rep("d",times=11)


C8TempA <- data.frame(Temp,SampleData1$Usat[1:11],DeviceA)
colnames(C8TempA)[2] <- "Usat"
colnames(C8TempA)[3] <- "DeviceID"


C8TempB <- data.frame(Temp,SampleData2$Usat[1:11],DeviceB)
colnames(C8TempB)[2] <- "Usat"
colnames(C8TempB)[3] <- "DeviceID"

C8TempC <- data.frame(Temp,SampleData3$Usat[1:11],DeviceC)
colnames(C8TempC)[2] <- "Usat"
colnames(C8TempC)[3] <- "DeviceID"

C8TempD <- data.frame(Temp,SampleData4$Usat[1:11],DeviceD)
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
SlopC2 <- c(slopeA,slopeB,slopeC,slopeD)

SampleName <- paste("C8Sample",SampleID," C4 Capacity and Vth correction",sep="")
Dataname <- paste("SampleData",SampleID,sep="")
for (i in 1:4) {assign(Dataname[i], read.csv(SampleName[i],header=T))}

#C8Temp <- data.frame(Temp,SampleData1$Usat,SampleData2$Usat,SampleData3$Usat,SampleData4$Usat)

DeviceA <- rep("a",times=11)
DeviceB <- rep("b",times=11)
DeviceC <- rep("c",times=11)
DeviceD <- rep("d",times=11)


C8TempA <- data.frame(Temp,SampleData1$Usat[1:11],DeviceA)
colnames(C8TempA)[2] <- "Usat"
colnames(C8TempA)[3] <- "DeviceID"


C8TempB <- data.frame(Temp,SampleData2$Usat[1:11],DeviceB)
colnames(C8TempB)[2] <- "Usat"
colnames(C8TempB)[3] <- "DeviceID"

C8TempC <- data.frame(Temp,SampleData3$Usat[1:11],DeviceC)
colnames(C8TempC)[2] <- "Usat"
colnames(C8TempC)[3] <- "DeviceID"

C8TempD <- data.frame(Temp,SampleData4$Usat[1:11],DeviceD)
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
SlopC4 <- c(slopeA,slopeB,slopeC,slopeD)

DeviceID <- c("Sample N", "Sample P1")
SlopeC2 <- data.frame(SlopC2[1:2],DeviceID)
colnames(SlopeC2)[1] <- "slope"
SlopeC3 <- data.frame(SlopC3[1:2],DeviceID)
colnames(SlopeC3)[1] <- "slope"
SlopeC4 <- data.frame(SlopC4[1:2],DeviceID)
colnames(SlopeC4)[1] <- "slope"

SlopeAll <- rbind(SlopeC2,SlopeC3,SlopeC4)
