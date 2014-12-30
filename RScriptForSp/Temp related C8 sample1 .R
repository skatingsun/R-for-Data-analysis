library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Recently Data/C8/Bianwen")
getwd()
## Set the working folder

ColNumber <- 3
ChannelLength <- 200
Temp <- 200
Capacity <- 3.4*10^-8 #3.4*10^-8 for PVDF-TrFE200nm /SiO2 50nm and 1.15*10^-8 for 300nm SiO2
LWRatio <- 0.2 #0.2 for  200u channel
## Set the defacut value

Temp <- (20:30)*10
DeviceID <- paste("Sample4 ",Temp,"k ",ChannelLength,"u C",ColNumber,"  Saturation Transfer.txt",sep="") #批量组合文件名
SatTemp <- paste("C8Sat",Temp, sep = "")
for (i in 1:11){assign(SatTemp[i],read.table(DeviceID[i],header=T,sep= '\t'))}

IDhalfTemp <- paste("IDroot",Temp, sep = "")
for (i in 1:11){assign(IDhalfTemp[i],eval(parse(text = SatTemp[i]))$IDEhalf)}
VbgSat <- eval(parse(text = SatTemp[1]))$Vbg

C8Temp <- data.frame(VbgSat,IDroot200,IDroot210,IDroot220,IDroot230,IDroot240,IDroot250,IDroot260,IDroot270,IDroot280,IDroot290,IDroot300)

Usat <- rep(0,times=11)
Vth <- rep(0,times=11)
Idrootext <- rep(0,times=6)

VbgSatExt <-C8Temp$VbgSat[(length(C8Temp$VbgSat)/2-5):(length(C8Temp$VbgSat)/2)]  
for (i in 1:11) {
  Idrootext <- C8Temp[(length(C8Temp$VbgSat)/2-5):(length(C8Temp$VbgSat)/2),i+1]
  LinearModel <- lm(VbgSatExt~Idrootext)
  Vth[i] <- coef(LinearModel)[1]
  LinearModel <- lm(Idrootext~VbgSatExt)
  SlopeSat <- coef(LinearModel)[2]
  Usat[i] <- 2*SlopeSat^2*LWRatio/Capacity
}

tempof <- 1/Temp
Usatl <- log(Usat)
qplot(tempof,Usatl)

## Input all the temp related saturation datas
Output <- data.frame(Usat,Vth)
#write.csv(Output,file=paste("C8Sample4 C",ColNumber, sep = ""))