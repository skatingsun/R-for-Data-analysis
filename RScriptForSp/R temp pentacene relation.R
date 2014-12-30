library(ggplot2)
rm(list=ls())

setwd("/Users/sunhuabin/Dropbox/Data/SiO2 and PVDF CV pingban/bianwen")
getwd()
Temp <- seq(210,300,by=10)
CVID <- paste("B ",Temp,"k.dat", sep = "")
Capacity <- seq(0,0,length=11)

for(i in 1:10){
        CapacityTemp <- seq(0,0,length =3 )
        CVTemp <- read.table(CVID[i],header = T , sep = ',')
        CapacityTemp <- mean(CVTemp$Cp[(length(CVTemp$Cp)/2-2):(length(CVTemp$Cp)/2)])
        Capacity[i] <- mean(CapacityTemp)*300/2.2 #单位为cm^2的归一化 
}




setwd("/Users/sunhuabin/Dropbox/Recently Data/PEN/Pen bianwen 1 and 2")
getwd()
## Set the working folder

ColNumber <- 3
ChannelLength <- 200
#Capacity <- 3.4*10^-8 #3.4*10^-8 for PVDF-TrFE200nm /SiO2 50nm and 1.15*10^-8 for 300nm SiO2
LWRatio <- 0.2 #0.2 for  200u channel
## Set the defacut value

Temp <- (21:30)*10
#Sample1 150u 190k  Saturation Transfer.txt
DeviceID <- paste("Sample2 ",ChannelLength,"u ",Temp,"k "," Saturation Transfer.txt",sep="") #批量组合文件名
SatTemp <- paste("PENSat",Temp, sep = "")
for (i in 1:10){assign(SatTemp[i],read.table(DeviceID[i],header=T,sep= '\t'))}


IDhalfTemp <- paste("IDroot",Temp, sep = "")
for (i in 1:10){assign(IDhalfTemp[i],eval(parse(text = SatTemp[i]))$IDEhalf)}
VbgSat <- eval(parse(text = SatTemp[1]))$Vbg

PENTemp <- data.frame(VbgSat,IDroot210,IDroot220,IDroot230,IDroot240,IDroot250,IDroot260,IDroot270,IDroot280,IDroot290,IDroot300)

Usat <- rep(0,times=10)
Vth <- rep(0,times=10)
Idrootext <- rep(0,times=6)

VbgSatExt <- PENTemp$VbgSat[(length(PENTemp$VbgSat)/2-5):(length(PENTemp$VbgSat)/2)]  
for (i in 1:10) {
  Idrootext <- PENTemp[(length(PENTemp$VbgSat)/2-5):(length(PENTemp$VbgSat)/2),i+1]
  LinearModel <- lm(VbgSatExt~Idrootext)
  Vth[i] <- coef(LinearModel)[1]
  Vn <- as.integer(-Vth[i]/0.5+20)
  VbgSatExt2 <- PENTemp$VbgSat[(Vn+17):(Vn+20)] # Vn+20 mean Vth + 10V ,the mobility at the Vg-Vth = 10V 
  Idrootext2 <- PENTemp[(Vn+17):(Vn+20),i+1]
  LinearModel2 <- lm(Idrootext2~VbgSatExt2)
  SlopeSat <- coef(LinearModel2)[2]
  Usat[i] <- 2*SlopeSat^2*LWRatio/Capacity[i]
}

tempof <- 1/Temp
Usatl <- log(Usat)
qplot(tempof,Usatl)


Output <- data.frame(Usat,Vth)
write.csv(Output,file=paste("PENSample2 corrected with Vth and Capacity W=",ChannelLength, sep = ""))


