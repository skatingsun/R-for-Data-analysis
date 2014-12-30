## Include the Vth shift in the mobility caculation, Set Mobility calculation area Vg-Vth = conditionMessage.condition. Capacity - temp change also take cared
library(ggplot2)
rm(list=ls())

setwd("/Users/sunhuabin/Dropbox/Data/SiO2 and PVDF CV pingban/bianwen")
getwd()
Temp <- seq(200,300,by=10)
CVID <- paste("D ",Temp,"k.dat", sep = "")
Capacity <- seq(0,0,length=11)

for(i in 1:11){
  CapacityTemp <- seq(0,0,length =3 )
  CVTemp <- read.table(CVID[i],header = T , sep = ',')
  CapacityTemp <- mean(CVTemp$Cp[(length(CVTemp$Cp)/2-2):(length(CVTemp$Cp)/2)])
  Capacity[i] <- mean(CapacityTemp)*300/2.2 #单位为cm^2的归一化 
}


## CV-Temp Data reading and analysis 1~11 means temp 200~300k, A means No PMMA, B~D mean 0.1 , 0.2 , 0.5%wt PMMA 



setwd("/Users/sunhuabin/Dropbox/Recently Data/C8/Bianwen")
getwd()
## Set the working folder

ColNumber <- 4
ChannelLength <- 200
Temp <- 200
#Capacity <- 3.4*10^-8 #3.4*10^-8 for PVDF-TrFE200nm /SiO2 50nm and 1.15*10^-8 for 300nm SiO2
LWRatio <- 0.2 #0.2 for  200u channel
## Set the defacut value



Temp <- seq(200,300, by=10)
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




for (i in 1:11) {
  VbgSatExt <-C8Temp$VbgSat[(length(C8Temp$VbgSat)/2-3):(length(C8Temp$VbgSat)/2)]  
  Idrootext <- C8Temp[(length(C8Temp$VbgSat)/2-3):(length(C8Temp$VbgSat)/2),i+1]
  LinearModel <- lm(VbgSatExt~Idrootext)
  Vth[i] <- coef(LinearModel)[1]
  Vn <- as.integer(-Vth[i]/0.5) #Get the Vth Data subset number
  VbgSatExt2 <- C8Temp$VbgSat[(Vn+17):(Vn+20)] # Vn+20 mean Vth + 10V ,the mobility at the Vg-Vth = 10V 
  Idrootext2 <- C8Temp[(Vn+17):(Vn+20),i+1]
  LinearModel2 <- lm(Idrootext2~VbgSatExt2)
  SlopeSat <- coef(LinearModel2)[2]
  Usat[i] <- 2*SlopeSat^2*LWRatio/Capacity[i]
}

tempof <- 1/Temp
Usatl <- log(Usat)

#plot the data and linear fit it.

Pic1 <- qplot(tempof,Usatl,size=I(3),geom = c("point"),  main = "Temp-mobility relationship",xlab = ("Temp^-1"))

Pic1 + theme_bw() + theme(plot.title = element_text(size = 30), axis.text.x = element_text(size = 20) , axis.text.y = element_text(size = 10), axis.title.x = element_text(size=20))

## Input all the temp related saturation datas
Output <- data.frame(Usat,Vth)
write.csv(Output,file=paste("C8Sample4 C",ColNumber, " Capacity and Vth correction", sep = ""))