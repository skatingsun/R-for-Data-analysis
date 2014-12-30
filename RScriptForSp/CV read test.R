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
  Capacity[i] <- mean(CapacityTemp) 
}

CVTEMP <- data.frame(Temp,Capacity)
GraCV <- ggplot(CVTEMP,aes(Temp,Capacity))
GraCV <- GraCV + geom_point(size=4)+geom_smooth(size=3)
GraCV

#aa <- read.table(CVID[1],header=T,sep= ',')
