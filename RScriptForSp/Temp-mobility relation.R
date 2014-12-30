library(ggplot2)
rm(list=ls())  #擦除所有变量
setwd("/Users/sunhuabin/Dropbox/Recently Data/C8/Bianwen")       #设置工作路径
getwd()
ColNumber <- 4
ChannelLength <- 200
Temp <- 200
DeviceID <- paste("Sample2 ",Temp,"k ",ChannelLength,"u C",ColNumber,"  Saturation Transfer.txt",sep="") #批量组合文件名
RawdataSat <- read.table(DeviceID,header=T,sep= '\t')
Capacity <- 3.4*10^-8 #3.4*10^-8 for PVDF-TrFE200nm /SiO2 50nm and 1.15*10^-8 for 300nm SiO2
LWRatio <- 0.2
## input the capacitance, Channel Width/Length Ratio and testing temperature here. Defult Valude capacity 300nm SiO2 F/cm2, W/L=1, Room temp=300k


TitleSat <- RawdataSat[1,]
GateVoltageSat <- RawdataSat[,1]

DrainCurrentSat <- RawdataSat[,2]
SourceCurrentSat <- RawdataSat[,6]
LeakageCurrentSat <- RawdataSat[,4]
DrainVoltageSat <- RawdataSat[4,5]
SqrtCurrentSat <- RawdataSat[,3]
SaturationFET <- data.frame(GateVoltageSat,DrainCurrentSat,SourceCurrentSat,LeakageCurrentSat,DrainVoltageSat,SqrtCurrentSat)
## saturation data input and set as a framework

GateVoltageSatExt <- GateVoltageSat[(length(GateVoltageSat)/2-5):(length(GateVoltageSat)/2)]
SqrtCurrentSatEXT <- SqrtCurrentSat[(length(SqrtCurrentSat)/2-5):(length(SqrtCurrentSat)/2)]
LinearModel <- lm(GateVoltageSatExt~SqrtCurrentSatEXT)
VthSat <- coef(LinearModel)[1]
LinearModel <- lm(SqrtCurrentSatEXT~GateVoltageSatExt)
qplot(GateVoltageSat,SqrtCurrentSat)
qplot(GateVoltageSatExt,SqrtCurrentSatEXT)
SlopeSat <- coef(LinearModel)[2]
uSat <- 2*SlopeSat^2*LWRatio/Capacity


#可以使用lapply，试一试...
#n<-1:4
#data<-lapply(n,read.table(paste("t",n,".txt",sep="")[[n]],header=T))



#数据结构相似时候可以用
#fname <- paste("pre",1:4011,".txt",sep="")
#datalist <- lapply(fname,read.delim,as.is=T)
#dataCombine <-do.call("rbind",datalist)

