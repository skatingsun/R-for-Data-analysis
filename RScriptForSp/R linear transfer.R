rm(list=ls())  #擦除所有变量
library(ggplot2)
setwd("/Users/sunhuabin/Dropbox/Data/14-03 pen and C8 interface TLM and Temp/14-03-21/PENTACENE/Pen bianwen 1 and 2")
##设置工作目录和包

RawdataLin <- read.table("Sample1 200u 300k Linear Transfer single", header=TRUE, row.names=1, sep='\t')
## data input, using hand to choose the file path, F here means the first line is title

Title <- RawdataLin[2,]
GateVoltage <- RawdataLin[2:length(RawdataLin[,1])-1, 1]
DrainCurrent <- RawdataLin[2:length(RawdataLin[,1])-1, 2]
SourceCurrent <- RawdataLin[2:length(RawdataLin[,1])-1, 6]
LeakageCurrent <- RawdataLin[2:length(RawdataLin[,1])-1, 4]
DrainVoltage <- RawdataLin[2,5]
Transconductance <- RawdataLin[2:length(RawdataLin[,1])-1, 3]
LinearFET <- data.frame(GateVoltage,DrainCurrent,SourceCurrent,LeakageCurrent,DrainVoltage,Transcouductance)
## seperate the data into different vectors, such as: Voltage, Current etc.  x here means the whole data point number

Capacity <- 1.15*10^-8
LWRatio <- 2
Temp <- 300
Step <- mean(diff(GateVoltage))
## input the capacitance, Channel Width/Length Ratio and testing temperature here. Defult Valude capacity 300nm SiO2 F/cm2, W/L=1, Room temp=300k

ggplot(LinearFET)
##plot the current framework data via ggplot  

LinearMobility <- Transconductance*LWRatio/(Capacity*DrainVoltage)
ggplot(GateVoltage,LinearMobility)
##Extrat linear mobility-voltage relation and give the pic

MobilibyLin <- max(LinearMobility)

## Linear data pre-treatment

RawdataSat <- read.table("Sample1 200u 300k Saturation Transfer", header=TRUE, row.names=1, sep='\t')

TitleSat <- RawdataSat[2,]
GateVoltageSat <- RawdataSat[2:length(RawdataLin[,1])-1,1]
DrainCurrentSat <- RawdataSat[2:length(RawdataLin[,1])-1,2]
SourceCurrentSat <- RawdataSat[2:length(RawdataLin[,1])-1,6]
LeakageCurrentSat <- RawdataSat[2:length(RawdataLin[,1])-1,4]
DrainVoltageSat <- RawdataSat[2,5]
SqrtCurrentSat <- RawdataLin[2:length(RawdataLin[,1]-1,3]]
SaturationFET <- data.frame(GateVoltageSat,DrainCurrentSat,SourceCurrentSat,LeakageCurrentSat,DrainVoltageSat,SqrtCurrentSat)
## saturation data input and set as a framework

GateVoltageSatExt <- GateVoltageSat[length(GateVoltageSat)/2-5:length(GateVoltageSat)/2]
SqrtCurrentSatEXT <- SqrtCurrentSat[length(SqrtCurrentSat)/2-5:length(SqrtCurrentSat)/2]
LinearModel <- lm(SqrtCurrentSatEXT~GateVoltageSatExt)
SlopeSat <- coef(LinearModel)[2]
VthSat <- coef(LinearModel)[1]
uSat <- 2*SlopeSat^2*LWRatio/Capacity

## Using linear model to fit the sqrt(Id) and Gate Voltage with data in Saturation region. and get the slope and "On" Voltage of the device.


