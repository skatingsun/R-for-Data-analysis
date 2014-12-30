library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Recently Data/C8/Bianwen")
getwd()

SatData <- read.table("Sample3 300k 200u C2  Saturation Transfer.txt",header=T,sep="\t")

#Read and organize the Data

SatLim <- lm(SatData$Vbg~SatData$IDEhalf)
Vth <- coef(SatLim)[1]
Idlog <- log10