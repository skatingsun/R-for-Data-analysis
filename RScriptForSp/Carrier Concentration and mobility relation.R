#Carrier concentration and mobility relating @ Linear region
library(ggplot2)
rm(list=ls())
# setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/N")
# getwd
# CVDataN <- read.table("300u30V.dat", header = T, sep=",")# every frequency step have 102 points of data
# setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P1")
# getwd
# CVDataP <- read.table("300u30v.dat", header = T, sep=",")
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P2")
getwd
CVDataN <- read.table("100u30v.dat", header = T, sep=",")
# setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P3")
# getwd
# CVDataP3 <- read.table("100u30v.dat", header = T, sep=",")
CVDataN20 <- CVDataN[1:102,]
CVDataN20 <- data.frame(CVDataN20)
#import the CV data

NCVGo <- matrix(0,nrow=401,ncol=5)
NCVBack <- matrix(0,nrow=401,ncol=5)
out <- rep(0,times=5)
outback <- rep(0,times=5)
#填充IV曲线的过程
for(i in 1:50){
        for(j in 1:8){
                out <- CVDataN20[i,]*(8 - j)/8+CVDataN20[i+1,]*j/8
                outback <- CVDataN20[51+i,]*(8-j)/8+CVDataN20[i+51+1,]*j/8
                NCVGo[(8*(i-1)+j+1),] <- t(out)
                NCVBack[(8*(i-1)+j+1),] <- t(outback)
        }      
}
NCVGo[1,] <- t(CVDataN20[1,])
NCVBack[1,] <- t(CVDataN20[52,])
as.numeric(NCVGo,length=5)
as.numeric(NCVBack,length=5)
NCVGo <- data.frame(NCVGo)
NCVBack <- data.frame(NCVBack)
colnames(NCVGo)[1] <- "Vg"
colnames(NCVGo)[2] <- "Freq"
colnames(NCVGo)[3] <- "Cp"
colnames(NCVGo)[4] <- "G"
colnames(NCVBack)[1] <- "Vg"
colnames(NCVBack)[2] <- "Freq"
colnames(NCVBack)[3] <- "Cp"
colnames(NCVBack)[4] <- "G"

NCVGo$Cp <- NCVGo$Cp - mean(NCVGo$Cp[1:60])
NCVBack$Cp <- NCVBack$Cp - mean(NCVBack$Cp[360:400])
NCVBack <- NCVBack[order(NCVBack[,1],decreasing=T),]
#Reshape the CV data, step change from 1.2V/point -> 0.15V/point

ConN <- rep(0,times=401)
ConNBack <- rep(0,times=401)
for(i in 1:401){
        ConN[i] <- sum(NCVGo$Cp[1:i]*0.15)
        ConNBack[i] <- sum(NCVBack$Cp[1:i]*0.15) 
}

NCVGo <- data.frame(NCVGo,ConN)
NCVBack <- data.frame(NCVBack,ConNBack)

#Mobility read in
setwd("/Users/sunhuabin/Dropbox/Recently Data/Around Zero")
MobilityAll <- read.csv("P2-All mobility FeFET.csv")
MobilityGo <- MobilityAll[1:401,]
MobilityBack <- MobilityAll[402:802,]
MobilityBack <- MobilityBack[order(MobilityBack[,2],decreasing=T),]

ConMobiGo <- data.frame(NCVGo$Vg,NCVGo$ConN,MobilityGo$Mobility)
colnames(ConMobiGo)[1] <- "Vg"
colnames(ConMobiGo)[2] <- "ConN"
colnames(ConMobiGo)[3] <- "Mobility"
ConMobiBack <- data.frame(NCVBack$Vg,NCVBack$ConN,MobilityBack$Mobility)
colnames(ConMobiBack)[1] <- "Vg"
colnames(ConMobiBack)[2] <- "ConN"
colnames(ConMobiBack)[3] <- "Mobility"


ConMobiGo <- ConMobiGo[216:401,]
ConMobiBack <- ConMobiBack[144:401,]
 IdPic <- qplot(ConN,Mobility,data=ConMobiBack,log="xy", geom=("point"),size = I(6), main = "Carrier Con & Mobility Relation", xlab = "Carrier Concentration", ylab = "Mobility")+ theme_set(theme_bw()) 
 IdPic +   theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )

write.csv(ConMobiBack,file="Mobi-Con P2")

# IdPic <- qplot(Vg,ConN,data=NCVGo, geom=("point"),size = I(6), main = "Carrier Con & Vg Relation", xlab = "Vbg", ylab = "Carrier Con @ L300u*W1000u channel")+ theme_set(theme_bw()) 
# IdPic +   theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )

# 
# IdPic <- qplot(Vg,ConNBack,data=NCVBack,geom=("point"),size = I(6), main = "Carrier Con & Vg Relation", xlab = "Vbg", ylab = "Carrier Con @ L300u*W1000u channel")+ theme_set(theme_bw()) 
#  IdPic +   theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )
