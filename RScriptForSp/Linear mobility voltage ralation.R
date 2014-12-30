library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Transfer and output")
getwd

## Working path setting

# FeFETN <- read.table("13-03-01 N 300u 01 Transistor 2 30 8.txt", header=T, sep= '\t', skip=2)
#FeFETP <- read.table("13-03-01 P1 300u 01 Transistor 2 30.txt", header=T, sep= '\t', skip=2)
FeFETN <- read.table("13-03-01 P2 300u 01 Transistor 2 30.txt", header=T, sep= '\t', skip=2)
# FeFETP3 <- read.table("13-03-01 P3 300u 01 Transistor 2 30.txt", header=T, sep= '\t', skip=2)


TransferDataN <- data.frame(FeFETN$Vg,FeFETN$I_D,FeFETN$gm,FeFETN$Ig)
colnames(TransferDataN)[1] <- "Vg"
colnames(TransferDataN)[2] <- "Id"
colnames(TransferDataN)[3] <- "gm"
colnames(TransferDataN)[4] <- "Ig"

# TransferDataP <- data.frame(FeFETP$Vg,FeFETP$I_D,FeFETP$gm,FeFETP$Ig)
# colnames(TransferDataP)[1] <- "Vg"
# colnames(TransferDataP)[2] <- "Id"
# colnames(TransferDataP)[3] <- "gm"
# colnames(TransferDataP)[4] <- "Ig"
# 
# TransferDataP2 <- data.frame(FeFETP2$Vg,FeFETP2$I_D,FeFETP2$gm,FeFETP2$Ig)
# colnames(TransferDataP2)[1] <- "Vg"
# colnames(TransferDataP2)[2] <- "Id"
# colnames(TransferDataP2)[3] <- "gm"
# colnames(TransferDataP2)[4] <- "Ig"
# 
# TransferDataP3 <- data.frame(FeFETP3$Vg,FeFETP3$I_D,FeFETP3$gm,FeFETP3$Ig)
# colnames(TransferDataP3)[1] <- "Vg"
# colnames(TransferDataP3)[2] <- "Id"
# colnames(TransferDataP3)[3] <- "gm"
# colnames(TransferDataP3)[4] <- "Ig"
# 
# DeviceData <- rbind(TransferDataN,TransferDataP,TransferDataP2,TransferDataP3)
#Current data import and reshaped



# setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/N")
# getwd
# CVDataN <- read.table("300u30V.dat", header = T, sep=",")# every frequency step have 102 points of data
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P2")
getwd
CVDataN <- read.table("300u30v.dat", header = T, sep=",")
# setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P2")
# getwd
# CVDataP2 <- read.table("100u30v.dat", header = T, sep=",")
# setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P3")
# getwd
# CVDataP3 <- read.table("100u30v.dat", header = T, sep=",")

CVDataN20 <- CVDataN[1:102,]
CVDataN20 <- data.frame(CVDataN20)


# CVDataP20 <- CVDataP[1:102,]
# DeviceIdP <- rep("Sample P1",times=102)
# CVDataP20 <- data.frame(CVDataP20,DeviceIdP)
# colnames(CVDataP20)[6] <- "DeviceID"
# 
# CVDataP220 <- CVDataP2[1:102,]
# DeviceIdP2 <- rep("Sample P2",times=102)
# CVDataP220 <- data.frame(CVDataP220,DeviceIdP2)
# colnames(CVDataP220)[6] <- "DeviceID"
# 
# CVDataP320 <- CVDataP3[1:102,]
# DeviceIdP3 <- rep("Sample P3",times=102)
# CVDataP320 <- data.frame(CVDataP320,DeviceIdP3)
# colnames(CVDataP320)[6] <- "DeviceID"

# get the data of freq=20Hz

#由于CV和IV的测试间隔不同，分别对两个进行数组扩充使得每个Step的间隔变为Vsetp=0.25V

Nallgo <- matrix(0,nrow=401,ncol=4)
Nallback <- matrix(0,nrow=401,ncol=4)
out <- rep(0,times=4)
outback <- rep(0,times=4)
#填充IV曲线的过程
for(i in 1:80){
        for(j in 1:5){
out <- TransferDataN[i,]*(5 - j)/5+TransferDataN[i+1,]*j/5   
outback <- TransferDataN[81+i,]*(5-j)/5+TransferDataN[i+1+81,]*j/5
Nallgo[(5*(i-1)+j+1),] <- t(out)
Nallback[5*(i-1)+j+1,] <- t(outback)
        }      
}

Nallgo[1,] <- t(TransferDataN[1,])
Nallback[1,] <- t(TransferDataN[82,])

as.numeric(Nallgo,length=5)
as.numeric(Nallback,length=5)
Nallgo <- data.frame(Nallgo)
Nallback <- data.frame(Nallback)
colnames(Nallgo)[1] <- "Vg"
colnames(Nallgo)[2] <- "Id"
colnames(Nallgo)[3] <- "gm"
colnames(Nallgo)[4] <- "Ig"
colnames(Nallback)[1] <- "Vg"
colnames(Nallback)[2] <- "Id"
colnames(Nallback)[3] <- "gm"
colnames(Nallback)[4] <- "Ig"


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

NCVGo$Cp <- NCVGo$Cp - mean(NCVGo$Cp[0:60])
NCVBack$Cp <- NCVBack$Cp - mean(NCVBack$Cp[360:400])
#对电容做归一化为cm单位， *600/2.2
LW <- 0.3
Vds <- 2
Ulingo <- Nallgo$gm*LW/(Vds*NCVGo$Cp)*2.2/600 
Vg <- Nallgo$Vg
MobiN <- data.frame(Vg,Ulingo)

Ulinback <- Nallback$gm*LW/(Vds*NCVBack$Cp)*2.2/600 
Vg <- Nallback$Vg
MobiNB <- data.frame(Vg,Ulinback)

#输出迁移率数据
DeviceID <- rep("Go",times=401)
MobiN <- data.frame(MobiN,DeviceID)
colnames(MobiN)[2] <- "Mobility"
DeviceID <- rep("Back",times=401)
MobiNB <- data.frame(MobiNB,DeviceID)
colnames(MobiNB)[2] <- "Mobility"
Mobi <- rbind(MobiN,MobiNB)
setwd("/Users/sunhuabin/Dropbox/Recently Data/Around Zero")
#write.csv(Mobi,file="P2-All mobility FeFET.csv")



#绘出Vbg=0V附近的载流子迁移率和电场的关系。
# MobiNBzeroleft <- MobiNB[177:200,]
# MobiNBzeroright <- MobiNB[202:225,]
# DeviceID <- rep("Right",times=24)
# Mright <- data.frame(MobiNBzeroright,DeviceID)
# DeviceID <- rep("Left",times=24)
# MobiNBzeroleft$Vg <- abs(MobiNBzeroleft$Vg)
# Mleft <- data.frame(MobiNBzeroleft,DeviceID)
# Mallzero <- rbind(Mright,Mleft)
# 
# IdPic <- qplot(Vg,Ulinback,log="xy",data=Mallzero,colour=DeviceID, geom=("point"),size = I(5), main = "Mobility Gate Bias relation around Vg=0V", xlab = "Vbg", ylab = "Mobility")+ theme_set(theme_bw()) 
# IdPic +  stat_smooth(method = "auto", size = 2)+ theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )

#IdPic <- qplot(Vg,gm,log="y",data=DeviceData,colour=DeviceID, geom=("point"),size = I(3), main = "FeFET Transfer", xlab = "Vg (V)", ylab = "TransConductance")+ theme_set(theme_bw()) 

#IdPic <- qplot(Vg,Id,log=("y"),data=DeviceData,colour=DeviceID, geom=("point"),size = I(3), main = "FeFET Transfer", xlab = "Vg (V)", ylab = "Id (A)")+ theme_set(theme_bw()) 
#IdPic + geom_path(alpha=0.7,size=2) + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )

#leakage current plot

# setwd("/Users/sunhuabin/Dropbox/Recently Data/Around Zero")
# write.csv(Mallzero,file="N.csv")

#IdPic <- qplot(Vg,Ig,data=DeviceData,colour=DeviceID, geom=("point"),size = I(2), main = "FeFET Leakage current", xlab = "Vg (V)", ylab = "Id (A)")+ theme_set(theme_bw()) 
# IdPic + geom_path(alpha=0.5,size=2)+ theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )