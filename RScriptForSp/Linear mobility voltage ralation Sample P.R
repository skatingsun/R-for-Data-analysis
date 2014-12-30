library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Transfer and output")
getwd

## Working path setting

#FeFETN <- read.table("13-03-01 N 300u 01 Transistor 2 30 8.txt", header=T, sep= '\t', skip=2)
 FeFETP <- read.table("13-03-01 P1 300u 01 Transistor 2 30.txt", header=T, sep= '\t', skip=2)
# FeFETP2 <- read.table("13-03-01 P2 300u 01 Transistor 2 30.txt", header=T, sep= '\t', skip=2)
# FeFETP3 <- read.table("13-03-01 P3 300u 01 Transistor 2 30.txt", header=T, sep= '\t', skip=2)


# TransferDataN <- data.frame(FeFETN$Vg,FeFETN$I_D,FeFETN$gm,FeFETN$Ig)
# colnames(TransferDataN)[1] <- "Vg"
# colnames(TransferDataN)[2] <- "Id"
# colnames(TransferDataN)[3] <- "gm"
# colnames(TransferDataN)[4] <- "Ig"

TransferDataP <- data.frame(FeFETP$Vg,FeFETP$I_D,FeFETP$gm,FeFETP$Ig)
colnames(TransferDataP)[1] <- "Vg"
colnames(TransferDataP)[2] <- "Id"
colnames(TransferDataP)[3] <- "gm"
colnames(TransferDataP)[4] <- "Ig"
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
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P1")
getwd
CVDataP <- read.table("100u30v.dat", header = T, sep=",")
# setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P2")
# getwd
# CVDataP <- read.table("100u30v.dat", header = T, sep=",")
# setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Interface effect C-V/P3")
# getwd
# CVDataP3 <- read.table("100u30v.dat", header = T, sep=",")
# 
# CVDataN20 <- CVDataN[1:102,]
# CVDataN20 <- data.frame(CVDataN20)


CVDataP20 <- CVDataP[1:102,]
#DeviceIdP <- rep("Sample P1",times=102)
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

Pallgo <- matrix(0,nrow=401,ncol=4)
Pallback <- matrix(0,nrow=401,ncol=4)
out <- rep(0,times=4)
outback <- rep(0,times=4)
#填充IV曲线的过程
for(i in 1:80){
        for(j in 1:5){
                out <- TransferDataP[i,]*(5 - j)/5+TransferDataP[i+1,]*j/5   
                outback <- TransferDataP[81+i,]*(5-j)/5+TransferDataP[i+1+81,]*j/5
                Pallgo[(5*(i-1)+j+1),] <- t(out)
                Pallback[5*(i-1)+j+1,] <- t(outback)
        }      
}

Pallgo[1,] <- t(TransferDataP[1,])
Pallback[1,] <- t(TransferDataP[82,])

as.numeric(Pallgo,length=5)
as.numeric(Pallback,length=5)
Pallgo <- data.frame(Pallgo)
Pallback <- data.frame(Pallback)
colnames(Pallgo)[1] <- "Vg"
colnames(Pallgo)[2] <- "Id"
colnames(Pallgo)[3] <- "gm"
colnames(Pallgo)[4] <- "Ig"
colnames(Pallback)[1] <- "Vg"
colnames(Pallback)[2] <- "Id"
colnames(Pallback)[3] <- "gm"
colnames(Pallback)[4] <- "Ig"


PCVGo <- matrix(0,nrow=401,ncol=5)
PCVBack <- matrix(0,nrow=401,ncol=5)
out <- rep(0,times=5)
outback <- rep(0,times=5)
#填充IV曲线的过程
for(i in 1:50){
        for(j in 1:8){
                out <- CVDataP20[i,]*(8 - j)/8+CVDataP20[i+1,]*j/8
                outback <- CVDataP20[51+i,]*(8-j)/8+CVDataP20[i+51+1,]*j/8
                PCVGo[(8*(i-1)+j+1),] <- t(out)
                PCVBack[(8*(i-1)+j+1),] <- t(outback)
        }      
}
PCVGo[1,] <- t(CVDataP20[1,])
PCVBack[1,] <- t(CVDataP20[52,])
as.numeric(PCVGo,length=5)
as.numeric(PCVBack,length=5)
PCVGo <- data.frame(PCVGo)
PCVBack <- data.frame(PCVBack)
colnames(PCVGo)[1] <- "Vg"
colnames(PCVGo)[2] <- "Freq"
colnames(PCVGo)[3] <- "Cp"
colnames(PCVGo)[4] <- "G"
colnames(PCVBack)[1] <- "Vg"
colnames(PCVBack)[2] <- "Freq"
colnames(PCVBack)[3] <- "Cp"
colnames(PCVBack)[4] <- "G"

PCVGo$Cp <- PCVGo$Cp - mean(PCVGo$Cp[0:60])
PCVBack$Cp <- PCVBack$Cp - mean(PCVBack$Cp[360:400])
#对电容做归一化为cm单位， *600/2.2
LW <- 0.3
Vds <- 2
Ulingo <- Pallgo$gm*LW/(Vds*PCVGo$Cp)*2.2/600 
Vg <- Pallgo$Vg
MobiP <- data.frame(Vg,Ulingo)

Ulinback <- Pallback$gm*LW/(Vds*PCVBack$Cp)*2.2/600 
Vg <- Pallback$Vg
MobiPB <- data.frame(Vg,Ulinback)




#绘出Vbg=0V附近的载流子迁移率和电场的关系。
MobiPBzeroleft <- MobiPB[186:200,]
MobiPBzeroright <- MobiPB[202:216,]
DeviceID <- rep("Right",times=15)
Mright <- data.frame(MobiPBzeroright,DeviceID)
DeviceID <- rep("Left",times=15)
MobiPBzeroleft$Vg <- abs(MobiPBzeroleft$Vg)
Mleft <- data.frame(MobiPBzeroleft,DeviceID)
Mallzero <- rbind(Mright,Mleft)


IdPic <- qplot(Vg,Ulinback,log="xy",data=Mallzero,colour=DeviceID, geom=("point"),size = I(5), main = "Mobility Gate Bias relation around Vg=0V", xlab = "Vbg", ylab = "Mobility")+ theme_set(theme_bw()) 
IdPic +  stat_smooth(method = "auto", size = 2)+ theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )

 setwd("/Users/sunhuabin/Dropbox/Recently Data/Around Zero")
 write.csv(Mallzero,file="P.csv")



#IdPic <- qplot(Vg,gm,log="y",data=DeviceData,colour=DeviceID, geom=("point"),size = I(3), main = "FeFET Transfer", xlab = "Vg (V)", ylab = "TransConductance")+ theme_set(theme_bw()) 

#IdPic <- qplot(Vg,Id,log=("y"),data=DeviceData,colour=DeviceID, geom=("point"),size = I(3), main = "FeFET Transfer", xlab = "Vg (V)", ylab = "Id (A)")+ theme_set(theme_bw()) 
#IdPic + geom_path(alpha=0.7,size=2) + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )

#leakage current plot



#IdPic <- qplot(Vg,Ig,data=DeviceData,colour=DeviceID, geom=("point"),size = I(2), main = "FeFET Leakage current", xlab = "Vg (V)", ylab = "Id (A)")+ theme_set(theme_bw()) 
# IdPic + geom_path(alpha=0.5,size=2)+ theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=30),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )