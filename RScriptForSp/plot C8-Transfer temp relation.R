library(ggplot2)
rm(list=ls())
#setwd("/Users/sunhuabin/Dropbox/Recently Data/C8/Bianwen")

setwd("/Users/sunhuabin/Dropbox/Recently Data/C8/Bianwen")
getwd

## Set the working path and clear the parameter

Temp <- seq(200,300,by=20)
ColNumber <- 2
ChannelLength <- 200
DeviceID <- paste("Sample2 ",Temp,"k ",ChannelLength,"u C",ColNumber,"  Saturation Transfer.txt",sep="") #批量组合文件名
SatTemp <- paste("C8Sat",Temp, sep = "")
IdTemp <- paste("ID",Temp,sep="")
for (i in 1:6){assign(SatTemp[i],read.table(DeviceID[i],header=T,sep= '\t'))}

Vbg <- C8Sat300$Vbg

for (i in 1:6) {
        TempID <- rep(paste(Temp[i],"k"),times=162)
        Id <- eval(parse(text=SatTemp[i]))$Id
        assign(IdTemp[i],data.frame(Vbg,Id,TempID))
        #colnames(eval(parse(IdTemp[i])))[2] <- "Id" 
          }

# C8T <- rbind(ID200[1:81,],ID220[1:81,],ID240[1:81,],ID260[1:81,],ID280[1:81,],ID300[1:81,])
C8T <- rbind(ID200,ID220,ID240,ID260,ID280,ID300)

ymaj <- c(1*10^-12,10^-10,1*10^-8,1*10^-6,1*10^-4)
xmaj <- c(-40,-30,-20,-10,0)

C8Pic <- qplot(Vbg,abs(Id),log=("y"),data=C8T,colour=TempID, geom="point",size = I(2),xlab=expression(paste("V"["g"]," (V)")), ylab=expression(paste("I"["d"]," (A)")))+ theme_set(theme_bw()) 
C8Pic +  geom_path(alpha=1,size=2) + scale_y_log10(limits=c(3*10^-12,1*10^-5),breaks= ymaj,labels=c(expression(paste("10"^"-12")),expression(paste("10"^"-10")),expression(paste("10"^"-8")),expression(paste("10"^"-6")),expression(paste("10"^"-4")))) + scale_x_continuous(limits=c(-30,0),breaks= xmaj,labels= xmaj) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=24), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(0.9,0.48)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))