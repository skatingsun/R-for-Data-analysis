library(ggplot2)
library(baseline)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/retention")
getwd

RetentionN <- read.table("N 100um and I_V-t Sampling write stay.txt", header = T, sep="\t" ,skip=2)
RetentionNoff <- read.table("N 100um and I_V-t Sampling erase stay.txt", header = T, sep="\t" ,skip=2)

RetentionP <- read.table("P1 100u 2 and I_V-t Sampling write stay.txt", header = T, sep="\t" ,skip=2)
RetentionPoff <- read.table("P1 100u 2 and I_V-t Sampling erase stay.txt", header = T, sep="\t" ,skip=2)

DeviceIDNON <- rep("Withtout PMMA On", times= 3999)
RetentionNON <- data.frame(RetentionN,DeviceIDNON)
names(RetentionNON) <- c("Times","Id","DeviceID")


DeviceIDNOFF <- rep("Withtout PMMA Off", times= 3999)
RetentionNOFF <- data.frame(RetentionNoff,DeviceIDNOFF)
names(RetentionNOFF) <- c("Times","Id","DeviceID")


DeviceIDPON <- rep("With PMMA On", times= 3999)
RetentionPON <- data.frame(RetentionP,DeviceIDPON)
names(RetentionPON) <- c("Times","Id","DeviceID")


DeviceIDPOFF <- rep("With PMMA Off", times= 3999)
RetentionPOFF <- data.frame(RetentionPoff,DeviceIDPOFF)
names(RetentionPOFF) <- c("Times","Id","DeviceID")


RetentionAll <- rbind(RetentionNON[61:1000,],RetentionNOFF[50:1000,],RetentionPON[1:1000,],RetentionPOFF[1:1000,])

# 
# DeviceIdN <- rep("Device without PMMA Buffering",times=3999)
# RetentionN1 <- data.frame(RetentionN,RetentionNoff[,2],DeviceIdN)
# colnames(RetentionN)[1] <- "Time"
# colnames(RetentionN)[2] <- "Id"
# colnames(RetentionN)[3] <- "Idoff"
# colnames(RetentionN)[4] <- "DeviceID"
# 
# 
# DeviceIdP <- rep("Device with PMMA Buffering",times=3999)
# RetentionP1 <- data.frame(RetentionP,RetentionPoff[,2],DeviceIdP)
# colnames(RetentionP)[1] <- "Time"
# colnames(RetentionP)[2] <- "Id"
# colnames(RetentionP)[3] <- "Idoff"
# colnames(RetentionP)[4] <- "DeviceID"
# 
# RetentionData1 <- rbind(RetentionN[61:1000,],RetentionP[1:1000,])
ymaj <- c(10^-11,10^-9,10^-7,10^-5)

IdPic <- qplot(Times-60,abs(Id),log=("xy"),data=RetentionAll,colour=DeviceID, geom=("point"),size = I(1), xlab = "Time (s)", ylab = expression(paste("I"["d"]," (A)")))+ theme_set(theme_bw()) 

IdPic <- IdPic + geom_path(alpha=1,size=3)
# with different line typeIdPic <- IdPic + geom_path(alpha=1,size=3,aes(linetype=DeviceID)) 

IdPic <- IdPic+ scale_y_log10(limits=c(10^-11,10^-5),breaks= ymaj,labels=c(expression(paste("10"^"-11")),expression(paste("10"^"-9")),expression(paste("10"^"-7")),expression(paste("10"^"-5"))))
#set the text size in the pictures, ** here we set the x title to font Times and Bold
IdPic <- IdPic+theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=24), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=20), legend.title=element_text(size=0))
#set the legend position in the picture none-> no legend
IdPic <- IdPic+ theme(legend.justification=c(1,0), legend.position=c(0.5,0.4))
#Draw a blank panel border
IdPic <-  IdPic+ theme(panel.border = element_rect(size= 1, colour = "black")) 
#remove the legend background
IdPic <- IdPic+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))
#remove grid lines
IdPic <- IdPic + theme(panel.background=element_rect(fill='transparent', color='grey'),panel.grid=element_blank())

IdPic

setwd("/Users/sunhuabin/Dropbox/")
write.csv(RetentionN,file = "RetentionN")
write.csv(RetentionP,file = "RetentionP")