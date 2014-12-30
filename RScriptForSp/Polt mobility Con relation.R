library(ggplot2)
rm(list=ls())
setwd("/Users/sunhuabin/Dropbox/Recently Data/Around Zero")

MobiConN <- read.csv("Mobi-Con N.csv")
MobiConP1 <- read.csv("Mobi-Con P.csv")
MobiConP2 <- read.csv("Mobi-Con P2.csv")

DeviceID <- rep("Without PMMA Buffering",times=258)
MobiN <- data.frame(MobiConN$ConN/(1.6*10^-19),MobiConN$Mobility,DeviceID)
colnames(MobiN)[1] <- "Con"
colnames(MobiN)[2] <- "Mobility"

DeviceID <- rep("With PMMA Buffering",times=258)
MobiP1 <- data.frame(MobiConP1$ConN/(1.6*10^-19),MobiConP1$Mobility,DeviceID)
colnames(MobiP1)[1] <- "Con"
colnames(MobiP1)[2] <- "Mobility"

DeviceID <- rep("Sample P2",times=258)
MobiP2 <- data.frame(MobiConP2$ConN/(1.6*10^-19),MobiConP2$Mobility,DeviceID)
colnames(MobiP2)[1] <- "Con"
colnames(MobiP2)[2] <- "Mobility"

MobiAll <- rbind(MobiN[1:80,],MobiP1[1:80,])
ymaj <- c(0.01,0.1,1,10)
xmaj <- c(10^9,10^10)

MobiN2 <- MobiN[100:200,]
MobiP2 <- MobiP1[100:200,]

lmA <- lm(log(MobiN2$Mobility) ~log(MobiN2$Con))
slopeA <- coef(lmA)[2]
lmB <- lm(log(MobiP2$Mobility) ~log(MobiP2$Con))
slopeB <- coef(lmB)[2]

IdPic <- qplot(Con,Mobility,data=MobiAll,colour=DeviceID, geom=("point"),size = I(3), xlab = "Effective electric field (V/m)",ylab = expression(paste(mu["FET"]," (", "cm"^"2","V"^"-1","s"^"-1",")")))+ theme_set(theme_bw()) 
IdPic + scale_x_log10( limits=c(9*10^8,1.5*10^10),breaks= xmaj,labels=c(expression(paste("10"^"9")),expression(paste("10"^"10"))))+scale_y_log10(limits=c(0.01,10), breaks= ymaj,labels=ymaj)+geom_path(alpha=0,size=3) + theme(axis.title.x=element_text(size=28), axis.title.y=element_text(size=28), plot.title=element_text(size=30), axis.text.x=element_text(size=24), axis.text.y=element_text(size=24), legend.text=element_text(size=20), legend.title=element_text(size=0)) + theme(legend.justification=c(1,0), legend.position=c(0.88,0)) + theme(panel.border = element_rect(size= 1, colour = "black")) + theme(panel.grid.major = element_line(size= NA, colour = NA))+theme(legend.key=theme_rect(colour="white",size=0.5,linetype="dashed"))

# scale_y_log10(limits=c(0.01,5),breaks= ymaj,labels=ymaj) +scale_x_log10()
