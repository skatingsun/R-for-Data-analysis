library(ggplot2)
library(grid)
rm(list=ls())

setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Transfer and output") # set the working path

# For example, imput a 150u N sample work @ Vd=-2V
DataN <- read.table("13-03-01 N 50u 01 Transistor 2 30.txt", header= T, sep = "\t",skip = 2)

Device <- rep("Without PMMA",times=81)
DataNBack <- DataN[82:162,] 
DataNBack <- cbind(DataNBack,Device)

setwd("/Users/sunhuabin/Dropbox/Data/Electric Data/interlayer effect/Transfer and output") 
DataP <- read.table("13-03-01 P1 150u 01 Transistor 2 30.txt", header= T, sep = "\t",skip = 2)

Device <- rep("With PMMA",times=81)
DataPBack <- DataP[82:162,] 
DataPBack <- cbind(DataPBack,Device)
DataAll <- rbind(DataNBack,DataPBack)

ymaj <- c(10^-10,10^-8,10^-6,10^-4)

IdPic <- qplot(Vg ,I_D ,data=DataAll, colour= Device,shape=Device , geom=("point"),size = I(4), main = "", xlab = "Gate Voltage (V)", ylab = "Drain Current (A)")+ theme_set(theme_bw()) + geom_line()

IdPic <- IdPic + theme(axis.title.x = element_text(size=20), axis.title.y = element_text(size=20), plot.title = element_text(size=25),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text =element_text(size=15), legend.title=element_text(size=20) )

IdPic <- IdPic+ scale_y_log10(breaks= ymaj,labels=c(expression(paste("10"^"-10")),expression(paste("10"^"-8")),expression(paste("10"^"-6")),expression(paste("10"^"-4"))))

IdPic <- IdPic+ theme(legend.justification=c(1,0), legend.position=c(0.5,0.5))
#Draw a blank panel border
IdPic <-  IdPic+ theme(panel.border = element_rect(size= 1, colour = "black")) 
#remove the legend background
IdPic <- IdPic+theme(legend.key=element_rect(colour="white",size=0.5,linetype="dashed"))
#remove grid lines
IdPic <- IdPic + theme(panel.background=element_rect(fill='transparent', color='grey'),panel.grid=element_blank())

#Tick marks inside
IdPic <- IdPic + theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))
#Note that the value should be passed as a unit object, which requires that the grid package (pre-installed with R) is loaded.

IdPic
