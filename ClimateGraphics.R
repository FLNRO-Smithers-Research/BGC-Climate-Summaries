# Codes below pair focus BGC and adjacent BGC units, and runs VSURF and CART analysis.
# Codes also create a table of CART splits with all valid alternate variables and thresholds
# Ensure that you have working Dataset and BEC Pair Table in your working directory:
# When you start with a new dataset, make sure to follow "When You Have New Dataset" below first:


# Install the following R packages:
install.packages("caret")
install.packages("rattle")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("tcltk")
install.packages("plyr")
install.packages("reshape")
install.packages("reshape2")
install.packages("VSURF")
install.packages("reports")
install.packages("rpart.utils")
install.packages("rfUtilities")
install.packages ("ggplot2")
install.packages ("functional")
install.packages ("plot3D")
install.packages("dplyr")
install.packages ("lattice")
install.packages ("alphahull")
install.packages("C50")

# Load R packages:
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(tcltk)
library(plyr)
library(reshape)
library(reshape2)
library(VSURF)
library(reports)
library(rpart.utils)
require(randomForest)
require(rfUtilities)
library("parallel")
library("foreach")
library("doParallel")
require("ggplot2")
library(functional)
require(plot3D)
library(dplyr)
require(lattice)
require(alphahull)
require(C50)
######Clear environment and add in dataset############

rm(list=ls())
# The codes below need to be run only once when you start with a new dataset #
## Set working directory:
wd=tk_choose.dir()
setwd(wd)

## Load up previously created climate summary data
fname="BGC_rf_model20Apr2016"
load(paste(fname,".Rdata",sep=""))

###select file with normal data to predict (from SpittleHouses file)
Y1<-read.csv (file.choose()) ##
#######
x1 <- Y1$MAT_diff
y1 <- Y1$MAP_pcent
x1 <- Y1$PPT_wt_pcent
x1 <- Y1$PPT_sm_pcent
x1 <- Y1$Tave_wt_diff
x1 <- Y1$Tave_sm_diff
site <- factor(Y1$Location, levels= levels (factor(reorder(site, x1))))
gcm <- factor(Y1$GCM, levels= levels (factor(reorder(gcm, x1))))

stripplot(x1~site,  ylab = "Summer Temperature change", group = gcm, auto.key= T,  jitter.data = F, grid = "h", 
          type = c("p","a"), pch=3)

######################


stripplot(x1~gcm | site)
dotplot(gcm~x1)
dotplot(gcm~x1 | site)
xyplot(y1~x1)

###############Build Graph of MAT by MAP for subzone averages##########

#windowsFonts(Times= windowsFont("TT Times New Roman"))
##Create averages by SubZ###
#List=c("BGC", "CMD", "MCMT", "TD")
#X2=X1[,names(X1) %in% c(List)]

X2=X1
X2$BGC <- as.factor(X2$BGC)
X2$BGCZ <- as.factor(substr(X2$BGC, 1, 4))
#BGCmeans <- ddply(X2,~BGC,summarise,x=mean(MAP), y=mean(MWMT), z=mean(CMD))
BGCmeans <- ddply(X2,.(BGC),colwise(mean))
print (BGCmeans, digits=2)
write.csv(BGCmeans, file= "MeanclimateSubzones.csv")

#############added stated into CSV and re-read to use
BGCmeans2 =read.csv("MeanclimateSubzones2.csv",stringsAsFactors=FALSE,na.strings=".")
###########
BGCmeans2$x = BGCmeans2$MAP
BGCmeans2$y <- BGCmeans2$MWMT
BGCmeans2$z <- BGCmeans2$CMD.total
#########remove extremes
#####MAP >1200 = Coast
xval <- 4000
BGCmeans2 <- subset(BGCmeans2, BGCmeans2$x <xval)
##########if Y = MWMT the yvalmin <10.5 = tundra###
yvalmax <- 20
yvalmin <- 10.5
yvalmin <- -20
BGCmeans2 <- subset(BGCmeans2, BGCmeans2$y <yvalmax)
BGCmeans2 <- subset(BGCmeans2, BGCmeans2$y >yvalmin)
####CMDTotal >850 = grassland/shrubland
zvalmax <- 850
BGCmeans2 <- subset(BGCmeans2, BGCmeans2$z >zvalmin)
BGCmeans2 <- subset(BGCmeans2, BGCmeans2$z <zvalmax)
BGCmeans2$BGCZ <- as.factor(substr(BGCmeans2$BGC, 1,   4))

text3D (BGCmeans2$x, BGCmeans2$y, BGCmeans2$z, theta =30, phi = 0 , add=FALSE, labels = BGCmeans2$BGC, cex = .5,
        xlab = "x=MAP", ylab = "y=MWMT", 
        zlab = "z=CMD.total", pch=16, ticktype = "detailed", colvar = BGCmeans2$Physiog)

#qplot (x= x, y= y, data=BGCmeans, geom="point")
ggplot(BGCmeans, aes(x=x, y=y, group=BGCZ))+
  geom_text(aes(label=BGC), cex=2)
          


               )
