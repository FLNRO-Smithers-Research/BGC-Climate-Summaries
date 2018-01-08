# Walter and Leif diagram

rm(list=ls())
library(climatol)

# choose climate summary file
fname=(file.choose())

# set that directory as working directory
setwd(dirname(fname))

# read in csv file
x=read.csv(fname,as.is=T)

# select BGC unit(s) and time period (only one time period at a time for now)
bgcList=select.list(unique(x$BGC),multiple=T,title="Select BGC units")
periodList=select.list(unique(x$period),preselect="1961-1990",multiple=F,title="Select time period")


diagwl.prep=function(bgc,period) {

	x1=subset(x,x$BGC==bgc&x$period==Period)
	tmx=subset(x1,x1$Var=="mean")[c(paste("Tmax0",1:9,sep=""),paste("Tmax",10:12,sep=""))]	
	tmn=subset(x1,x1$Var=="mean")[c(paste("Tmin0",1:9,sep=""),paste("Tmin",10:12,sep=""))]	
	ppt=subset(x1,x1$Var=="mean")[c(paste("PPT0",1:9,sep=""),paste("PPT",10:12,sep=""))]	
	tmn2=subset(x1,x1$Var=="mean")[c(paste("DD_0_0",1:9,sep=""),paste("DD_0_",10:12,sep=""))]	
	tmn2[which(tmn2>0)]=-16
	tmn2[which(tmn2==0)]=15
	
	
	return(rbind(as.matrix(ppt),as.matrix(tmx),
	as.matrix(tmn),as.matrix(tmn2)))
	
	
	
	}

jpeg(file="walter.jpeg", width = 960, height = 960)
	
	for (k in 1:length(periodList)) {
	
		for (j in 1:length(bgcList)) {
	
	
			bgc=bgcList[j]
			Period=periodList[k]
			diagwl(diagwl.prep(bgc,Period),mlab="en",p3line=F,est=bgc, per=Period)
		}
	}
	
	
	dev.off()
dev.off()	

