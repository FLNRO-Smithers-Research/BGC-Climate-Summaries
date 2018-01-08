# BGC annual data dot plots
# 
# Script will create dot plots for means of annual climate variables for BGC units
# Input for this script will be the BGC Summary file created previously

# clean up workspace
rm(list=ls())
require(doBy)
		
# set file name - what do you want to call the BGC Summary file?
fname="BGC_dotplots"
 
# read in climate data
# user chooses file interactively
# choose the .csv file produced from BGCClimateSummaries.R
a=file.choose()
bgcData=read.csv(a,as.is=TRUE,stringsAsFactors=FALSE)
setwd(gsub(basename(a),"",a))

{ # If the individual BGC units summary file is selected, select the period to create the dot plots
# note: if you select the Paired BGC units summary, no period is selected (already is 1961-1990)
	if(length(bgcData$Period)>0) {
		bgcData=subset(bgcData,Period==select.list(unique(bgcData$Period), preselect = NULL, multiple = FALSE,
					 title = "Choose Time Period to Analyze",
					 graphics = TRUE))
	}
		
}

{ # create annual variable data frame to pass to subsequent functions
	annVar=data.frame(var=names(bgcData)[160:length(names(bgcData))],stringsAsFactors=FALSE)
		annVar$title=annVar$var

	
}

{ # Select annual climate data - user input
		
	varList=select.list(annVar[,"var"],preselect = annVar[,"var"], multiple = TRUE,
					 title = "Choose Climate Variables to Analyze",
					 graphics = TRUE)
	
	X=subset(bgcData,Var=="mean",select=c("BGC",varList))
#	X=subset(bgcData,Var=="5%",select=c("BGC",varList))
#  X=subset(bgcData,Var=="95%",select=c("BGC",varList))
}


{ # dotplots by variable and output to pdf file

	BGCdotplot=function(var) {
		
		X2=summaryBy(Var~BGC+Zone,data=data.frame(BGC=X$BGC,Var=X[,var],Zone=gsub(" ","",substr(X$BGC,start=1,stop=4)),stringsAsFactors=FALSE),FUN=mean,na.rm=TRUE)
		X3=orderBy(~Var.mean,data=X2)
		# identify zone for blocking (break the dataset into two for plotting)
		#zoneList=c("ESSF","SBS","IDF","ICH")
	
		#Y1=orderBy(~Var.mean,data=X2[which(X2$Zone%in%zoneList==TRUE),])
		#Y2=orderBy(~Var.mean,data=X2[which(X2$Zone%in%zoneList==FALSE),])
				
		# get plot title
		plot.title=annVar$title[which(annVar$var%in%var==TRUE)]
		
		# get x label
		# xlab=annVar$xlab[which(annVar$var%in%var==TRUE)]
		# if (xlab=="Deg. C") xlab=expression(~degree~C)
		
			
		# label and point formats
		p.adj=0.33 # make point size this proportion
		lab.adj=0.5 # make label size this proportion
		
		par(mfrow=c(1,2),mar=c(5, 6, 4, 2) + 0.1)
		
		# with(Y1,dotchart(Var.mean,labels=BGC,groups=as.factor(Zone),pch=21,bg="black",cex=p.adj,main=plot.title,xlab=xlab,ylab=""))
		# with(Y2,dotchart(Var.mean,labels=BGC,groups=as.factor(Zone),pch=21,bg="black",cex=p.adj,main=plot.title,xlab=xlab,ylab=""))
		
		with(X3,dotchart(Var.mean,labels=BGC,groups=as.factor(Zone),pch=21,bg="black",cex=p.adj,main=plot.title,ylab=""))
		#with(Y2,dotchart(Var.mean,labels=BGC,groups=as.factor(Zone),pch=21,bg="black",cex=p.adj,main=plot.title,ylab=""))
						
	}
	
	
	
# write everything to pdf
	pdf(file=paste(fname,".pdf",sep=""),width=8,height=10)
	sapply(varList,BGCdotplot)
	dev.off()

# message to user
winDialog(type = "ok", message=paste(fname,".pdf created in ",getwd(),sep=""))
}

