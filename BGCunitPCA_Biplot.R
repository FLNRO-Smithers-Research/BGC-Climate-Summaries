# This script will create a biplot from PCA of BGC climate data
# need FactoMineR package

rm(list=ls())

# choose the input file ('BGC_Climate_Summaries.csv' on my computer)
a=file.choose()
bgcData=read.csv(a,stringsAsFactors=FALSE,na.strings=".")

# files will be saved in same directory as bgcData 
setwd(gsub(basename(a),"",a))

{ # User selects time period and climate variables to include in the PCA

	# select the time period to analyze
	bgcData=subset(bgcData,period==select.list(unique(bgcData$period), preselect = unique(bgcData$period)[1], multiple = FALSE,
		title = "Choose Time Period to Analyze",graphics = TRUE))
	
	# Select climate variables to include in PCA
	annVar=colnames(bgcData)[grep("Tmax01",colnames(bgcData)):ncol(bgcData)]	# assume Tmax01 is first column of climate data
	
	# bFFP and eFFP have NA values that cause problems in PCA
	
	varList=select.list(annVar,preselect = annVar, multiple = TRUE,
					 title = "Choose Climate Variables to Analyze",
					 graphics = TRUE)
	
	X=subset(bgcData,Var=="mean",select=c("BGC",varList))
	rownames(X)=X$BGC
	X$BGC=NULL
	
}

{ # PCA
 
library(FactoMineR)

# only retain first two factors
res.pca=PCA(X,ncp=2,scale.unit=TRUE,graph=FALSE)

}

{ # PCA biplot
# graph out ALL sites and some factors

# extract sites and variables from PCA
	sites=as.data.frame(res.pca$ind$coord)
	sites$Site=rownames(sites)
	sites$zone=substr(sites$Site,start=1,stop=4)

	vars=res.pca$var$coord
	varList=c("MAT","MAP","TD","DD.18","MSP","AHM","CMD","PAS","TD","Tmax_sp","PPT_sm","DD.5","Tmax_sm","Tave_sm","Tmin_wt","Tave_wt","Tmin_at","Tave_sp",
		"bFFP","DD.0","Eref")
	vars2=as.data.frame(vars[rownames(vars)%in%varList,])
	vars=as.data.frame(vars)


{ # PCA biplot Sites and Variable

# Choose sites to highlight
	
	# create vectors of colors and symbols to use
	colList=c("black","blue","green","orange","purple","red","cyan","darkgrey")
	pchList=c(15,17,19,15,17,19,15,17)
	
	zoneList=select.list(unique(sites$zone),preselect = NULL, multiple = TRUE,
						 title = "Choose Zones",
						 graphics = TRUE)
	
	# by default, all zones are plotted in light grey circles
	sites$col="lightgrey"
	sites$pch=19
	
	# assign unique colors and symbols to each zone of interest
	for (k in 1:length(zoneList)) {sites$col[grep(zoneList[k],sites$zone)]=colList[k];sites$pch[grep(zoneList[k],sites$zone)]=pchList[k]}
	
	# subset for zones of interest
	sites2=subset(sites,col!="lightgrey")
	
	# plot
	# start plot - will have two X and Y axes
	xlab=paste("Dim.1 (",round(res.pca$eig[1,2],1),"%)",sep="")
	ylab=paste("Dim.2 (",round(res.pca$eig[2,2],1),"%)",sep="")
	main1=paste("PCA biplot of First 2 dimensions (",round(res.pca$eig[2,3],1),"% of total variance)",sep="")
	
	fname="PCA biplots with Zones highlighted.pdf"
	pdf(width=8,height=8,file=fname)
	with(sites,plot(Dim.1,Dim.2,ylim=c(-20,20),xlim=c(-20,20),type="n",xlab=xlab,ylab=ylab,main=main1))
	par(new=T)
	with(vars2,plot(Dim.1,Dim.2,ylim=c(-1,1),xlim=c(-1,1),axes=F,type="n",xlab="",ylab=""))

	# show all variables in light grey
	with(vars,arrows(x0=0,y0=0,x1=Dim.1,y1=Dim.2,col="lightgrey",lwd=2,length=0.05))

	# plot variables of interest and label them
	with(vars2,arrows(x0=0,y0=0,x1=Dim.1,y1=Dim.2,col="red",lwd=2,length=0.1))
	with(vars2,text(x=Dim.1+0.01,Dim.2+0.01,cex=0.8,labels=rownames(vars2)))

	par(new=T)
	with(sites,plot(Dim.1,Dim.2,ylim=c(-20,20),xlim=c(-20,20),col=sites$col,pch=sites$pch,ylab="",xlab=""))
	with(sites2,points(Dim.1,Dim.2,ylim=c(-20,20),xlim=c(-20,20),col=sites2$col,pch=sites2$pch,cex=1.5))
	
	# put dashed lines at 0,0
	abline(v=0,h=0,lty=2)
	
	# legend
	legend(x=-20,y=21,legend=c(gsub(" ","",zoneList),"others"),pch=c(pchList[1:k],19),col=c(colList[1:k],"lightgrey"),cex=0.8,bty="n")
	
	dev.off()
	
	winDialog(type="ok",paste(fname," saved to ",getwd(),sep=""))
	
}

}

