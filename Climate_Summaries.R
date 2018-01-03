# R codes here are written to calculate climate summaries


# STEP 1: Prepare dataset for analysis----
# clear workspace
rm(list=ls())

#Choose file = from ClimateWNA output -- all variables time series 1901-latest
a=file.choose()
setwd(gsub(basename(a),"",a))

# Read dataset:
if(grepl(".csv", a)==TRUE){mydata=read.csv(a,stringsAsFactors=FALSE,na.strings=".") # file = .csv
} else if(grepl(".zip",a)==TRUE){mydata=read.csv(unzip(a),stringsAsFactors=FALSE,na.strings=".") # .zip csv
} else if(grepl(".RData",a)|grepl(".Rda",a)==TRUE){load(a)} # .RData
fname= basename(a)

#save(mydata,file=paste(fname,".Rda",sep="")) # save as an RDA for future applications

# Name project and set working directory
#fname="BGC99_Climate_Summaries"


mydata=mydata[,-c(4:6)] # Drop Lat, Long, Elevation

memory.size()   # take a look at how much memory R is using
gc()   

X=as.data.frame (mydata)


# STEP 2: Choose Periods to run analysis on----
# Time Period
# 1) 1901-1960
# 2) 1901-1990
# 3) 1901-2015
# 4) 1931-1960
# 5) 1945-1976
# 6) 1961-1990
# 7) 1971-2000
# 8) 1977-1998
# 9) 1991-2015

time1=select.list(c(paste(min(X$Year),"1960",sep="-"),paste(min(X$Year),"1990",sep="-"),paste(min(X$Year),max(X$Year),sep="-"),
                   "1931-1960", "1945-1976","1961-1990","1971-2000","1977-1998",paste("1991",max(X$Year),sep="-")),preselect = "1961-1990", multiple = TRUE,
                  title = "Choose Time Periods",graphics = TRUE)

timePeriod=matrix(unlist(strsplit(time1,"-")),length(time1),2,byrow=TRUE)

# STEP 3: Calculate mean, sd,max,min and extreme values
library(plyr)
library(foreach)
library(doParallel)

registerDoParallel(detectCores())


{ #START:
  temp=data.frame(matrix(nrow=0,ncol=ncol(X)),stringsAsFactors=FALSE)
  
  for (i in 1:length(time1)){

    # Reset dataset
    X=mydata

    # Subset Time Period Chosen:
    X=subset(X,Year>=as.numeric(timePeriod[i,1]) & Year<=as.numeric(timePeriod[i,2]))

    
    # Calculate by BEC units and points
    ind=which(colnames(X)=="Tmax01")
    X.mean=ddply(X,.(ID2,ID1),.parallel=TRUE,function(x) apply(x[,4:ncol(x)],2,mean))
    X.sd=ddply(X,.(ID2,ID1),.parallel=TRUE,function(x) apply(x[,4:ncol(x)],2,sd))
    X.min=ddply(X,.(ID2,ID1),.parallel=TRUE,function(x) apply(x[,4:ncol(x)],2,min))
    X.max=ddply(X,.(ID2,ID1),.parallel=TRUE,function(x) apply(x[,4:ncol(x)],2,max))
    X.5q=ddply(X,.(ID2,ID1),.parallel=TRUE,function(x) apply(x[,4:ncol(x)],2,quantile,probs=.1))
    X.95q=ddply(X,.(ID2,ID1),.parallel=TRUE,function(x) apply(x[,4:ncol(x)],2,quantile,probs=.9))

    
    # Calculate means by BEC units 
    X.mean1=ddply(X.mean,.(ID2),.parallel=TRUE,function(x) apply(x[,3:ncol(x)],2,mean))
    X.sd1=ddply(X.sd,.(ID2),.parallel=TRUE,function(x) apply(x[,3:ncol(x)],2,mean))
    X.sd2=ddply(X.mean,.(ID2),.parallel=TRUE,function(x) apply(x[,3:ncol(x)],2,sd))
    X.min1=ddply(X.min,.(ID2),.parallel=TRUE,function(x) apply(x[,3:ncol(x)],2,mean))
    X.max1=ddply(X.max,.(ID2),.parallel=TRUE,function(x) apply(x[,3:ncol(x)],2,mean))
    X.5q1=ddply(X.5q,.(ID2),.parallel=TRUE,function(x) apply(x[,3:ncol(x)],2,mean))
    X.95q1=ddply(X.95q,.(ID2),.parallel=TRUE,function(x) apply(x[,3:ncol(x)],2,mean))
    
    X.mean1$Var="mean"
    X.sd1$Var="st.dev.Ann"
    X.sd2$Var="st.dev.Geo"
    X.min1$Var="min"
    X.max1$Var="max"
    X.5q1$Var="10%"
    X.95q1$Var="90%"
    
    # Concatenate Datasets:
    X1=rbind(X.mean1,X.sd1,X.sd2,X.min1,X.max1,X.5q1,X.95q1)
    X1$period=paste(timePeriod[i,1],timePeriod[i,2],sep="-")
    colnames(X1)[1]="BGC"
    final=X1[,c(1,ncol(X1),(ncol(X1)-1),2:(ncol(X1)-2))]
    
    # Save Outputs:
    A=assign(paste0("final"),data.frame(final))
    temp=rbind(temp,A)    
    rm(list=ls(pattern="final"))
    
  }
} #END


## Reorder
temp=temp[order(temp[,"BGC"],temp[,"period"],temp[,"Var"]),]

write.csv(temp,file=paste(fname,"csv",sep="."),row.names=FALSE)




