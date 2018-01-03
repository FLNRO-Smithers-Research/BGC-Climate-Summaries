# R codes here are written to extract a set number of points from a larger points file to feed into ClimateBC
require(dplyr)

# STEP 1: Prepare dataset for analysis----
# clear workspace
rm(list=ls())

#Choose file expect a ClimateBC formatted CSV with a Random number generated last column
a=file.choose()

# Read dataset:
if(grepl(".csv", a)==TRUE){mydata=read.csv(a,stringsAsFactors=FALSE,na.strings=".") # file = .csv
} else if(grepl(".zip",a)==TRUE){mydata=read.csv(unzip(a),stringsAsFactors=FALSE,na.strings=".") # .zip csv
} else if(grepl(".RData",a)|grepl(".Rda",a)==TRUE){load(a)} # .RData


# Name project and set working directory
fname="ClimateBCPts"
setwd(gsub(basename(a),"",a))
 
X = mydata
X$ID2 <- as.factor(X$ID2)
top_n = {X %>%
    group_by(ID2) %>%
    top_n(n = 100, wt= rnd)} # set n to the number of points per BGC (or unit in ID2)
top_n=top_n[,-c(6)] # Drop random number

write.csv(top_n,file=paste(fname,"csv",sep="."),row.names=FALSE) # file format ready for ClimateBC

