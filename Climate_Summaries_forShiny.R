# Script to create climate summaries given output from climate BC
##Kiri Daust, April 2018
######Future Data#################################
#.libPaths("E:/R packages")
library(tcltk)
library(foreach)
library(data.table)
library(matrixStats)

colSdColMeans <- function(x, na.rm=TRUE) { ###from @sgibb on StackOverflow - faster than apply
  if (na.rm) {
    n <- colSums(!is.na(x)) # thanks @flodel
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x*x, na.rm=na.rm) - (colMeans(x, na.rm=na.rm))^2
  return(sqrt(colVar * n/(n-1)))
}
require(data.table)
wd <- tk_choose.dir(); setwd(wd)
gc()

###FUTURE CLIMATE DATA
##read in climate BC output - 100 pts per BGC currently
rawDat <- fread(file.choose(), drop = c("Latitude","Longitude","Elevation"), stringsAsFactors = FALSE, data.table = FALSE)

###Clean data, separate variables
Ystr <- strsplit(rawDat$Year, "_")
GCM <- matrix(unlist(Ystr), ncol=3, byrow=TRUE)
rawDat <- cbind(GCM, rawDat)
colnames(rawDat)[1:3]=c("GCM","Scenario", "FuturePeriod" )
rawDat$FuturePeriod <- gsub(".gcm","",rawDat$FuturePeriod)
rawDat <- rawDat[rawDat$Scenario != "rcp26",]
rawDat <- rawDat[order(rawDat$Scenario, rawDat$FuturePeriod, rawDat$ID2, rawDat$ID1, rawDat$GCM),]
Scenario <- as.character(unique(rawDat$Scenario))
Period <- as.character(unique(rawDat$FuturePeriod))
BGC.list <- as.character(unique(rawDat$ID2))
rawDat <- rawDat[,-4]

library(doSNOW)
cl <- makeCluster(7)
registerDoSNOW(cl)
iterations <- length(BGC.list)*6
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


###Nested foreach loops through each scenario, period, BGC, and site to calculate statistics
results <- foreach(SCN = Scenario, .combine = rbind, .packages = c("matrixStats", "foreach")) %dopar%{
  datScn <- rawDat[rawDat$Scenario == SCN,]
  byPer <- foreach(PER = Period, .combine = rbind) %do%{
    datPer <- datScn[datScn$FuturePeriod == PER,]
    byBGC <- foreach(BGC = BGC.list, .combine = rbind) %do%{
      datBGC <- datPer[datPer$ID2 == BGC,]
      datEns <- datBGC[grep("Ensemble",datBGC$GCM),-c(1:5)]
      Mean <- colMeans(datEns)
      SD.Geo <- colSdColMeans(datEns) ###Geopgraphical SD, based on ensemble model
      datBGC <- datBGC[-grep("Ensemble",datBGC$GCM),]
      Site.list <- as.list(unique(datBGC$ID1))
      siteSD <- foreach(Site = Site.list, .combine = rbind) %do%{
        dat <- datBGC[datBGC$ID1 == Site,-c(1:5)]
        SDmod <- colSdColMeans(dat)
        SDmod
      }
      SD.mod <- colMeans(siteSD) ###model SD, based on all non-ensemble models
      datMat <- as.matrix(datBGC[,-c(1:5)])
      max <- colMaxs(datMat)
      min <- colMins(datMat)
      q10 <- colQuantiles(datMat, probs = 0.1)
      q90 <- colQuantiles(datMat, probs = 0.9)
      out <- as.data.frame(rbind(Mean, max, min, q10, q90, SD.Geo, SD.mod))
      out$BGC <- BGC
      out$Period <- PER
      out$Scenario <- SCN
      out$Stat <- rownames(out)
      rownames(out) <- NULL
      out
    }
  }
}

results <- results[,c(248:251,1:247)]
write.csv(results, "ClimateSummary_Future.csv")
results <- as.data.frame(results)

############Historic Data#######################################
require(Rcpp)
require(matrixStats)
require(data.table)

###read in historical data
rawDat <- fread(file.choose(), drop = c("Latitude","Longitude","Elevation"), 
                stringsAsFactors = FALSE, data.table = FALSE)


mySD <- function(x,group){
  return(mean(unique(ave(x, group, FUN = sd))))
}

##choose what periods to create summaries for
Period <- list(c(1901,1930),c(1931,1960),c(1961,1990),c(1991,2015),c(1901,1990),c(1901,2015),
               c(1945,1976),c(1971,2000),c(1977,1998))
BGC.list <- as.character(unique(rawDat$ID2))
Zone.list <- as.character(unique(gsub("[[:lower:]]|[[:digit:]]","", BGC.list)))

library(doSNOW)
cl <- makeCluster(7)
registerDoSNOW(cl)
iterations <- length(BGC.list)*length(Period)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

results <- foreach(PER = Period, .combine = rbind) %:%
  foreach(BGC = BGC.list, .combine = rbind, .packages = c("matrixStats","foreach"), .options.snow = opts) %dopar%{
    datYear <- rawDat[rawDat$Year >= PER[1] & rawDat$Year <= PER[2],]
    datBGC <- datYear[datYear$ID2 == BGC,]
    datFinal <- datBGC[0,]
    datMat <- as.matrix(datBGC[,-c(1:3)])
    Sd.Geo <- apply(datMat,2,FUN = mySD, group = datBGC$Year)
    Sd.Year <- apply(datMat,2,FUN = mySD, group = datBGC$ID1)
    xMean <- colMeans(datMat)
    xMax <- colMaxs(datMat)
    xMin <- colMins(datMat)
    q10 <- colQuantiles(datMat, probs = 0.1)
    q90 <- colQuantiles(datMat, probs = 0.9)
    out <- as.data.frame(rbind(Sd.Geo,Sd.Year,xMean,xMax,xMin,q10,q90))
    out$BGC <- BGC
    out$TimePer <- paste(PER[1],"-",PER[2])
    out$Statistic <- rownames(out)
    rownames(out) <- NULL
    out
  }

results <- results[,c(248:250,1:247)]
old <- c("Sd.Geo", "Sd.Year", "xMean", "xMax", "xMin", "q10","q90")
new <- c("st.dev.Geo", "st.dev.Ann", "mean", "max", "min", "10%", "90%")
results$Statistic <- changeNames(results$Statistic, old, new)
write.csv(results,"ClimateSummaryCurrent.csv")

changeNames <- function(x, old, new){
  result <- vector("numeric", length(x))
  for (i in 1:length(x)){
    code <- x[i]
    index <- match(code, old)
    result[i] <- new[index]
  }
  return(result)
}
