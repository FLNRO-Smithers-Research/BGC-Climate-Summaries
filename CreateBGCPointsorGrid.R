###This script imports BEC shapefiles and DEMs to create training point data. 
##Process 1 will create a random sample of points per BGC
##Process 2 creates a regular grid of points to be applied in mapping script
###Kiri Daust, August 2018

#.libPaths("E:/R packages351")
rm(list=ls())
library(dplyr)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(magrittr)
library(tibble)
library(tidyr)
library(sf)
library(tcltk)
library(foreach)
library(httr)
library(jsonlite)
library(randomForest)
library(data.table)
require(survey)


dem <- raster("./inputs/SpatialFiles/bc25fill") ###Read DEM
bec11 <- st_read(dsn="./inputs/SpatialFiles/BGCv11_WithLandcover.gdb",layer="BGCv11_withLandcover") ##read BGC shape file - updated to clipped version
CRS.albers <- CRS ("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
allUnits <- unique(as.character(bec11$MAP_LABEL))###What units are in BEC?
allUnits <- allUnits[allUnits !=""]
##set up for parallel processing
require(doParallel)
set.seed(123321)
coreNum <- as.numeric(detectCores()-1)
coreNo <- makeCluster(coreNum)
registerDoParallel(coreNo, cores = coreNum)
#clusterEvalQ(coreNo, .libPaths("E:/R packages351"))
#BGC = "CWH wh 1"

###randomly select  points within each BEC unit and get elevation data
out <- foreach(BGC = allUnits, .combine = rbind, .packages = c("sf","sp","raster")) %dopar% {
  temp <- bec11$Shape[bec11$MAP_LABEL == BGC] ###Extract polygons for each subzones
  temp <- as(temp, "Spatial") ##conver to sp
  p <- spsample(temp, 100, type = "regular", offset = c(0, 0)) #, iter = 15   change for number of points here
  p <- spTransform(p, CRS("+init=epsg:4617")) #to lat lon in NAD83
    #p <- spTransform(p, CRS("+init=epsg:4326")) #to lat lon in WGS84
  coords <- p@coords
  coords <- as.data.frame(coords)
  
 p2 <- spTransform(p, CRS(proj4string(dem)))
 coords$el <- raster::extract(dem,p2) ##get elevation for each point
  coords$BGC <- BGC
  coords
}
stopCluster(coreNo)
out2 <- rename(out, "lon" = "x1", "lat" = "x2", "el" = "Elevation")
out2 <- out2[out2$lat < 60,]
out2 = out2[,c("BGC", "lat","lon","Elevation")]
write.csv(out2,"BECv11_100Pt_Reg.csv", row.names = TRUE) ##rename for version and number of points


####select random points from Alberta
ABdem <- raster("AlbertaDEM.tif")
ABSNR <- st_read(dsn="AlbertaNSR.gdb",layer="AlbertaBGC") ##read BGC shape file - updated to clipped version
CRS.albers <- CRS ("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
allUnits <- unique(as.character(ABSNR$NRSSubzone))###What units are in AB?
allUnits <- allUnits[allUnits !=""]
##set up for parallel processing
require(doParallel)
set.seed(123321)
coreNum <- as.numeric(detectCores()-1)
coreNo <- makeCluster(coreNum)
registerDoParallel(coreNo, cores = coreNum)
clusterEvalQ(coreNo, .libPaths("E:/R packages351"))
#BGC = "NM"
###randomly select 2000 points within each BEC unit and get elevation data
out <- foreach(BGC = allUnits, .combine = rbind, .packages = c("sf","sp","raster")) %dopar% {
  temp <- ABSNR$Shape[ABSNR$NRSSubzone == BGC] ###Extract polygons for each subzones
  temp <- as(temp, "Spatial") ##conver to sp
  p <- spsample(temp, 500, type = "random") #, iter = 15change for number of points here
  #p <- spTransform(p, CRS("+init=epsg:4326")) #to lat lon
  p <- spTransform(p, CRS("+init=epsg:4617")) #to lat lon in NAD83
  coords <- p@coords
  coords <- as.data.frame(coords)
  
  p2 <- spTransform(p, CRS(proj4string(ABdem)))
  coords$Elevation <- raster::extract(ABdem,p2) ##get elevation for each point
  coords$BGC <- BGC
  coords
}
stopCluster(coreNo)
out2 <- out[out$y < 60,]
out2 = out2[,c("BGC", "y","x","Elevation")]
write.csv(out2,"AlbertaSNR_500Pt.csv", row.names = TRUE) ##rename for version and number of points


####create 4 km grid BC, AB, US########################
CRS.albers <- CRS ("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")

BC <- readOGR(dsn = "BC_AB_US_Shp", layer = "ProvincialOutline")
AB <- readOGR(dsn = "BC_AB_US_Shp", layer = "AlbertaSubRegions")
US <- readOGR(dsn = "BC_AB_US_Shp", layer = "USA_States")

BC <- spTransform(BC, CRS.albers)
AB <- spTransform(AB, CRS.albers)
US <- spTransform(US, CRS.albers)

BCdem <- raster("bc25fill")

###combine alberta and US DEMs
ABdem <- raster("./inputs/SpatialFiles/AlbertaDEM.tif")
USdem1 <- raster("./inputs/SpatialFiles/USA_WestDEM.tif")
USdem2 <- raster("./inputs/SpatialFiles/USA_WestCoastDEM.tif")
USdem <- raster::merge(USdem1,USdem2)
allDEM <- raster::merge(ABdem,USdem) # not possible to merge BC with rasters due to different resolutions
writeRaster(allDEM, filename = "./inputs/SpatialFiles/WNA.tif")
allDEM <- raster("WNA.tif")

#####Loop through BC, Alberta, and US to create grid
names <- c("BC","AB","USA")
names <- c("BC")
i <-0  #, "AlbertaSubRegions", "USA_States"
grid4k <- foreach(X = c("ProvincialOutline"), .combine = rbind) %do% {
  i <- i+1
  shp <- readOGR(dsn = "BC_AB_US_Shp", layer = X)
  shp <- spTransform(shp, CRS.albers)
  p <- spsample(shp, cellsize = c(1000,1000), type = "regular")# change this to change grid size and type regular
  p <- spTransform(p, CRS("+init=epsg:4326")) #to lat long
  coords <- p@coords
  coords <- as.data.frame(coords)
  if(i == 1){
    dem <- BCdem
  }else{
    dem <- allDEM
  }
  p <- spTransform(p, CRS(proj4string(dem)))
  coords$elev <- raster::extract(dem,p)
  colnames(coords) <- c("long","lat","el")
  coords$Region <- names[i]
  coords
}

rownames(grid4k) <- NULL
setDT(grid4k, keep.rownames = TRUE)[]
grid4k <-grid4k[,c("rn", "Region", "lat", "long", "el")]
grid4k <- grid4k[grid4k$el >0,]
#grid4US <- grid4US[grid4US$el >0,]
grid4BC <- grid4k[grid4k$Region == "BC",]
grid4USAB <- grid4k[grid4k$Region != "BC",]
grid4USAB <- grid4USAB[grid4USAB$lat >= 37,]
grid4USAB <- grid4USAB[grid4USAB$long <= -104,]
grid4US <-grid4USAB[grid4USAB$Region != "AB",]
write.csv (grid4k, "BC_AB_US4kmGrid.csv", row.names = FALSE)
write.csv(grid4BC, "BC1kmGrid.csv", row.names = FALSE)
write.csv(grid4USAB,"US_AB4kmGrid.csv", row.names = FALSE)
write.csv (grid4US,  "US4kmGrid.csv", row.names = FALSE)


 #############################################################################3
####Assign BGCs and subregions
##################################################################################3
###for BC (input 4 km grid, output is just BC points with BGC assigned)
grid4BC <- fread("US_AB4kmGrid.csv", stringsAsFactors = FALSE, data.table = FALSE)
BGC = "CWH ms 2"
BCwBGC <- foreach(BGC = allUnits, .combine = rbind) %do%{
  dat <- grid4BC
  pointsOrig <- dat
  coordinates(dat) <- c("long", "lat")
  #proj4string(dat) <- CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
  #proj4string(dat) <- CRS("+init=epsg:4326") ##WGS84
  proj4string(dat) <- CRS("+init=epsg:4617") ##NAD83
  dat <- spTransform(dat, CRS.albers)  # standard albers projection for BC gov't data BCAlbers 
  
  tempPoly <- bec11[bec11$BGC_LABEL == BGC,]
  tempPoly <- as(tempPoly, "Spatial") ##conver to sp
  tempPoly <- spTransform(tempPoly, CRS.albers) 
  dat <- over(dat, tempPoly) ###which ones are inside the BGC
  pointsOrig <- pointsOrig[!is.na(dat$BGC_LABEL),] ###Remove points not inside BGC
  if(nrow(pointsOrig) > 0){ ###check that some points fall inside BGC
    pointsOrig$BGC <- BGC
    pointsOrig
  }
}
 BCwBGC$ID1 <-row.names(BCwBGC)
colnames(BCwBGC)[6] <- "ID2"
BC4kgrid2  <- BCwBGC [,c("ID1", "ID2","lat", "long", "el")]
write.csv (BC4kgrid2, "BC1kmGrid_mapped_BGC2.csv", row.names = FALSE)
###random sample of grid by subzone

mydesign <- stratified(pointDat, "Subzone" ,10, replace=FALSE)


 ###for AB (input 4 km grid, output is just Alberta points with BGC assigned)
AB <- fread("AB2kmGrid.csv", stringsAsFactors = FALSE, data.table = FALSE)
colnames(AB) <-c("ID1", "ID2", "Latitude", "Longitude", "Elevation")
#AB <- AB[AB$Region == "AB",]
#ABSNR <- read_sf(dsn="AlbertaNSR.gdb",layer="AlbertaBGC") ##read BGC shape file - updated to clipped version
ABSNR <- readOGR(dsn = "BC_AB_US_Shp", layer = "AlbertaSubRegions_Albers")
CRS.albers <- CRS ("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
ABSNR <- spTransform(ABSNR, CRS.albers)
abUnits <- unique(as.character(ABSNR$NRSSubzone))
abUnits <- abUnits[abUnits !=""]
BGC = "IMA ab"
ABwBGC <- foreach(BGC = abUnits, .combine = rbind) %do%{
  dat <- AB
  pointsOrig <- dat
  coordinates(dat) <- c("Longitude", "Latitude")
  proj4string(dat) <- CRS("+init=epsg:4617") ##NAD83
  dat <- spTransform(dat, CRS.albers)  # standard albers projection for BC gov't data
  
  tempPoly <- ABSNR[ABSNR$NRSSubzone == BGC,]
  #tempPoly <- as(tempPoly, "Spatial") ##conver to sp
  #tempPoly <- spTransform(tempPoly, CRS.albers)
 # tempPoly <- st_transform(tempPoly, "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs") 
  dat <- over(dat, tempPoly) ###which ones are inside the BGC
  #dat2 <- st_intersects(dat, tempPoly) ###which ones are inside the BGC
  pointsOrig <- pointsOrig[!is.na(dat),] ###Remove points not inside BGC
  pointsOrig$BGC <- BGC
  pointsOrig
}
#ABwBGC$ID1 <-row.names(ABwBGC)
colnames(ABwBGC)[1] <- "ID1"
AB4kgrid2  <- ABwBGC [,c("ID1", "BGC","Latitude", "Longitude", "Elevation")]
AB4kgrid2 <- AB4kgrid2 [!is.na (AB4kgrid2$ID1),]
write.csv (AB4kgrid2, "AB2kmGrid_w_BGC_8Mar2018.csv")
write.csv (ABwBGC, "AB2kmGrid_w_BGC_1961-1990MSY_8Mar2018.csv")

################ function to return a stratified random sample from a grid of points
stratified <- function(df, group, size, select = NULL, 
                       replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}


rndsample <- stratified(X1.sub, group = "BGC", size = 100)

X1.sub <- rndsample

VarList = c("AHM", "bFFP","CMD.total","DD5_sp","EMT","Eref_sm","EXT","FFP","MCMT","MSP",
            "PPT_JAS","PPT_MJ","PPT06","SHM","TD","Tmax_sp","Tmin_at","Tmin_sm","Tmin_wt",
            "PAS","CMD.def","CMDMax","eFFP","Eref09","MAT","PPT07","Tmin_sp")
List = c("rn", "Subzone", "Latitude", "Longitude", "Elevation")
X1.sub2 = rndsample[,names(rndsample) %in% c(List,VarList)]
write.csv (X1.sub2, "USS_500TrainingPtsData_BGC_predicted__Jan29.csv")
