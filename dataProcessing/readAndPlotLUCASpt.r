library(tidyverse)

bpath <- '/ESS_Datasets/USERS/Duveiller/Workspace/'
fpath <- paste0(bpath,'/bph-covercrops/testFigures_v5/')
dir.create(fpath,showWarnings = T, recursive = T)


#dpath <- '/ESS_Datasets/USERS/Duveiller/Workspace/SOIL_LUCAS/'
dpath <- '/DATA/scratch/SOIL_LUCAS/'
vpath <- '/DATA/datasets/WorldVector/'
  # read full data file
rawCsv <- readr::read_csv(paste0(dpath,'FileGregAllFlagsNDVISNOW.csv'))

# read LUCAS points
require(sf)
ptsAll <- sf::st_read(paste0(dpath,'LUCAS_ARABLE.shp'), quiet = TRUE)

# prepare map 
world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(nc)))%>%
  st_transform(laes_prj)



# define spatial flitering requirements
minSNR <- 25
minDist <- 7
ndays <- 150

#36822858
#37362822  # reasonable
#48103100  # good point
#40203266  # ok point in Flevoland
#54862418  # not great, in RO
#31301872  # in SP, with with soil, and perhaps cover crop?

source('/DATA/repos/bph-covercrops/dataProcessing/plotFigs4LucasPt.r') # fucntion for NPP
source('/DATA/repos/bph-covercrops/dataProcessing/plot___LucasPt_xplrFigs.r') # fucntion for NDVI



source('/DATA/repos/bph-covercrops/dataProcessing/plot___LucasPt_xplrFigs_v2.R') # fucntion for NDVI


LptList <- sample(rawCsv$POINT_ID.x,50)
for(Lpt in LptList){
 plotFigs4LucasPt(Lpt=Lpt,fpath=fpath,rawCsv=rawCsv,minSNR=minSNR,minDist=minDist,ndays=ndays)
}

