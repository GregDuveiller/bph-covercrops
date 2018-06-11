library(tidyverse)

bpath <- '/ESS_Datasets/USERS/Duveiller/Workspace/'
fpath <- paste0(bpath,'/bph-covercrops/testFigures_v2/')
dir.create(fpath,showWarnings = T, recursive = T)


#dpath <- '/ESS_Datasets/USERS/Duveiller/Workspace/SOIL_LUCAS/'
dpath <- '/DATA/scratch/SOIL_LUCAS/'
vpath <- '/DATA/datasets/WorldVector/'
  # read full data file
rawCsv <- readr::read_csv(paste0(dpath,'FileGregAllFlagsNDVI.csv'))

# define spatial flitering requirements
minSNR <- 10
minDist <- 5

# set point to plot
Lpt <- 31301872
#36822858
#37362822  # reasonable
#48103100  # good point
#40203266  # ok point in Flevoland
#54862418  # not great, in RO
#31301872  # in SP, with with soil, and perhaps cover crop?

source('/DATA/repos/bph-covercrops/dataProcessing/plotFigs4LucasPt.r') # fucntion for NPP
source('/DATA/repos/bph-covercrops/dataProcessing/plot___LucasPt_xplrFigs.r') # fucntion for NDVI

# plot it all
plotFigs4LucasPt(Lpt=Lpt,fpath=fpath,rawCsv=rawCsv,minSNR=minSNR,minDist=minDist)
