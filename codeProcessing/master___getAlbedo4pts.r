### master script to get albedo change due to cover crops in winter over LUCAS points
# (bph-covercrops)
#
# ===+++~~~---~~~+++===
# | Gregory Duveiller |
# ===+++~~~---~~~+++===

### package loading ###
require(tidyverse)
require(scales)
require(grid)
require(RColorBrewer)
require(sf)
require(here)

### path initialization ###

# Locate data
dpath <- 'dataProcessing/step1_dataExtractionFromGEE/'
opath <- 'dataProcessing/step2_deriveCoverCropAlbedo/'
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
fpath <- 'xplrFigures/albedo_perPts_EU'

# # For Home MacOS workstation
# dpath <- '/Volumes/GregDrive/Work/Workspace/bph-covercrops/dataProcessing/'
# opath <- '/Volumes/GregDrive/Work/Workspace/bph-covercrops/dataResults/'
# fpath <- '/Volumes/GregDrive/Work/Workspace/bph-covercrops/dataFigures/'
# 
# vpath <- '/Users/greg/Work/AncillaryDatasets/WorldVector/'


# read LUCAS points
ptsAll <- sf::st_read('dataInput/LUCAS_ARABLE.shp', quiet = TRUE)

### Process pts ###
source('codeProcessing/rStep1___fitRegressions.r')
# 
# ### get Landsat NDVI values...
# source('codeProcessing/rStep2___addLandsatData.r')








##### COULD BE OF USE : #####

# 
# 
# # Some data analysis of the NDVI points .... could be moved elsewhere
# ptsNDVImin_15yr <- ptsNDVImin %>% 
#   group_by(POINT_ID) %>%
#   summarize(medianNDVI_15yr = median(NDVI,na.rm=T))
# 
# ptsNDVImax_15yr <- ptsNDVImax %>% 
#   group_by(POINT_ID) %>%
#   summarize(medianNDVI_15yr = median(NDVI,na.rm=T))
# 
# ptsNDVImin_08yr <- ptsNDVImin %>% 
#   filter(date %in% c(2004:2011)) %>%
#   group_by(POINT_ID) %>%
#   summarize(medianNDVI_08yr = median(NDVI,na.rm=T))
# 
# ptsNDVImax_08yr <- ptsNDVImax %>% 
#   filter(date %in% c(2004:2011)) %>%
#   group_by(POINT_ID) %>%
#   summarize(medianNDVI_08yr = median(NDVI,na.rm=T))
# 
# ##### some graphs to test #####
# ptsNDVImin_all <- ptsNDVImin %>% 
#   left_join(ptsNDVImin_08yr, by = "POINT_ID")%>% 
#   left_join(ptsNDVImin_15yr, by = "POINT_ID")
# 
# 
# ggplot(ptsNDVImin)+
#   geom_point(aes(x=GPS_LONG,y=GPS_LAT,colour=NDVI))+
#   scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
#   facet_wrap(~date)
# 
# ggplot(ptsNDVImin)+
#   geom_point(aes(x=GPS_LONG,y=GPS_LAT,colour=NDVI))+
#   scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
#   facet_wrap(~date)
# 
# 
# 
# ggplot(filter(ptsNDVImin_all,date==2009))+
#   geom_point(aes(x=GPS_LONG,y=GPS_LAT,colour=ptsNDVImin-medianNDVI_08yr))+
#   scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)
# 
# ggplot(ptsNDVImin_all)+
#   geom_histogram(aes(x=NDVI-medianNDVI_08yr),alpha=0.5,fill='blue')+
#   geom_histogram(aes(x=NDVI-medianNDVI_15yr),alpha=0.5,fill='green')+
#   geom_histogram(aes(x=medianNDVI_08yr-medianNDVI_15yr),alpha=0.5,fill='red')+
#   facet_wrap(~date)+
#   coord_cartesian(xlim = c(-0.2,0.2))
# #####
# 
# # outDF <- outDF %>% 
# #   left_join(ptsNDVImin_08yr, by = 'POINT_ID') %>%
# #   dplyr::rename(NDVI.L.min=medianNDVI_08yr) %>% 
# #   left_join(ptsNDVImax_08yr, by = 'POINT_ID') %>%
# #   dplyr::rename(NDVI.L.max=medianNDVI_08yr)
# # 
# # outDF <- outDF %>% 
# #   left_join(ptsNDVImin_15yr, by = 'POINT_ID') %>%
# #   dplyr::rename(NDVI.L.min=medianNDVI_15yr) %>% 
# #   left_join(ptsNDVImax_15yr, by = 'POINT_ID') %>%
# #   dplyr::rename(NDVI.L.max=medianNDVI_15yr)
# 


