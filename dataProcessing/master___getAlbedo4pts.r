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


### path initialization ###

# For JRC Linux workstation
dpath <- '/DATA/scratch/SOIL_LUCAS/'
opath <- '/ESS_Datasets/USERS/Duveiller/Workspace/bph-covercrops/dataResults/'

vpath <- '/DATA/datasets/WorldVector/'

# For Home MacOS workstation
dpath <- '/Volumes/GregDrive/Work/Workspace/bph-covercrops/dataProcessing/'
opath <- '/Volumes/GregDrive/Work/Workspace/bph-covercrops/dataResults/'
fpath <- '/Volumes/GregDrive/Work/Workspace/bph-covercrops/dataFigures/'

vpath <- '/Users/greg/Work/AncillaryDatasets/WorldVector/'

### parametrization ###

minSNR <- 25
minDist <- 7
ndays <- 150


### read data ###

# read LUCAS points
ptsAll <- sf::st_read(paste0(dpath,'LUCAS_ARABLE.shp'), quiet = TRUE)
# read full data file
rawCsv <- readr::read_csv(paste0(dpath,'FileGregAllFlagsNDVISNOW.csv'))
# read min-max LANDSAT NDVI
ptsNDVImin <- readr::read_csv(paste0(dpath,'min_NDVI.csv'))
ptsNDVImax <- readr::read_csv(paste0(dpath,'max_NDVI.csv'))



### Process pts ###

LptList <- unique(rawCsv$POINT_ID.x)
extractData <- function(Lpt){
# declare window size
ws = 21 # needs to be hardcoded perhaps/
tryCatch({
# subset data and sort out some column names
subDat <- rawCsv %>% 
  rename(SNR_vct=`SNR_filter[, 22]`, 
         NDV_vct=NDVI,
         NDV_qlty_vct=BRDF_Albedo_Band_Mandatory_Quality_Band1,
         ALB_vct=AlbedoMean, 
         ALB_qlty_vct=BRDF_Albedo_Band_Mandatory_Quality_shortwave,
         SNW_vct=Snow_BRDF_Albedo,
         NPP_vct=PsnNet,NPP_qlty_vct=Psn_QC) %>%
  filter(POINT_ID.x==Lpt) 

subDat$NDV_vct[is.na(subDat$NDV_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$NDV_qlty_vct[is.na(subDat$NDV_qlty_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$ALB_vct[is.na(subDat$ALB_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$ALB_qlty_vct[is.na(subDat$ALB_qlty_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$SNW_vct[is.na(subDat$SNW_vct)] <- paste(rep('NA',ws^2),collapse=',')

# prepare dataframe to host exploded values from the window
dat <- data.frame(idpt=rep(1:ws^2,times=dim(subDat)[1]),
                  time=as.Date(rep(subDat$time,each=ws^2),format = '%Y_%m_%d'))

# explose the cells with multiple values of the window
dat$SNR=as.numeric(as.vector(unlist(strsplit(subDat$SNR_vct, ","))))
dat$NDV=as.numeric(as.vector(unlist(strsplit(subDat$NDV_vct, ","))))
dat$ALB=as.numeric(as.vector(unlist(strsplit(subDat$ALB_vct, ","))))*0.001
dat$ALB_qlty=as.integer(as.vector(unlist(strsplit(subDat$ALB_qlty_vct, ","))))
dat$NDV_qlty=as.integer(as.vector(unlist(strsplit(subDat$NDV_qlty_vct, ","))))
dat$SNW=as.integer(as.vector(unlist(strsplit(subDat$SNW_vct, ","))))

# calculate distances within the window
dd <- data.frame(idpt=1:ws^2,row=rep(1:ws,each=ws),col=rep(1:ws,times=ws))
dd$dist <- sqrt((dd$col-11)^2 + (dd$row-11)^2)

# further prepare the dataframe
dat <- dat %>%
  filter(SNR!=0)%>% # removes areas that have been masked in GEE
  left_join(dd,by = "idpt") # join it with a filter


# base filter
datf0 <- filter(dat,SNR>minSNR,SNW==0,ALB>0,dist<minDist)
if(dim(datf0)[1]==0){print(paste('No data left to plot for point', Lpt)); return(dfout=NULL)}

# get all times 
timeVct <- unique(datf0$time)
# max time limits
TIME_lims <- c(min(timeVct),max(timeVct))
# get maxtime
dum1 <-datf0 %>% group_by(time) %>% summarise(meanNDV=mean(NDV,na.rm=T))
timeMax <- dum1$time[which(dum1$meanNDV==max(dum1$meanNDV))]
# get mintime
dum2 <- filter(dum1,time<=timeMax,time>timeMax-ndays)
timeMin <- dum2$time[which(dum2$meanNDV==min(dum2$meanNDV))]


fit <- lm(ALB ~ NDV, data = filter(datf0,time<=timeMax,time>timeMin))  



dfout <- data.frame(POINT_ID=Lpt,
                    NDVI.L.min = filter(ptsNDVImin,POINT_ID==Lpt)$median,
                    NDVI.L.max = filter(ptsNDVImax,POINT_ID==Lpt)$median,
                    NDVI.M.min = min(dum2$meanNDV),
                    NDVI.M.max = max(dum1$meanNDV),
                    b0=fit$coefficients[1],
                    b1=fit$coefficients[2],
                    n=length(fit$residuals),
                    adj.r.sqr=summary(fit)$adj.r.squared,
                    rmse=sqrt(mean((fit$residuals)^2)),
                    mae=mean(abs(fit$residuals))) 
}, error=function(cond){return(dfout=NULL)})

return(dfout)
}

outList <- lapply(LptList,FUN = extractData)
outDF <- do.call('rbind',outList)



### get values ###  
pts0 <- outDF %>%
  mutate(Landsat=b0+b1*NDVI.L.min,
         MODIS=b0+b1*NDVI.M.min) %>%
  gather(BareSoil.Albedo.Source,BareSoil.Albedo,Landsat,MODIS) %>%
  mutate(Landsat=b0+b1*NDVI.L.max,
         MODIS=b0+b1*NDVI.M.max) %>%
  gather(MaxVgt.Albedo.Source,MaxVgt.Albedo,Landsat,MODIS) %>%
  left_join(select(ptsAll,POINT_ID,GPS_LAT,GPS_LONG),by='POINT_ID')

dir.create(opath)
save('pts0',file = paste0(opath,'deltaCC_df.RData'))


# prepare map 
world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)


pts_sf <- st_as_sf(pts0) %>% st_transform(laes_prj)

require(RColorBrewer)
require(scales)

dir.create(fpath)

g.map.delta <- ggplot(filter(pts_sf,
                             BareSoil.Albedo.Source=='Landsat',
                             MaxVgt.Albedo.Source=='MODIS')) + 
  geom_sf(data=europe_laea,fill='grey50')+
  geom_sf(aes(colour=MaxVgt.Albedo-BareSoil.Albedo))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  coord_sf(xlim=c(2.5e6,6e6),ylim=c(1.5e6,4.5e6))+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))
ggsave(filename = 'DeltaCC.png', plot = g.map.delta, path = fpath, width = 12, height = 10)

g.map.combo <- ggplot(pts_sf) + 
  geom_sf(data=europe_laea,fill='grey50')+
  geom_sf(aes(colour=MaxVgt.Albedo-BareSoil.Albedo))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  facet_grid(BareSoil.Albedo.Source~MaxVgt.Albedo.Source,labeller = label_both)+    
  coord_sf(xlim=c(2.5e6,6e6),ylim=c(1.5e6,4.5e6))+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))
ggsave(filename = 'DeltaCC_combo.png', plot = g.map.combo, path = fpath, width = 12, height = 10)


