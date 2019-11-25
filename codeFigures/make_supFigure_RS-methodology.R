
require(ggplot2)
require(dplyr)
require(tidyr)
require(scales)
require(grid)
require(RColorBrewer)
require(sf)
library(raster)
library(RStoolbox)


# load data files
fpath <- 'textFigures/'
Lpt <- '36323364'
iYear <- '2008'
load(paste0('dataFigures/dat4figLUCASpt/pt_',Lpt,'/dat_',Lpt,'_',iYear,'.Rda'))


#### Subplot: map of europe #### 

# prepare map 
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)

g.map <- ggplot(europe_laea) + # NOTE: europe_laea should be provided
  geom_sf() +
  geom_sf(data = pts.sub,colour = "red", size = 4)+
  coord_sf(xlim=c(2.5e6,6e6),ylim=c(1e6,4.5e6)) +  
  theme(plot.tag = element_text(face = "bold")) +
  ggtitle('Location of the LUCAS point')


#### Subplot: RGB #### 
rgbZone = stack(paste0('dataFigures/dat4figLUCASpt/pt_',Lpt,'/L5_clip_winter_',Lpt,'.tif'))

pts <- sf::st_read('dataFigures/dat4figLUCASpt/arable.shp', quiet = TRUE)
pts.sub <- pts %>% filter(POINT_ID == Lpt)

laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
plat_prj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

buf.sub <- pts.sub %>% 
  sf::st_transform(crs = laes_prj) %>%
  sf::st_buffer(dist = 3500) %>%
  sf::st_transform(crs = plat_prj)

zone.bbox <- st_bbox(buf.sub); offset <- 0.01
yLimsZone <- c(zone.bbox$ymin - offset, zone.bbox$ymax + offset)
xLimsZone <- c(zone.bbox$xmin - offset, zone.bbox$xmax + offset)

g.rgb <- ggRGB(img = rgbZone, r = 3, g = 2, b = 1,
               stretch = 'hist') + 
  geom_sf(data = buf.sub, color = 'red', fill = "transparent") +
  geom_sf(data = pts.sub, color = 'cyan', shape = 3, size = 5) +
  coord_sf(expand = F, ylim = yLimsZone, xlim = xLimsZone) +
  theme(axis.title = element_blank(),
        plot.tag = element_text(face = "bold")) +
  ggtitle('Winter Landsat imagery') 


#### Subplot: Plot of the SNR #### 
iTime <- '2008-10-07'
g.snr <- ggplot(filter(dat, time == iTime))+
  geom_raster(aes(x = col, y = row, fill = SNR))+
  geom_point(data = filter(dat, idpt == 221, time == iTime),aes(x = col,y = row), size = 5,shape=3,colour='cyan')+
  geom_point(data = filter(dat, time == iTime, SNR > minSNR, dist < minDist),aes(x = col,y=row),
             shape=21,fill='grey80',colour='grey50',size=2)+
  scale_fill_viridis_c(option='C',limits=c(0,70),oob=squish) +
  coord_fixed(xlim = c(2,20), ylim = c(2,20), expand = F) +
  theme(legend.position = 'right',
        legend.key.height = unit(0.5,'in'),
        plot.tag = element_text(face = "bold"),
        axis.title = element_blank()) +
  ggtitle('Location of selected time series')



#### Subplot: Time Series ####

# base filter
datf0 <- dat %>%
  filter(SNR > minSNR,SNW == 0, ALB > 0, dist < minDist) %>%
  rename(NDVI = NDV, Albedo = ALB)
# get all times 
timeVct <- unique(datf0$time)
# max time limits
TIME_lims <- c(min(timeVct),max(timeVct))
# get maxtime
dum1 <-datf0 %>% group_by(time) %>% summarise(meanNDV=mean(NDVI,na.rm=T))
timeMax <- dum1$time[which(dum1$meanNDV==max(dum1$meanNDV))]
# get mintime
dum2 <- filter(dum1,time<=timeMax,time>timeMax-ndays)
timeMin <- dum2$time[which(dum2$meanNDV==min(dum2$meanNDV))]
# plot time series for central pixel (including its flags)
datT0 <- gather(datf0, variable, value, Albedo, NDVI)
datT <- filter(dat, idpt == 221, SNW == 0) %>%  
  rename(NDVI = NDV, Albedo = ALB) %>%
  gather(variable, value, Albedo, NDVI)# Get central pixel

pal <- rev(brewer.pal(9,'YlGn'))
as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
}


g.tsx <- ggplot(datT0)+
  geom_vline(xintercept = timeMin, colour = 'grey60')+
  geom_vline(xintercept = timeMax, colour = 'grey60')+
  geom_line(aes(x = time, y = value, group = idpt), alpha = 0.2, colour = 'cornflowerblue')+
  geom_point(data = datT, aes(x=time,y=value, fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  facet_wrap(~variable, nc = 1, scales="free") + 
  scale_fill_gradientn('', colours = pal, labels = as.Date_origin, limits=TIME_lims)+
  scale_y_continuous('') +
  scale_x_date('') +
  theme(legend.position = 'none', 
        legend.key.width = unit(1,'in'),
        plot.tag = element_text(face = "bold")) +
  ggtitle('Time series of variables') 


#### Subplot: Regression #### 
g.AvN <- ggplot(datf0 %>% filter(time <= timeMax, time > timeMin), 
                aes(x = NDVI, y = Albedo)) + 
  geom_point(aes(fill = as.integer(time)), shape = 21, colour = 'grey40', size = 2)+
  geom_smooth(method = 'lm', colour = pal[2], fullrange = T)+
  geom_point(data=filter(datf0, dist == 0,time <= timeMax, time > timeMax-ndays),
             aes(fill=as.integer(time)), shape=21, colour='grey20',size=5)+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin, limits=TIME_lims)+
  theme(legend.position = 'none',
        plot.tag = element_text(face = "bold")) +
  ggtitle('Local regression for the LUCAS point') 



#### Bring it all together #### 
fname <- 'FigureS1_methodology'
figW <- 12; figH <- 9; fmt <- 'png'
fullfname <- paste0(fpath, fname,'.',fmt)
if(fmt=='png'){png(fullfname, width=figW, height=figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width=figW, height=figH)}
print(g.map, vp = viewport(width = 0.3, height = 0.4, x = 0.0, y = 0.6, just=c(0,0)))
print(g.rgb, vp = viewport(width = 0.3, height = 0.4, x = 0.3, y = 0.6, just=c(0,0)))
print(g.snr, vp = viewport(width = 0.4, height = 0.4, x = 0.6, y = 0.6, just=c(0,0)))
print(g.tsx, vp = viewport(width = 0.5, height = 0.6, x = 0.0, y = 0.0, just=c(0,0)))
print(g.AvN, vp = viewport(width = 0.5, height = 0.6, x = 0.5, y = 0.0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.02, "npc"), y = unit(0.97, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("b")), x = unit(0.33, "npc"), y = unit(0.97, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("c")), x = unit(0.63, "npc"), y = unit(0.97, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("d")), x = unit(0.02, "npc"), y = unit(0.57, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("e")), x = unit(0.02, "npc"), y = unit(0.29, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("f")), x = unit(0.53, "npc"), y = unit(0.57, "npc"), gp=gpar(fontsize=18))


dev.off()



