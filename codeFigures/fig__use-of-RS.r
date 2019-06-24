library(raster)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(dplyr)
library(sf)
library(RStoolbox)
library(here)

pt_ID <- '28761622'
pt_ID <- '34362088'

laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
buf <- sf::st_read('dataFigures/dat4figLUCASpt/arable_buffer.shp', quiet = TRUE)
bufzone <- buf %>% filter(POINT_ID == pt_ID)
pts <- sf::st_read('dataFigures/dat4figLUCASpt/arable.shp', quiet = TRUE)
ptszone <- pts %>% filter(POINT_ID == pt_ID)


L8_Winter = stack(paste0('dataFigures/dat4figLUCASpt/pt_',pt_ID,'/L8_clip_winter_',pt_ID,'.tif'))
L8_Summer = stack(paste0('dataFigures/dat4figLUCASpt/pt_',pt_ID,'/L8_clip_summer_',pt_ID,'.tif'))


Albedo <- stack(list(winter = raster(paste0('dataFigures/dat4figLUCASpt/pt_',pt_ID,'/albedo_winter_',pt_ID,'.tif')),
                     summer = raster(paste0('dataFigures/dat4figLUCASpt/pt_',pt_ID,'/albedo_summer_',pt_ID,'.tif'))))
Albedo_df <- as.data.frame(0.001 * Albedo, xy = T, long = T)
colnames(Albedo_df) <- c('Easting','Northing','Season','Albedo')




# plot parametrizations
ALB_lims <- c(0,0.2)



# RGB plots
g.map.L8.winter <- ggRGB(img = L8_Winter, r = 3, g = 2, b = 1,
                      stretch = 'hist') + 
  geom_sf(data = bufzone, color = 'red', fill = "transparent") +
  scale_y_continuous('Northing') + 
  scale_x_continuous('Easting') + 
  coord_sf(expand = F)

g.map.L8.summer <- ggRGB(img = L8_Summer, r = 3, g = 2, b = 1,
                      stretch = 'hist') + 
  geom_sf(data = bufzone, color = 'red', fill = "transparent") +
  scale_y_continuous('Northing') + 
  scale_x_continuous('Easting') + 
  coord_sf(expand = F)


# Albedo maps 
g.map.Albedo.winter <- ggplot(Albedo_df %>% filter(Season == 'winter')) +
  geom_raster(aes(x = Easting, y = Northing, fill = Albedo)) +
  geom_sf(data = bufzone, color = 'red', fill = "transparent") +
  geom_sf(data = ptszone, color = 'red') +
  scale_fill_viridis_c(limits = ALB_lims, oob = squish, na.value = "transparent") + ## option = "inferno"  +
  coord_sf(expand = F) +
  theme(legend.position = c(0.1,0.1),
        axis.title = element_blank())

g.map.Albedo.summer <- ggplot(Albedo_df %>% filter(Season == 'summer')) +
  geom_raster(aes(x = Easting, y = Northing, fill = Albedo)) +
  geom_sf(data = bufzone, color = 'red', fill = "transparent") +
  scale_fill_viridis_c(limits = ALB_lims, oob = squish, na.value = "transparent") + ## option = "inferno"  +
  coord_sf(expand = F) +
  theme(legend.position = c(0.1,0.1),
        axis.title = element_blank())



### ... 



g.map.albedo <- ggplot(filter(dat, time == '2016-10-07'))+
  geom_raster(aes(x = col, y = row, fill = ALB))+
  geom_point(data = filter(dat, idpt == 221, time == '2016-10-07'),
             aes(x = col, y = row), size = 5, shape = 3, colour = 'cyan')+
  geom_point(data = filter(dat,time == '2008-10-07', SNR > minSNR, dist < minDist),
             aes(x = col, y = row), shape = 21, fill = 'grey80', colour = 'grey50', size = 4)+
  scale_fill_viridis_c(limits = ALB_lims, oob = squish, na.value = "transparent")+
  coord_fixed()+
  theme(legend.position = c(0.1,0.1))


# SNR
g.map.snr <- ggplot(filter(dat, time == '2016-10-07'))+
  geom_raster(aes(x = col, y = row, fill = SNR))+
  geom_point(data = filter(dat, idpt == 221, time == '2016-10-07'),
             aes(x = col, y = row), size = 5, shape = 3, colour = 'cyan')+
  geom_point(data = filter(dat,time == '2008-10-07', SNR > minSNR, dist < minDist),
             aes(x = col, y = row), shape = 21, fill = 'grey80', colour = 'grey50', size = 4)+
  scale_fill_viridis_c(option='C', limits = c(0,70), oob = squish)+
  coord_fixed()+
  theme(legend.position = c(0.1,0.1))


# regression
g.plot.ALBvsNDVI <- ggplot(filter(datf0, time <= timeMax, time > timeMin),
                      aes(x = NDV, y = ALB)) + 
  geom_point(aes(x=NDV,y=ALB,fill=as.integer(time)), shape=21, colour='grey40',size=2)+
  geom_smooth(method = 'lm', colour=pal[2],fullrange=T)+
  geom_point(data=filter(datf0,dist==0,time<=timeMax,time>timeMax-ndays),
             aes(fill=as.integer(time)), shape=21, colour='grey20',size=5)+
  scale_fill_gradientn('Time', colours = pal, labels = as.Date_origin, limits = TIME_lims)+
  coord_cartesian(ylim = ALB_lims)+
  theme(legend.position = 'none') #c(0.8,0.2)
