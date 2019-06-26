library(raster)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(dplyr)
library(sf)
library(RStoolbox)
library(here)

Lpt <- '28761622'
Lpt <- '34362088'
Lpt <- '37923016'



load(paste0('dataFigures/dat4figLUCASpt/pt_',Lpt,'/dat_',Lpt,'.Rda'))
ALB_lims <- quantile(dat$ALB, na.rm = T, probs = c(0.05, 0.95))


L8_Winter = stack(paste0('dataFigures/dat4figLUCASpt/pt_',Lpt,'/L8_clip_winter_',Lpt,'.tif'))
L8_Summer = stack(paste0('dataFigures/dat4figLUCASpt/pt_',Lpt,'/L8_clip_summer_',Lpt,'.tif'))


Albedo <- stack(list(winter = raster(paste0('dataFigures/dat4figLUCASpt/pt_',Lpt,'/albedo_winter_',Lpt,'.tif')),
                     summer = raster(paste0('dataFigures/dat4figLUCASpt/pt_',Lpt,'/albedo_summer_',Lpt,'.tif'))))
Albedo_df <- as.data.frame(0.001 * Albedo, xy = T, long = T)
colnames(Albedo_df) <- c('Easting','Northing','Season','Albedo')

laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
plat_prj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
buf <- sf::st_read('dataFigures/dat4figLUCASpt/arable_buffer.shp', quiet = TRUE)
bufzone <- buf %>% filter(POINT_ID == Lpt)

pts <- sf::st_read('dataFigures/dat4figLUCASpt/arable.shp', quiet = TRUE)
pts.sub <- pts %>% filter(POINT_ID == Lpt)

#buf.sub <- pts.sub %>% sf::st_buffer(res(Albedo)[1] * 7)

buf.sub <- pts.sub %>% 
  sf::st_transform(crs = laes_prj) %>%
  sf::st_buffer(dist = 3500) %>%
  sf::st_transform(crs = plat_prj)
  
zone.bbox <- st_bbox(buf.sub); offset <- 0.01
yLims <- c(zone.bbox$ymin - offset, zone.bbox$ymax + offset)
xLims <- c(zone.bbox$xmin - offset, zone.bbox$xmax + offset)


# RGB plots
g.map.L8.winter <- ggRGB(img = L8_Winter, r = 3, g = 2, b = 1,
                      stretch = 'hist') + 
  geom_sf(data = ptszone_buf, color = 'red', fill = "transparent") +
  geom_sf(data = ptszone, color = 'red') +
  scale_y_continuous('Northing') + 
  scale_x_continuous('Easting') + 
  coord_sf(expand = F, ylim = yLims, xlim = xLims) +
  
g.map.L8.summer <- ggRGB(img = L8_Summer, r = 3, g = 2, b = 1,
                      stretch = 'hist') + 
  geom_sf(data = bufzone, color = 'red', fill = "transparent") +
  geom_sf(data = ptszone, color = 'red') +
  scale_y_continuous('Northing') + 
  scale_x_continuous('Easting') + 
  coord_sf(expand = F, ylim = yLims, xlim = xLims) +
  

# Albedo maps 
g.map.Albedo.winter <- ggplot(Albedo_df %>% filter(Season == 'winter')) +
  geom_raster(aes(x = Easting, y = Northing, fill = Albedo)) +
  geom_sf(data = buf.sub, color = 'red', fill = "transparent") +
  geom_sf(data = pts.sub, color = 'red') +
  scale_fill_viridis_c(limits = ALB_lims, oob = squish, na.value = "transparent") + ## option = "inferno"  +
  coord_sf(expand = F, ylim = yLims, xlim = xLims) +
  theme(legend.position = c(0.1,0.1),
        axis.title = element_blank())

g.map.Albedo.summer <- ggplot(Albedo_df %>% filter(Season == 'summer')) +
  geom_raster(aes(x = Easting, y = Northing, fill = Albedo)) +
  geom_sf(data = buf.sub, color = 'red', fill = "transparent") +
  geom_sf(data = pts.sub, color = 'red') +
  scale_fill_viridis_c(limits = ALB_lims, oob = squish, na.value = "transparent") + ## option = "inferno"  +
  coord_sf(expand = F, ylim = yLims, xlim = xLims) +
  theme(legend.position = c(0.1,0.1),
        axis.title = element_blank())



### ... 



iTime <- '2016-10-07'
g.map.albedo <- ggplot(filter(dat, time == iTime))+
  geom_raster(aes(x = col, y = row, fill = ALB))+
  geom_point(data = filter(dat, idpt == 221, time == iTime),
             aes(x = col, y = row), size = 5, shape = 3, colour = 'cyan')+
  geom_point(data = filter(dat,time == iTime, SNR > minSNR, dist < minDist),
             aes(x = col, y = row), shape = 21, fill = 'grey80', colour = 'grey50', size = 4)+
  scale_fill_viridis_c(limits = ALB_lims, oob = squish, na.value = "transparent")+
  theme(legend.position = c(0.1,0.1))


# SNR
iTime <- '2016-10-07'
g.map.sig2noiseR <- ggplot(filter(dat, time == iTime))+
  geom_raster(aes(x = col, y = row, fill = SNR))+
  geom_point(data = filter(dat,time == iTime, SNR > minSNR, dist < minDist),
             aes(x = col, y = row), shape = 21, fill = 'grey80', colour = 'grey50', size = 4)+
  geom_point(data = filter(dat, idpt == 221, time == iTime),
             aes(x = col, y = row), size = 5, shape = 3, colour = 'cyan')+
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


require(grid)

# prepare output
figW <- 9; figH <- 12; fmt <- 'png'
fullfname <- paste0('textFigures/','Figure1_test','.',fmt)
if(fmt == 'png'){png(fullfname, width = figW, height = figH, units = "in", res = 150)}
if(fmt == 'pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.map.L8.winter, vp = viewport(width = 0.5, height = 0.33, x = 0.0, y = 0.66, just=c(0,0)))
print(g.map.L8.summer, vp = viewport(width = 0.5, height = 0.33, x = 0.5, y = 0.66, just=c(0,0)))
print(g.map.Albedo.winter, vp = viewport(width = 0.5, height = 0.33, x = 0.0, y = 0.33, just=c(0,0)))
print(g.map.Albedo.summer, vp = viewport(width = 0.5, height = 0.33, x = 0.5, y = 0.33, just=c(0,0)))
print(g.map.sig2noiseR, vp = viewport(width = 0.5, height = 0.33, x = 0.0, y = 0, just=c(0,0)))
print(g.plot.ALBvsNDVI, vp = viewport(width = 0.5, height = 0.33, x = 0.5, y = 0, just=c(0,0)))
dev.off()

