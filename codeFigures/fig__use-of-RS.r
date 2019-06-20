library(raster)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(dplyr)
library(sf)
library(RStoolbox)
library(here)

pt_ID <- '28761622'


buf <- sf::st_read('dataFigures/dat4figLUCASpt/arable_buffer.shp', quiet = TRUE)


L8_Winter = stack(paste0('dataFigures/dat4figLUCASpt/pt_',pt_ID,'/L8_clip_winter_',pt_ID,'.tif'))
L8_Summer = stack(paste0('dataFigures/dat4figLUCASpt/pt_',pt_ID,'/L8_clip_summer_',pt_ID,'.tif'))


Albedo <- stack(list(winter = raster(paste0('dataFigures/dat4figLUCASpt/pt_',pt_ID,'/albedo_winter_',pt_ID,'.tif')),
                     summer = raster(paste0('dataFigures/dat4figLUCASpt/pt_',pt_ID,'/albedo_summer_',pt_ID,'.tif'))))
Albedo_df <- as.data.frame(0.0001 * Albedo, xy = T, long = T)
colnames(Albedo_df) <- c('Easting','Northing','Season','Albedo')


laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
bufzone <- buf %>% filter(POINT_ID == pt_ID) %>%
  sf::st_transform(laes_prj)


xLims <- c(-6.15,-6.05)
yLims <- c(36.25, 36.35)

# RGB plots
g.map.L8.winter <- ggRGB(img = L8_Winter, r = 3, g = 2, b = 1,
                      stretch = 'hist') + 
  geom_sf(data = bufzone, color = 'red') +
  scale_y_continuous('Northing') + 
  scale_x_continuous('Easting') +
  coord_sf(xlim = xLims, ylim = yLims)

g.map.L8.summer <- ggRGB(img = L8_Summer, r = 3, g = 2, b = 1,
                      stretch = 'hist') + 
  scale_y_continuous('Northing') + 
  scale_x_continuous('Easting') +
  coord_cartesian(xlim = xLims, ylim = yLims)


# Albedo maps 
g.map.Albedo.winter <- ggplot(Albedo_df %>% filter(Season == 'winter')) +
  geom_raster(aes(x = Easting, y = Northing, fill = Albedo)) +
  geom_sf(data = bufzone, color = 'red', fill = "transparent") +
  scale_fill_viridis_c(limits=c(0,0.02), oob=squish, na.value = "transparent") + ## option = "inferno"  +
  #coord_sf(xlim = xLims, ylim = yLims) +
  theme(legend.position = c(0.1,0.1))

g.map.Albedo.summer <- ggplot(Albedo_df %>% filter(Season == 'summer')) +
  geom_raster(aes(x = Easting, y = Northing, fill = Albedo)) +
  scale_fill_viridis_c(limits=c(0,0.02), oob=squish, na.value = "transparent") + ## option = "inferno"  +
  coord_cartesian(xlim = xLims, ylim = yLims) +
  theme(legend.position = c(0.1,0.1))




ggplot(Albedo_df) +
  geom_raster(aes(x, y, fill = albedo)) +
  scale_fill_viridis_c(limits=c(0,200), oob=squish, na.value = "transparent") + ## option = "inferno"  +
  # coord_sf(xlim=c(2.5e6,6e6),ylim=c(1.5e6,5.4e6))+
  xlab("") + ylab("")+
  facet_wrap(~season)+  theme_bw()+
  labs(fill = expression(atop(
    "Albedo" )))+
  # theme_Publication()+
  theme(panel.grid.minor=element_blank(),
        strip.text = element_text(size = 20),
        legend.key.width = unit(1.5,'cm'),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20)
  )
