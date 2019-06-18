library(ggplot2)
library(sf)
library(grid)
library(scales)
library(RColorBrewer)
library(here)


# load data
load(file = 'dataFigures/redux_data4fig.Rda') # 'GHGbdg','GHGsen', 'aLCS'


# prepare map 
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)




pts_sf <- aLCS %>% 
  st_as_sf(coords = c("GPS_LONG","GPS_LAT")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)

g.map.delta <- ggplot(pts_sf) + 
  geom_sf(data = europe_laea, fill = 'grey50')+
  geom_sf(aes(colour = SWin_Ta_Wm2))+
  scale_colour_gradientn(limits = c(40,120), 
                         colors = rev(brewer.pal(9,'Spectral')), oob = squish)+
  coord_sf(xlim = c(2.5e6,6e6), ylim = c(1.5e6,4.5e6))+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))

ggsave(filename = paste0('DeltaCC_',iYear,'_LbMv','.png'), plot = g.map.delta, path = fpath, width = 12, height = 10)






# 
# fig1 <- ggplot() + 
#   geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), 
#                color = "grey20", fill = "grey90", size = 0.25) +
#   geom_point(aes(x= GPS_LONG, y=GPS_LAT, color = SWin_Ta_Wm2), 
#              data = aLCS , size=0.8, alpha = 0.5) +   
#   scale_colour_gradient2(limits=c(40, 120), 
#                          low="blue", mid= "green", high="red", midpoint=80,  na.value = "grey80", name="(W/m2)") +
#   ggtitle("Ta * SWin") +
#   annotate("text", x=-1.5, y=69, label= paste("mean(sd)= ",round(mean(aLCS$SWin_Ta_Wm2, na.rm=TRUE),2), "?", round(sd(aLCS$SWin_Ta_Wm2, na.rm=TRUE),2), sep="") , col="black" , size=3)




# 
