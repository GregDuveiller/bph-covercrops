library(ggplot2)
library(sf)
library(grid)
library(scales)
library(RColorBrewer)
library(here)


fpath <- 'textFigures/'


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



custom_theme <- theme(legend.position = 'top',
                      legend.key.width = unit(0.8,'in'))

pointSize <- 0.2

g.map.TransSWin <- ggplot(pts_sf) + 
  geom_sf(data = europe_laea, fill = 'grey50')+
  geom_sf(aes(colour = SWin_Ta_Wm2), size = pointSize)+
  scale_colour_gradientn("Ta * SWin",
                         limits = c(40,120), 
                         colors = rev(brewer.pal(9,'Spectral')), oob = squish)+
  coord_sf(xlim = c(2.5e6,6e6), ylim = c(1.5e6,4.5e6))+
  custom_theme +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


g.map.BareSoilA <- ggplot(pts_sf) + 
  geom_sf(data = europe_laea, fill = 'grey50')+
  geom_sf(aes(colour = BareSoil.Albedo), size = pointSize)+
  scale_colour_viridis_c("Bare soil albedo",
                         limits = c(0.03,0.32), 
                         option = "viridis", oob = squish)+
  coord_sf(xlim = c(2.5e6,6e6), ylim = c(1.5e6,4.5e6))+
  custom_theme +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


g.map.AlbedoChg <- ggplot(pts_sf) + 
  geom_sf(data = europe_laea, fill = 'grey50')+
  geom_sf(aes(colour = a_dif), size = pointSize)+
  scale_colour_gradientn("Albedo change",
                         limits = c(-0.03,0.03), 
                         colors = rev(brewer.pal(9,'RdBu')), oob = squish)+
  coord_sf(xlim = c(2.5e6,6e6), ylim = c(1.5e6,4.5e6))+
  custom_theme +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


g.map.AlbRadFor <- ggplot(pts_sf) + 
  geom_sf(data = europe_laea, fill = 'grey50')+
  geom_sf(aes(colour = RFa_Wm2), size = pointSize)+
  scale_colour_gradientn("Albedo radiative forcing",
                         limits = c(-1.5,1.5), 
                         colors = rev(brewer.pal(9,'RdBu')), oob = squish)+
  coord_sf(xlim = c(2.5e6,6e6), ylim = c(1.5e6,4.5e6))+
  custom_theme +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


fname <- 'Figure1'
figW <- 12; figH <- 10; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.map.TransSWin, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.5, just=c(0,0)))
print(g.map.BareSoilA, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.5, just=c(0,0)))
print(g.map.AlbedoChg, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.0, just=c(0,0)))
print(g.map.AlbRadFor, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.0, just=c(0,0)))
dev.off()


#ggsave(filename = paste0('DeltaCC_',iYear,'_LbMv','.png'), plot = g.map.TaSWin, path = fpath, width = 12, height = 10)






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
