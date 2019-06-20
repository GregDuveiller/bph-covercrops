library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(grid)
library(scales)
library(RColorBrewer)
library(here)


#### preparation ####


fpath <- 'textFigures/'


# load data
load(file = 'dataFigures/redux_data4fig.Rda') # 'GHGbdg','GHGsen', 'aLCS'


# prepare map 
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)



pts_sf_aLCS <- aLCS %>% 
  st_as_sf(coords = c("GPS_LONG","GPS_LAT")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)

pts_sf_GHGsen <- GHGsen %>% 
  dplyr::rename('2030' = GHGr30, '2100' = GHGr00) %>%
  dplyr::select('POINT_ID', 'GPS_LAT', 'GPS_LONG', '2030', '2100') %>%
  tidyr::gather(key = 'TimeHorizon', value = 'GHGr', c('2030', '2100')) %>%
  st_as_sf(coords = c("GPS_LONG","GPS_LAT")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)

pointSize <- 0.2
landColor <- 'grey60'



#### FIG 1 ####


custom_theme <- theme(legend.position = 'top',
                      legend.key.width = unit(0.8,'in'))

g.map.TransSWin <- ggplot(pts_sf_aLCS) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = SWin_Ta_Wm2), size = pointSize)+
  scale_colour_viridis_c("Ta * SWin",
                         limits = c(40,120), 
                         option = "magma",
                         oob = squish)+
  coord_sf(xlim = c(2.5e6,6e6), ylim = c(1.5e6,4.5e6))+
  custom_theme +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


g.map.BareSoilA <- ggplot(pts_sf_aLCS) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = BareSoil.Albedo), size = pointSize)+
  scale_colour_viridis_c("Bare soil albedo",
                         limits = c(0.05,0.25), 
                         option = "viridis", 
                         oob = squish)+
  coord_sf(xlim = c(2.5e6,6e6), ylim = c(1.5e6,4.5e6))+
  custom_theme +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


g.map.AlbedoChg <- ggplot(pts_sf_aLCS) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = a_dif), size = pointSize)+
  scale_colour_gradientn("Albedo change",
                         limits = c(-0.01,0.01), 
                         colors = rev(brewer.pal(9,'RdBu')), 
                         oob = squish)+
  coord_sf(xlim = c(2.5e6,6e6), ylim = c(1.5e6,4.5e6))+
  custom_theme +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))

g.map.AlbRadFor <- ggplot(pts_sf_aLCS) + 
  geom_sf(data = europe_laea, fill = land.color)+
  geom_sf(aes(colour = RFa_Wm2), size = pointSize)+
  scale_colour_gradientn("Albedo radiative forcing",
                         limits = c(-0.5,0.5), 
                         colors = rev(brewer.pal(9,'RdBu')), 
                         oob = squish)+
  coord_sf(xlim = c(2.5e6,6e6), ylim = c(1.5e6,4.5e6))+
  custom_theme +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


fname <- 'Figure2'
figW <- 10; figH <- 10; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.map.TransSWin, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.5, just=c(0,0)))
print(g.map.BareSoilA, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.5, just=c(0,0)))
print(g.map.AlbedoChg, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.0, just=c(0,0)))
print(g.map.AlbRadFor, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.03, "npc"), y = unit(0.94, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("b")), x = unit(0.53, "npc"), y = unit(0.94, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("c")), x = unit(0.03, "npc"), y = unit(0.49, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("d")), x = unit(0.53, "npc"), y = unit(0.49, "npc"), gp=gpar(fontsize=18))


dev.off()




#### FIG 2 ####

lab <- expression(Delta* "Soil fluxes (Mg CO"[2]*"eq Ha"^-1*")")   ##labels

g.projection <- ggplot(GHGbdg, aes(x = year, y = dSOC, 
                                   colour = scenarios, fill = scenarios)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin = dSOC_2q, ymax = dSOC_1q), linetype = 0, alpha = 0.2) +
  scale_y_continuous(lab, limits = c(-30, 20)) +
  scale_x_continuous('Year') +
  scale_fill_discrete('') +
  scale_color_discrete('') +
  geom_hline(yintercept=0, linetype="solid", alpha=0.4) +
  geom_vline(xintercept=2030, linetype="dashed", alpha=0.2) +
  geom_vline(xintercept=2100, linetype="dashed", alpha=0.2) +
  theme_bw()+ 
  theme(legend.position = c(0.75,0.85),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g.map.AlbRelImp <- ggplot(pts_sf_GHGsen) + 
  geom_sf(data = europe_laea, fill = landColor) +
  geom_sf(aes(colour = GHGr), size = pointSize) +
  scale_colour_viridis_c("Ratio of albedo forcing",
                         limits = c(0, 1),
                         option = 'plasma', 
                         oob = squish) +
  facet_wrap(~TimeHorizon, ncol = 2) +
  coord_sf(xlim = c(2.5e6,6e6), ylim = c(1.5e6,4.5e6)) +
  theme(legend.position = 'top',
        legend.key.width = unit(0.9,'in')) +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))



fname <- 'Figure3'
figW <- 14; figH <- 5; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.projection, vp = viewport(width = 0.4, height = 1, x = 0.0, y = 0, just=c(0,0)))
print(g.map.AlbRelImp, vp = viewport(width = 0.6, height = 1, x = 0.4, y = 0, just=c(0,0)))
dev.off()

