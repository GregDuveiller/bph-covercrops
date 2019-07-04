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



# prepare map 
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)


# load first dataset, with normal plants
load(file = 'dataFigures/redux_data4fig_normal.Rda') # 'GHGbdg','GHGsen', 'aLCS'

# make sf points for the general maps 
pts_sf_aLCS <- aLCS %>% 
  st_as_sf(coords = c("GPS_LONG","GPS_LAT")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)

# make sf points for the maps showing projected scenario with normal plants
GHGsen_1 <- GHGsen %>% 
  dplyr::rename('2030' = GHGr30, '2100' = GHGr00) %>%
  dplyr::select('POINT_ID', 'GPS_LAT', 'GPS_LONG', '2030', '2100') %>%
  dplyr::mutate(plantType = factor('Normal', levels = c('Normal','Mutant'))) %>%
  tidyr::gather(key = 'TimeHorizon', value = 'GHGr', c('2030', '2100'))

# add category of normal plants
GHGbdg_1 <- GHGbdg %>%  mutate(plantType = factor('Normal', levels = c('Normal','Mutant')))

# load second dataset, with mutant plants
load(file = 'dataFigures/redux_data4fig_mutant.Rda') # 'GHGbdg','GHGsen', 'aLCS'

# make sf points for the maps showing projected scenario with mutant plants
GHGsen_2 <- GHGsen %>% 
  dplyr::rename('2030' = GHGr30, '2100' = GHGr00) %>%
  dplyr::select('POINT_ID', 'GPS_LAT', 'GPS_LONG', '2030', '2100') %>%
  dplyr::mutate(plantType = factor('Mutant', levels = c('Normal','Mutant'))) %>%
  tidyr::gather(key = 'TimeHorizon', value = 'GHGr', c('2030', '2100')) 

# add category of mutant plants
GHGbdg_2 <- GHGbdg %>% mutate(plantType = factor('Mutant', levels = c('Normal','Mutant')))

# combine datasets...
GHGbdg <- bind_rows(GHGbdg_1, GHGbdg_2)
pts_sf_GHGsen <- bind_rows(GHGsen_1, GHGsen_2) %>%
  st_as_sf(coords = c("GPS_LONG","GPS_LAT")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)



# set parameters for plots
pointSize <- 0.2
landColor <- 'grey60'
xLims <- c(2.5e6,6e6)
yLims <- c(1.5e6,4.5e6)


#### FIG 1 ####


custom_theme <- theme(legend.position = 'top',
                      legend.key.width = unit(0.8,'in'),
                      plot.tag = element_text(face = "bold"))

anot.TransSWin <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS$SWin_Ta_Wm2, na.rm=TRUE),4))
                         %+-% .(round(sd(pts_sf_aLCS$SWin_Ta_Wm2, na.rm=TRUE),4)))

g.map.TransSWin <- ggplot(pts_sf_aLCS) + 
  geom_sf(data = europe_laea, fill = landColor) +
  geom_sf(aes(colour = SWin_Ta_Wm2), size = pointSize) +
  scale_colour_viridis_c("Ta * SWin",
                         limits = c(40,120), 
                         option = "magma",
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  labs(tag = 'a', caption = anot.TransSWin) + 
  custom_theme +  
  #geom_text(data = data.frame(x = 2.9e6, y = 4.4e6, label = anot)) +
  #annotate(geom = "text", x = 2.9e6, y = 4.4e6, label = anot) +
  #annotate(geom = "label", x = 2.9e6, y = 4.4e6, label = anot) +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))

  
anot.BareSoilA <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS$BareSoil.Albedo, na.rm=TRUE),4)) 
                         %+-% .(round(sd(pts_sf_aLCS$BareSoil.Albedo, na.rm=TRUE),4)))

g.map.BareSoilA <- ggplot(pts_sf_aLCS) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = BareSoil.Albedo), size = pointSize)+
  scale_colour_viridis_c("Bare soil albedo",
                         limits = c(0.05,0.25), 
                         option = "viridis", 
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  labs(tag = 'b', caption = anot.BareSoilA) + 
  custom_theme +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))



anot.AlbedoChg <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS$a_dif, na.rm=TRUE),4)) 
                         %+-% .(round(sd(pts_sf_aLCS$a_dif, na.rm=TRUE),4)))

g.map.AlbedoChg <- ggplot(pts_sf_aLCS) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = a_dif), size = pointSize)+
  scale_colour_gradientn("Albedo change",
                         limits = c(-0.01,0.01), 
                         colors = brewer.pal(9,'PiYG'), 
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  labs(tag = 'c', caption = anot.AlbedoChg) + 
  custom_theme +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))



anot.AlbRadFor <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS$RFa_Wm2, na.rm=TRUE),4)) 
                         %+-% .(round(sd(pts_sf_aLCS$RFa_Wm2, na.rm=TRUE),4)))

g.map.AlbRadFor <- ggplot(pts_sf_aLCS) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = RFa_Wm2), size = pointSize)+
  scale_colour_gradientn("Albedo radiative forcing",
                         limits = c(-0.5,0.5), 
                         colors = brewer.pal(9,'RdBu'), 
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  labs(tag = 'd', caption = anot.AlbRadFor) + 
  custom_theme +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


fname <- 'Figure1'
figW <- 10; figH <- 11; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.map.TransSWin, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.5, just=c(0,0)))
print(g.map.BareSoilA, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.5, just=c(0,0)))
print(g.map.AlbedoChg, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.0, just=c(0,0)))
print(g.map.AlbRadFor, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.0, just=c(0,0)))

# grid.text(expression(bold("a")), x = unit(0.03, "npc"), y = unit(0.96, "npc"), gp=gpar(fontsize=18))
# grid.text(expression(bold("b")), x = unit(0.53, "npc"), y = unit(0.96, "npc"), gp=gpar(fontsize=18))
# grid.text(expression(bold("c")), x = unit(0.03, "npc"), y = unit(0.46, "npc"), gp=gpar(fontsize=18))
# grid.text(expression(bold("d")), x = unit(0.53, "npc"), y = unit(0.46, "npc"), gp=gpar(fontsize=18))


dev.off()




#### FIG 2 ####

scen.cols <- c('CO2_soil' = 'tan4', 'N2O_dir' = 'goldenrod', 'albedo' = 'cornflowerblue')
scen.lbls <- c('CO2_soil' = 'CO2', 'N2O_dir' = 'N2O', 'albedo' = 'albedo')

lab <- expression(Delta* "Soil fluxes (Mg CO"[2]*"eq Ha"^-1*")")   ##labels

g.futprojection <- ggplot(GHGbdg, aes(x = year, y = dSOC, 
                                   colour = scenarios, fill = scenarios)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin = dSOC_2q, ymax = dSOC_1q), linetype = 0, alpha = 0.2) +
  facet_wrap(~plantType, nrow = 2) +
  scale_y_continuous(lab, limits = c(-30, 20)) +
  scale_x_continuous('Year') +
  scale_fill_manual('', values = scen.cols, labels = scen.lbls) +
  scale_color_manual('', values = scen.cols, labels = scen.lbls) +
  geom_hline(yintercept = 0, linetype = "solid", alpha = 0.4) +
  geom_vline(xintercept = 2030, linetype = "dashed", alpha = 0.2) +
  geom_vline(xintercept = 2100, linetype = "dashed", alpha = 0.2) +
  labs(tag = 'a') +
  theme_minimal()+ 
  theme(legend.position = c(0.78,0.93),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = 'bold', size = '12'),
        plot.tag = element_text(face = "bold"))


g.map.AlbRelImp <- ggplot(pts_sf_GHGsen) + 
  geom_sf(data = europe_laea, fill = landColor) +
  geom_sf(aes(colour = GHGr), size = pointSize) +
  scale_colour_viridis_c("Ratio of\n albedo\n forcing",
                         limits = c(0, 1),
                         option = 'plasma', 
                         oob = squish) +
  facet_grid(plantType ~ TimeHorizon) +
  coord_sf(xlim = xLims, ylim = yLims) +
  labs(tag = 'b') +
  theme(legend.position = 'right',
        legend.key.height = unit(0.9,'in'),
        strip.text = element_text(face = 'bold', size = '12'),
        plot.tag = element_text(face = "bold")) +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))



fname <- 'Figure2'
figW <- 15; figH <- 9; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.futprojection, vp = viewport(width = 0.30, height = 1, x = 0.00, y = 0, just=c(0,0)))
print(g.map.AlbRelImp, vp = viewport(width = 0.70, height = 1, x = 0.30, y = 0, just=c(0,0)))
dev.off()

