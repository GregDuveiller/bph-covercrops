library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(grid)
library(scales)
library(RColorBrewer)
library(here)

################################################################################
#### data preparation #### ----



fpath <- 'textFigures/'

# some general setups...
ccTypes <- c('Normal', 'Mutant')
ccTypes_lbls <- c('Normal cover crop', 'Chlorophyll-deficient mutant')

# prepare map 
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
vpath <- '../../AncillaryDatasets/WorldVector/'

world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)


### load first dataset, with normal plants ----
load(file = 'dataFigures/data4scen1_normalCC-noSNOW.Rda') 
# (loads a list named 'dat' with several objects 'GHGbdg','GHGsen', 'aLCS', etc)

# make sf points for the general maps 
pts_sf_aLCS_1 <- dat$aLCS %>% 
  st_as_sf(coords = c("GPS_LONG","GPS_LAT")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)

# make sf points for the maps showing projected scenario with normal plants
GHGsen_1 <- dat$GHGsen %>% 
  dplyr::rename('2030' = GHGr30, '2100' = GHGr00) %>%
  dplyr::select('POINT_ID', 'GPS_LAT', 'GPS_LONG', '2030', '2100') %>%
  dplyr::mutate(ccType = factor('Normal', levels = ccTypes,
                                labels = ccTypes_lbls)) %>%
  tidyr::gather(key = 'TimeHorizon', value = 'GHGr', c('2030', '2100')) %>%
  dplyr::mutate(snow = F)

# add category of normal plants
GHGbdg_1 <- dat$GHGbdg %>%  
  mutate(ccType = factor('Normal', levels = ccTypes, labels = ccTypes_lbls)) %>%
  mutate(snow = F)


## sort data per country level

# wrapper function to grab the data per country
get.df <- function(yr = '2100', ccType = 'Normal'){
  
  bph.sub <- dat$bph %>% 
    dplyr::select('POINT_ID', paste0('yr_', yr)) %>%
    dplyr::rename('BPH' = paste0('yr_', yr))
  
  bgc.sub <- dat$bgc %>% 
    dplyr::select('POINT_ID', paste0('yr_', yr)) %>%
    dplyr::rename('BGC' = paste0('yr_', yr)) 
  
  country.dat <- pts_sf_aLCS_1 %>%
    as.data.frame() %>%
    dplyr::transmute(POINT_ID = POINT_ID,
                     Country = factor(NUT0, 
                                      levels = levels(dat$arableLand$Country))) %>%
    right_join(bph.sub, by = c('POINT_ID')) %>%
    right_join(bgc.sub, by = c('POINT_ID')) %>%
    group_by(Country) %>%
    dplyr::summarise(BPH_mean = mean(BPH, na.rm = T), # these are in Mg ha-1
                     BGC_mean = mean(BGC, na.rm = T)) %>%
    left_join(dat$arableLand, by = 'Country') %>%
    filter(!is.na(BPH_mean) & !is.na(BGC_mean)) %>%
    mutate(BPH = BPH_mean * Area_Mha, # so now in Tg 
           BGC = BGC_mean * Area_Mha) %>%
    select(-BPH_mean, -BGC_mean) %>%
    tidyr::pivot_longer(cols = c('BPH', 'BGC'),
                        names_to = 'Type', values_to = 'CO2.eq') %>%
    mutate(yr = yr, 
           ccType = factor(ccType, levels = ccTypes, labels = ccTypes_lbls))
  
  return(country.dat)}

# get the needed data for the normal case
dat.perCountry_1 <- bind_rows(
  get.df(yr = '2030', ccType = 'Normal'),
  get.df(yr = '2100', ccType = 'Normal')) %>%
  mutate(snow = F)



### load third dataset, with mutant plants ----
load(file = 'dataFigures/data4scen3_brightCC-noSNOW.Rda') 
# (loads a list named 'dat' with several objects 'GHGbdg','GHGsen', 'aLCS', etc)

# make sf points for the maps showing projected scenario with mutant plants
GHGsen_3 <- dat$GHGsen %>% 
  dplyr::rename('2030' = GHGr30, '2100' = GHGr00) %>%
  dplyr::select('POINT_ID', 'GPS_LAT', 'GPS_LONG', '2030', '2100') %>%
  dplyr::mutate(ccType = factor('Mutant', 
                                levels = ccTypes,
                                labels = ccTypes_lbls)) %>%
  tidyr::gather(key = 'TimeHorizon', value = 'GHGr', c('2030', '2100')) %>%
  mutate(snow = F)

# add category of mutant plants
GHGbdg_3 <- dat$GHGbdg %>% 
  mutate(ccType = factor('Mutant', levels = ccTypes, labels = ccTypes_lbls)) %>%
  mutate(snow = F)

# get the needed data for the normal case
dat.perCountry_3 <- bind_rows(
  get.df(yr = '2030', ccType = 'Mutant'),
  get.df(yr = '2100', ccType = 'Mutant')) %>%
  mutate(snow = F)




### load second dataset, with normal plants + snow ----
load(file = 'dataFigures/data4scen2_normalCC-withSNOW.Rda') 
# (loads a list named 'dat' with several objects 'GHGbdg','GHGsen', 'aLCS', etc)


# make sf points for the general maps 
pts_sf_aLCS_2 <- dat$aLCS %>% 
  st_as_sf(coords = c("GPS_LONG","GPS_LAT")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)

# make sf points for the maps showing projected scenario with normal plants
GHGsen_2 <- dat$GHGsen %>% 
  dplyr::rename('2030' = GHGr30, '2100' = GHGr00) %>%
  dplyr::select('POINT_ID', 'GPS_LAT', 'GPS_LONG', '2030', '2100') %>%
  dplyr::mutate(ccType = factor('Normal', levels = ccTypes,
                                labels = ccTypes_lbls)) %>%
  tidyr::gather(key = 'TimeHorizon', value = 'GHGr', c('2030', '2100')) %>%
  dplyr::mutate(snow = T)

# add category for normal plant + snow
GHGbdg_2 <- dat$GHGbdg %>%  
  mutate(ccType = factor('Normal', levels = ccTypes, labels = ccTypes_lbls)) %>%
  mutate(snow = T)

# get the needed data for the normal case
dat.perCountry_2 <- bind_rows(
  get.df(yr = '2030', ccType = 'Normal'),
  get.df(yr = '2100', ccType = 'Normal')) %>%
  mutate(snow = T)



### load fourth dataset, with normal plants + snow ----
load(file = 'dataFigures/data4scen4_mutantCC-withSNOW.Rda') 
# (loads a list named 'dat' with several objects 'GHGbdg','GHGsen', 'aLCS', etc)


# make sf points for the general maps 
pts_sf_aLCS_4 <- dat$aLCS %>% 
  st_as_sf(coords = c("GPS_LONG","GPS_LAT")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)


# make sf points for the maps showing projected scenario with mutant plants + snow
GHGsen_4 <- dat$GHGsen %>% 
  dplyr::rename('2030' = GHGr30, '2100' = GHGr00) %>%
  dplyr::select('POINT_ID', 'GPS_LAT', 'GPS_LONG', '2030', '2100') %>%
  dplyr::mutate(ccType = factor('Mutant', 
                                levels = ccTypes,
                                labels = ccTypes_lbls)) %>%
  tidyr::gather(key = 'TimeHorizon', value = 'GHGr', c('2030', '2100')) %>%
  mutate(snow = T)


# add category of mutant plants + snow
GHGbdg_4 <- dat$GHGbdg %>% 
  mutate(ccType = factor('Mutant', levels = ccTypes, labels = ccTypes_lbls)) %>%
  mutate(snow = T)

# get the needed data for the normal case
dat.perCountry_4 <- bind_rows(
  get.df(yr = '2030', ccType = 'Mutant'),
  get.df(yr = '2100', ccType = 'Mutant')) %>%
  mutate(snow = T)





### combine datasets... -----

GHGbdg <- bind_rows(GHGbdg_1, GHGbdg_3, GHGbdg_2, GHGbdg_4)

pts_sf_GHGsen <- bind_rows(GHGsen_1, GHGsen_3, GHGsen_2, GHGsen_4) %>%
  st_as_sf(coords = c("GPS_LONG","GPS_LAT")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)

dat.perCountry <- bind_rows(dat.perCountry_1, dat.perCountry_3,
                            dat.perCountry_2, dat.perCountry_4)






################################################################################
#### Figures #### ----


# set parameters for plots
pointSize <- 0.2
landColor <- 'grey60'
xLims <- c(2.5e6,6e6)
yLims <- c(1.5e6,4.5e6)

custom_theme_maps <- theme(legend.position = 'bottom',
                           legend.key.width = unit(0.8,'in'),
                           plot.tag = element_text(face = "bold"))

#### FIG 1 #### ----



anot.TransSWin <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_1$SWin_Ta_Wm2, na.rm=TRUE),4))
                         %+-% .(round(sd(pts_sf_aLCS_1$SWin_Ta_Wm2, na.rm=TRUE),4)))

g.map.TransSWin <- ggplot(pts_sf_aLCS_1) + 
  geom_sf(data = europe_laea, fill = landColor) +
  geom_sf(aes(colour = SWin_Ta_Wm2), size = pointSize) +
  scale_colour_viridis_c("Ta * SWin",
                         limits = c(40,120), 
                         option = "magma",
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  # labs(tag = 'a', caption = anot.TransSWin) + 
  labs(caption = anot.TransSWin) + 
  custom_theme_maps +  
  #geom_text(data = data.frame(x = 2.9e6, y = 4.4e6, label = anot)) +
  #annotate(geom = "text", x = 2.9e6, y = 4.4e6, label = anot) +
  #annotate(geom = "label", x = 2.9e6, y = 4.4e6, label = anot) +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


anot.BareSoilA <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_1$BareSoil.Albedo, na.rm=TRUE),4)) 
                         %+-% .(round(sd(pts_sf_aLCS_1$BareSoil.Albedo, na.rm=TRUE),4)))

g.map.BareSoilA <- ggplot(pts_sf_aLCS_1) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = BareSoil.Albedo), size = pointSize)+
  scale_colour_viridis_c("Bare soil albedo",
                         limits = c(0.05,0.25), 
                         option = "viridis", 
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  # labs(tag = 'b', caption = anot.BareSoilA) + 
  labs(caption = anot.BareSoilA) + 
  custom_theme_maps +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))



anot.AlbedoChg <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_1$a_dif, na.rm=TRUE),4)) 
                         %+-% .(round(sd(pts_sf_aLCS_1$a_dif, na.rm=TRUE),4)))

g.map.AlbedoChg <- ggplot(pts_sf_aLCS_1) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = a_dif), size = pointSize)+
  scale_colour_gradientn("Albedo change",
                         limits = c(-0.01,0.01), 
                         colors = brewer.pal(9,'PiYG'), 
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  # labs(tag = 'c', caption = anot.AlbedoChg) + 
  labs(caption = anot.AlbedoChg) + 
  custom_theme_maps +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))



anot.AlbRadFor <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_1$RFa_Wm2,
                                                   na.rm = TRUE),4)) 
                         %+-% .(round(sd(pts_sf_aLCS_1$RFa_Wm2, na.rm = TRUE), 4)))

g.map.AlbRadFor <- ggplot(pts_sf_aLCS_1) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = RFa_Wm2), size = pointSize)+
  scale_colour_gradientn("Albedo radiative forcing",
                         limits = c(-0.5,0.5), 
                         colors = brewer.pal(9,'RdBu'), 
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  # labs(tag = 'd', caption = anot.AlbRadFor) + 
  labs(caption = anot.AlbRadFor) + 
  custom_theme_maps +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


fname <- 'Figure1_Bph-effect'
figW <- 10; figH <- 11; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.map.TransSWin, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.5, just=c(0,0)))
print(g.map.BareSoilA, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.5, just=c(0,0)))
print(g.map.AlbedoChg, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.0, just=c(0,0)))
print(g.map.AlbRadFor, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.07, "npc"), y = unit(0.96, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("b")), x = unit(0.57, "npc"), y = unit(0.96, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("c")), x = unit(0.07, "npc"), y = unit(0.46, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("d")), x = unit(0.57, "npc"), y = unit(0.46, "npc"), gp=gpar(fontsize=18))

dev.off()




#### FIG 2 #### ----

snow_flag <- F

scen.cols <- c('CO2_soil' = 'firebrick3',   # 'tan4', 
               'N2O_dir' ='goldenrod',         # 'goldenrod', 
               'albedo' = 'cornflowerblue')
scen.lbls <- c('CO2_soil' = bquote('CO'[2]), 
               'N2O_dir' = bquote('N'[2]*'O'), 
               'albedo' = 'Albedo')

lab <- expression(Delta* "Soil fluxes (Mg CO"[2]*"e ha"^-1*")")   ##labels

# add dummy title on top for projections
GHGbdg$Simulation <- 'Projected temporal trends'

g.futprojection <- ggplot(GHGbdg %>% filter(snow == snow_flag),
                          aes(x = year, y = dSOC, 
                                      colour = scenarios, fill = scenarios)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin = dSOC_2q, ymax = dSOC_1q), linetype = 0, alpha = 0.2) +
  facet_grid(ccType~Simulation) +
  scale_y_continuous(lab, limits = c(-30, 20)) +
  scale_fill_manual('', values = scen.cols, labels = scen.lbls) +
  scale_color_manual('', values = scen.cols, labels = scen.lbls) +
  geom_hline(yintercept = 0, linetype = "solid", colour = 'grey10') +
  geom_vline(xintercept = 2030, linetype = "dashed", colour = 'grey50') +
  geom_vline(xintercept = 2100, linetype = "dashed", colour = 'grey50') +
  # labs(tag = 'a') +
  theme(legend.position = c(0.78,0.92),
        #axis.line = element_line(colour = 'gray20'),
        axis.ticks = element_line(colour = 'gray20'),
        axis.title.x = element_blank(),
        panel.border = element_rect (fill = 'NA', colour = 'gray20'),
        panel.background = element_rect (fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = 'bold', size = '12'),
        plot.tag = element_text(face = "bold"))

pts_sf_GHGsen$TimeHorizonLbl <- paste('Spatial distribution for time horizon:', pts_sf_GHGsen$TimeHorizon)

g.map.AlbRelImp <- ggplot(pts_sf_GHGsen %>% filter(snow == snow_flag)) + 
  geom_sf(data = europe_laea, fill = landColor) +
  geom_sf(aes(colour = GHGr), size = pointSize) +
  # geom_point(aes(colour = GHGr, 
  #                x = st_coordinates(geometry)[,1],
  #                y = st_coordinates(geometry)[,2]),
  #            size = pointSize) +
  scale_colour_viridis_c("Ratio of\n albedo\n forcing",
                         limits = c(0, 1),
                         option = 'plasma', 
                         oob = squish) +
  facet_grid(ccType ~ TimeHorizonLbl) +
  coord_sf(xlim = xLims, ylim = yLims) +
  # labs(tag = 'b') +
  theme(legend.position = 'right',
        legend.key.height = unit(0.9,'in'),
        axis.title = element_blank(),
        strip.text = element_text(face = 'bold', size = '12'),
        plot.tag = element_text(face = "bold")) +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))



fname <- 'Figure2_Projections'; if(snow_flag == T){ fname <- paste0(fname + '_snow')}
figW <- 15; figH <- 8; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.futprojection, vp = viewport(width = 0.30, height = 1, x = 0.00, y = 0, just=c(0,0)))
print(g.map.AlbRelImp, vp = viewport(width = 0.70, height = 1, x = 0.30, y = 0, just=c(0,0)))


grid.text(expression(bold("a")), x = unit(0.06, "npc"), y = unit(0.92, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("b")), x = unit(0.36, "npc"), y = unit(0.92, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("c")), x = unit(0.65, "npc"), y = unit(0.92, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("d")), x = unit(0.06, "npc"), y = unit(0.46, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("e")), x = unit(0.36, "npc"), y = unit(0.46, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("f")), x = unit(0.65, "npc"), y = unit(0.46, "npc"), gp=gpar(fontsize=18))


dev.off()




#### FIG 3 #### ----

effects.cols <- c('BGC' = 'coral', 
                  'BPH' = 'cornflowerblue')
effects.lbls <- c('BGC' = bquote('Biogeochemical (CO'[2]*'+ N'[2]*'O)'), 
                  'BPH' = 'Biogeophysical (albedo)')

dat.perCountry$yr_lbl <- paste('Time horizon:', dat.perCountry$yr)

tot.perCountry <- dat.perCountry %>% 
  group_by(ccType, yr_lbl) %>% 
  summarise(TotalMit = round(sum(CO2.eq), digits = 0)) %>%
  mutate(x = 12.5, y = -550)

g.country <- ggplot(dat.perCountry) +
  geom_bar(aes(x = reorder(Country, Area_Mha), y = CO2.eq, fill = Type),
          stat = 'identity', position = 'stack') +
  geom_hline(yintercept = 0, linetype = "solid", colour = 'grey10') +
  coord_polar(theta = "x", direction = -1) +
  geom_label(data = tot.perCountry, size = 3.5,
             aes(x = x, y = y, 
                 label = paste('Total mitigation potential:\n', TotalMit, 'Tg CO2e'))) +
                 #label = bquote("Total mitigation potential:\n" ~.(TotalMit) ~ "Tg COe"))) +
  facet_grid(ccType ~ yr_lbl) +
  scale_y_continuous(limits = c(-700, 0)) +
  scale_x_discrete('') + 
  scale_fill_manual('Type of effect:', 
                      values = effects.cols, 
                      labels = effects.lbls) +
  theme(legend.position = 'top',
        strip.text = element_text(face = 'bold', size = '12')) +
  ylab(expression("Mitigation potential (Tg CO"[2]*"e ha"^-1*")")) +
  labs(caption = 'Note: there are insufficient data points for LU, MT, CY and HR.')

fname <- 'Figure3_perCountry'
figW <- 8; figH <- 8; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)

if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}

print(g.country, vp = viewport(width = 1, height = 1, x = 0.00, y = 0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.14, "npc"), y = unit(0.85, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("b")), x = unit(0.55, "npc"), y = unit(0.85, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("c")), x = unit(0.14, "npc"), y = unit(0.44, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("d")), x = unit(0.55, "npc"), y = unit(0.44, "npc"), gp=gpar(fontsize=18))

dev.off()


#### FIG S6 #### ----



anot.AlbedoChg <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_2$a_dif, na.rm=TRUE),4)) 
                         %+-% .(round(sd(pts_sf_aLCS_2$a_dif, na.rm=TRUE),4)))

g.map.AlbedoChg <- ggplot(pts_sf_aLCS_2) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = a_dif), size = pointSize)+
  scale_colour_gradientn("Albedo change",
                         limits = c(-0.01,0.01), 
                         colors = brewer.pal(9,'PiYG'), 
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  # labs(tag = 'a', caption = anot.AlbedoChg) + 
  labs(caption = anot.AlbedoChg) + 
  custom_theme_maps +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))






anot.AlbRadFor <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_2$RFa_Wm2,
                                                   na.rm = TRUE),4)) 
                         %+-% .(round(sd(pts_sf_aLCS_2$RFa_Wm2, na.rm = TRUE), 4)))

g.map.AlbRadFor <- ggplot(pts_sf_aLCS_2) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = RFa_Wm2), size = pointSize)+
  scale_colour_gradientn("Albedo radiative forcing",
                         limits = c(-0.5,0.5), 
                         colors = brewer.pal(9,'RdBu'), 
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  # labs(tag = 'b', caption = anot.AlbRadFor) + 
  labs(caption = anot.AlbRadFor) + 
  custom_theme_maps +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))





dat.perCountry_2$yr_lbl <- paste('Time horizon:', dat.perCountry_2$yr)

tot.perCountry <- dat.perCountry_2 %>% 
  group_by(ccType, yr_lbl) %>% 
  summarise(TotalMit = round(sum(CO2.eq), digits = 0))

g.country <- ggplot(dat.perCountry_2) +
  geom_bar(aes(x = reorder(Country, Area_Mha), y = CO2.eq, fill = Type), 
           stat = 'identity', position = 'stack') + 
  geom_hline(yintercept = 0, linetype = "solid", colour = 'grey10') +
  coord_polar(theta = "x", direction = -1) +
  geom_label(data = tot.perCountry, size = 3.5,
             aes(x = 12.5, y = -450,
                 label = paste('Total mitigation potential:\n', TotalMit, 'Tg CO2e'))) +
  facet_grid(ccType ~ yr_lbl) +
  scale_y_continuous(limits = c(-700, 100)) +
  scale_x_discrete('') + 
  scale_fill_manual('Type of effect:', 
                    values = effects.cols, 
                    labels = effects.lbls) +
  theme(legend.position = 'top',
        plot.tag = element_text(face = "bold"),
        strip.text = element_text(face = 'bold', size = '12')) +
  # labs(tag = 'c', caption = 'Note: there are insufficient data points for LU, MT, CY and HR.') +
  labs(caption = 'Note: there are insufficient data points for LU, MT, CY and HR.') +
  ylab(expression("Mitigation potential (Tg CO"[2]*"e ha"^-1*")")) 
  


fname <- 'FigureS6_NormalCCwithSNOW'
figW <- 10; figH <- 12; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}

h.t <- 0.48
print(g.map.AlbedoChg, vp = viewport(width = 0.5, height = h.t, x = 0.0, y = 1 - h.t, just=c(0,0)))
print(g.map.AlbRadFor, vp = viewport(width = 0.5, height = h.t, x = 0.5, y = 1 - h.t, just=c(0,0)))
print(g.country, vp = viewport(width = 1, height = 1 - h.t, x = 0.0, y = 0.0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.07, "npc"), y = unit(0.95, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("b")), x = unit(0.57, "npc"), y = unit(0.95, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("c")), x = unit(0.10, "npc"), y = unit(0.41, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("d")), x = unit(0.55, "npc"), y = unit(0.41, "npc"), gp=gpar(fontsize=18))
          
dev.off()



#### FIG S7 #### ----



anot.AlbedoChg <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_4$a_dif, na.rm=TRUE),4)) 
                         %+-% .(round(sd(pts_sf_aLCS_4$a_dif, na.rm=TRUE),4)))

g.map.AlbedoChg <- ggplot(pts_sf_aLCS_4) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = a_dif), size = pointSize)+
  scale_colour_gradientn("Albedo change",
                         limits = c(-0.01,0.01), 
                         colors = brewer.pal(9,'PiYG'), 
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  # labs(tag = 'a', caption = anot.AlbedoChg) + 
  labs(caption = anot.AlbedoChg) + 
  custom_theme_maps +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))






anot.AlbRadFor <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_4$RFa_Wm2,
                                                   na.rm = TRUE),4)) 
                         %+-% .(round(sd(pts_sf_aLCS_4$RFa_Wm2, na.rm = TRUE), 4)))

g.map.AlbRadFor <- ggplot(pts_sf_aLCS_4) + 
  geom_sf(data = europe_laea, fill = landColor)+
  geom_sf(aes(colour = RFa_Wm2), size = pointSize)+
  scale_colour_gradientn("Albedo radiative forcing",
                         limits = c(-0.5,0.5), 
                         colors = brewer.pal(9,'RdBu'), 
                         oob = squish)+
  coord_sf(xlim = xLims, ylim = yLims) +
  # labs(tag = 'b', caption = anot.AlbRadFor) + 
  labs(caption = anot.AlbRadFor) + 
  custom_theme_maps +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))





dat.perCountry_4$yr_lbl <- paste('Time horizon:', dat.perCountry_4$yr)

tot.perCountry <- dat.perCountry_4 %>% 
  group_by(ccType, yr_lbl) %>% 
  summarise(TotalMit = round(sum(CO2.eq), digits = 0))

g.country <- ggplot(dat.perCountry_4) +
  geom_bar(aes(x = reorder(Country, Area_Mha), y = CO2.eq, fill = Type), 
           stat = 'identity', position = 'stack') + 
  geom_hline(yintercept = 0, linetype = "solid", colour = 'grey10') +
  coord_polar(theta = "x", direction = -1) +
  geom_label(data = tot.perCountry, size = 3.5,
             aes(x = 12.5, y = -450,
                 label = paste('Total mitigation potential:\n', TotalMit, 'Tg CO2e'))) +
  facet_grid(ccType ~ yr_lbl) +
  scale_y_continuous(limits = c(-700, 100)) +
  scale_x_discrete('') + 
  scale_fill_manual('Type of effect:', 
                    values = effects.cols, 
                    labels = effects.lbls) +
  theme(legend.position = 'top',
        plot.tag = element_text(face = "bold"),
        strip.text = element_text(face = 'bold', size = '12')) +
  # labs(tag = 'c', caption = 'Note: there are insufficient data points for LU, MT, CY and HR.')
  labs(caption = 'Note: there are insufficient data points for LU, MT, CY and HR.')+ 
  ylab(expression("Mitigation potential (Tg CO"[2]*"e ha"^-1*")")) 


fname <- 'FigureS7_MutantCCwithSNOW'
figW <- 10; figH <- 12; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}

h.t <- 0.48
print(g.map.AlbedoChg, vp = viewport(width = 0.5, height = h.t, x = 0.0, y = 1 - h.t, just=c(0,0)))
print(g.map.AlbRadFor, vp = viewport(width = 0.5, height = h.t, x = 0.5, y = 1 - h.t, just=c(0,0)))
print(g.country, vp = viewport(width = 1, height = 1 - h.t, x = 0.0, y = 0.0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.07, "npc"), y = unit(0.95, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("b")), x = unit(0.57, "npc"), y = unit(0.95, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("c")), x = unit(0.10, "npc"), y = unit(0.41, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("d")), x = unit(0.55, "npc"), y = unit(0.41, "npc"), gp=gpar(fontsize=18))

dev.off()





