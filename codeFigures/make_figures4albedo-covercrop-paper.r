#### Script to generate the figures for the paper:
# 
#  Lugato E., Cescatti A., Arwyn J., Ceccherini G. and Duveiller G. (2020)
#  "Maximising climate mitigation potential by carbon and radiative agricultural 
#  land management with cover crops" Environmental Research Letters. 
#  https://doi.org/10.1088/1748-9326/aba137
#
#  Input data required:
#  - data4scen1_normalCC-noSNOW.Rda
#  - data4scen2_normalCC-withSNOW.Rda
#  - data4scen3_mutantCC-noSNOW.Rda
#  - data4scen4_mutantCC-withSNOW.Rda
#
# ______________________________________________________________________________



# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(grid)
library(scales)
library(RColorBrewer)
library(here)

#### data preparation #### ----

fpath <- 'textFigures/'
fmt <- 'pdf' # fmt <- 'png'


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

# wrapper fucntion
get.GHGsen <- function(ccType, snow_flag){
  GHGsen_0 <- dat$GHGsen %>% 
    dplyr::rename('2030' = GHGr30, '2050' = GHGr50, '2070' = GHGr70, '2100' = GHGr00) %>%
    dplyr::select('POINT_ID', 'GPS_LAT', 'GPS_LONG', '2030', '2050', '2070', '2100') %>%
    dplyr::mutate(ccType = factor(ccType, levels = ccTypes,
                                  labels = ccTypes_lbls)) %>%
    tidyr::gather(key = 'TimeHorizon', value = 'GHGr', c('2030', '2050', '2070', '2100')) %>%
    dplyr::mutate(snow = snow_flag)
}
GHGsen_1 <- get.GHGsen(ccType = 'Normal', snow_flag = F)

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
  get.df(yr = '2050', ccType = 'Normal'),
  get.df(yr = '2070', ccType = 'Normal'),
  get.df(yr = '2100', ccType = 'Normal')) %>%
  mutate(snow = F)



### load third dataset, with mutant plants ----
load(file = 'dataFigures/data4scen3_mutantCC-noSNOW.Rda') 
# (loads a list named 'dat' with several objects 'GHGbdg','GHGsen', 'aLCS', etc)

# make sf points for the maps showing projected scenario with mutant plants
GHGsen_3 <- get.GHGsen(ccType = 'Mutant', snow_flag = F)

# add category of mutant plants
GHGbdg_3 <- dat$GHGbdg %>% 
  mutate(ccType = factor('Mutant', levels = ccTypes, labels = ccTypes_lbls)) %>%
  mutate(snow = F)

# get the needed data for the normal case
dat.perCountry_3 <- bind_rows(
  get.df(yr = '2030', ccType = 'Mutant'),
  get.df(yr = '2050', ccType = 'Mutant'),
  get.df(yr = '2070', ccType = 'Mutant'),
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
GHGsen_2 <- get.GHGsen(ccType = 'Normal', snow_flag = T)

# add category for normal plant + snow
GHGbdg_2 <- dat$GHGbdg %>%  
  mutate(ccType = factor('Normal', levels = ccTypes, labels = ccTypes_lbls)) %>%
  mutate(snow = T)

# get the needed data for the normal case
dat.perCountry_2 <- bind_rows(
  get.df(yr = '2030', ccType = 'Normal'),
  get.df(yr = '2050', ccType = 'Normal'),
  get.df(yr = '2070', ccType = 'Normal'),
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
GHGsen_4 <- get.GHGsen(ccType = 'Mutant', snow_flag = T)


# add category of mutant plants + snow
GHGbdg_4 <- dat$GHGbdg %>% 
  mutate(ccType = factor('Mutant', levels = ccTypes, labels = ccTypes_lbls)) %>%
  mutate(snow = T)

# get the needed data for the normal case
dat.perCountry_4 <- bind_rows(
  get.df(yr = '2030', ccType = 'Mutant'),
  get.df(yr = '2050', ccType = 'Mutant'),
  get.df(yr = '2070', ccType = 'Mutant'),
  get.df(yr = '2100', ccType = 'Mutant')) %>%
  mutate(snow = T)





### combine datasets... -----

GHGbdg_bgc <- bind_rows(
  GHGbdg_1 %>% select(-snow) %>% filter(scenarios %in% c('CO2_soil', 'N2O_dir')),
  GHGbdg_3 %>% select(-snow) %>% filter(scenarios %in% c('CO2_soil', 'N2O_dir')))

GHGbdg_bph <- bind_rows(
  GHGbdg_1 %>% filter(scenarios == 'albedo') %>% 
    unite(col = scenarios, scenarios, snow, sep = '_snow'),
  GHGbdg_3 %>% filter(scenarios == 'albedo') %>% 
    unite(col = scenarios, scenarios, snow, sep = '_snow'), 
  GHGbdg_2 %>% filter(scenarios == 'albedo') %>% 
    unite(col = scenarios, scenarios, snow, sep = '_snow'), 
  GHGbdg_4 %>% filter(scenarios == 'albedo') %>% 
    unite(col = scenarios, scenarios, snow, sep = '_snow'))

GHGbdg <- bind_rows(GHGbdg_bgc, GHGbdg_bph) 

pts_sf_GHGsen <- bind_rows(GHGsen_1, GHGsen_3, GHGsen_2, GHGsen_4) %>%
  st_as_sf(coords = c("GPS_LONG","GPS_LAT")) %>%
  st_set_crs(st_crs(world)) %>%
  st_transform(laes_prj)

dat.perCountry <- bind_rows(dat.perCountry_1, dat.perCountry_3,
                            dat.perCountry_2, dat.perCountry_4)





#### Figures #### ----

  
# set parameters for plots
pointSize <- 0.2
landColor <- 'grey60'
xLims <- c(2.5e6,6e6)
yLims <- c(1.5e6,4.5e6)
  
custom_theme_maps <- theme(legend.position = 'top',
                           legend.key.width = unit(0.8,'in'),
                           plot.tag = element_text(face = "bold"))
  
#### FIG 1 #### ----

  
  # SNOW FREE
  
  anot.BareSoilA1 <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_1$BareSoil.Albedo, na.rm=TRUE),4)) 
                           %+-% .(round(sd(pts_sf_aLCS_1$BareSoil.Albedo, na.rm=TRUE),4)))
  
  g.map.BareSoilA1 <- ggplot(pts_sf_aLCS_1) + 
    geom_sf(data = europe_laea, fill = landColor)+
    geom_sf(aes(colour = BareSoil.Albedo), size = pointSize)+
    scale_colour_viridis_c("Bare soil albedo (snow free)",
                           limits = c(0.05,0.25), 
                           option = "viridis", 
                           oob = squish)+
    coord_sf(xlim = xLims, ylim = yLims) +
    # labs(tag = 'b', caption = anot.BareSoilA) + 
    labs(caption = anot.BareSoilA1) + 
    custom_theme_maps +  
    guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))
  
  
  anot.AlbedoChg1 <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_1$a_dif, na.rm=TRUE),4)) 
                           %+-% .(round(sd(pts_sf_aLCS_1$a_dif, na.rm=TRUE),4)))
  
  g.map.AlbedoChg1 <- ggplot(pts_sf_aLCS_1) + 
    geom_sf(data = europe_laea, fill = landColor)+
    geom_sf(aes(colour = a_dif), size = pointSize)+
    scale_colour_gradientn("Albedo change (snow free)",
                           limits = c(-0.01,0.01), 
                           colors = brewer.pal(9,'PiYG'), 
                           oob = squish)+
    coord_sf(xlim = xLims, ylim = yLims) +
    # labs(tag = 'c', caption = anot.AlbedoChg) + 
    labs(caption = anot.AlbedoChg1) + 
    custom_theme_maps +  
    guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))
  
  
  anot.AlbRadFor1 <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_1$RFa_Wm2,
                                                     na.rm = TRUE),4)) 
                           %+-% .(round(sd(pts_sf_aLCS_1$RFa_Wm2, na.rm = TRUE), 4)))
  
  g.map.AlbRadFor1 <- ggplot(pts_sf_aLCS_1) + 
    geom_sf(data = europe_laea, fill = landColor)+
    geom_sf(aes(colour = RFa_Wm2), size = pointSize)+
    scale_colour_gradientn("Albedo radiative forcing (snow free)",
                           limits = c(-0.5,0.5), 
                           colors = brewer.pal(9,'RdBu'), 
                           oob = squish)+
    coord_sf(xlim = xLims, ylim = yLims) +
    # labs(tag = 'd', caption = anot.AlbRadFor) + 
    labs(caption = anot.AlbRadFor1) + 
    custom_theme_maps +  
    guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))
  
  
  # WITH SNOW
  
  anot.SnowCover2 <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_2$snowDJF, na.rm=TRUE),4)) 
                           %+-% .(round(sd(pts_sf_aLCS_2$snowDJF, na.rm=TRUE),4)))
  
  g.map.SnowCover2 <- ggplot(pts_sf_aLCS_2) + 
    geom_sf(data = europe_laea, fill = landColor)+
    geom_sf(aes(colour = snowDJF), size = pointSize)+
    scale_colour_viridis_c("Mean snow water equivalent during DJF [cm]",
                           limits = c(0,5), 
                           option = "magma", 
                           oob = squish)+
    coord_sf(xlim = xLims, ylim = yLims) +
    # labs(tag = 'b', caption = anot.BareSoilA) + 
    labs(caption = anot.SnowCover2) + 
    custom_theme_maps +  
    guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))
  
  
  anot.AlbedoChg2 <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_2$a_dif, na.rm=TRUE),4)) 
                           %+-% .(round(sd(pts_sf_aLCS_2$a_dif, na.rm=TRUE),4)))
  
  g.map.AlbedoChg2 <- ggplot(pts_sf_aLCS_2) + 
    geom_sf(data = europe_laea, fill = landColor)+
    geom_sf(aes(colour = a_dif), size = pointSize)+
    scale_colour_gradientn("Albedo change (with snow)",
                           limits = c(-0.01,0.01), 
                           colors = brewer.pal(9,'PiYG'), 
                           oob = squish)+
    coord_sf(xlim = xLims, ylim = yLims) +
    # labs(tag = 'c', caption = anot.AlbedoChg) + 
    labs(caption = anot.AlbedoChg2) + 
    custom_theme_maps +  
    guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))
  
  
  anot.AlbRadFor2 <- bquote(mu(sigma) == .(round(mean(pts_sf_aLCS_2$RFa_Wm2,
                                                     na.rm = TRUE),4)) 
                           %+-% .(round(sd(pts_sf_aLCS_2$RFa_Wm2, na.rm = TRUE), 4)))
  
  g.map.AlbRadFor2 <- ggplot(pts_sf_aLCS_2) + 
    geom_sf(data = europe_laea, fill = landColor)+
    geom_sf(aes(colour = RFa_Wm2), size = pointSize)+
    scale_colour_gradientn("Albedo radiative forcing (with snow)",
                           limits = c(-0.5,0.5), 
                           colors = brewer.pal(9,'RdBu'), 
                           oob = squish)+
    coord_sf(xlim = xLims, ylim = yLims) +
    # labs(tag = 'd', caption = anot.AlbRadFor) + 
    labs(caption = anot.AlbRadFor2) + 
    custom_theme_maps +  
    guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))
  
  
  
  
  
  fname <- 'Figure1_Bph-effect'
  figW <- 10; figH <- 16
  fullfname <- paste0(fpath, fname, '.', fmt)
  if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
  if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}
  
  w1 <- 0.5; w2 <- 0.5; h <- 0.33
  
  print(g.map.SnowCover2, vp = viewport(width = w1, height = h, x = 0.0, y = 2*h, just=c(0,0)))
  print(g.map.AlbedoChg2, vp = viewport(width = w1, height = h, x = 0.0, y = 1*h, just=c(0,0)))
  print(g.map.AlbRadFor2, vp = viewport(width = w1, height = h, x = 0.0, y = 0*h, just=c(0,0)))
  
  print(g.map.BareSoilA1, vp = viewport(width = w2, height = h, x = 0.5, y = 2*h, just=c(0,0)))
  print(g.map.AlbedoChg1, vp = viewport(width = w2, height = h, x = 0.5, y = 1*h, just=c(0,0)))
  print(g.map.AlbRadFor1, vp = viewport(width = w2, height = h, x = 0.5, y = 0*h, just=c(0,0)))
  
  grid.text(expression(bold("a")), x = unit(0.04, "npc"), y = unit(0.97, "npc"), gp=gpar(fontsize=18))
  grid.text(expression(bold("b")), x = unit(0.54, "npc"), y = unit(0.97, "npc"), gp=gpar(fontsize=18))
  grid.text(expression(bold("c")), x = unit(0.04, "npc"), y = unit(0.64, "npc"), gp=gpar(fontsize=18))
  grid.text(expression(bold("d")), x = unit(0.54, "npc"), y = unit(0.64, "npc"), gp=gpar(fontsize=18))
  grid.text(expression(bold("e")), x = unit(0.04, "npc"), y = unit(0.31, "npc"), gp=gpar(fontsize=18))
  grid.text(expression(bold("f")), x = unit(0.54, "npc"), y = unit(0.31, "npc"), gp=gpar(fontsize=18))
  
  dev.off()
  
  
  

# print(g.map.TransSWin, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.5, just=c(0,0)))
# print(g.map.BareSoilA, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.5, just=c(0,0)))
# print(g.map.AlbedoChg, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.0, just=c(0,0)))
# print(g.map.AlbRadFor, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.0, just=c(0,0)))
# 
# grid.text(expression(bold("a")), x = unit(0.07, "npc"), y = unit(0.96, "npc"), gp=gpar(fontsize=18))
# grid.text(expression(bold("b")), x = unit(0.57, "npc"), y = unit(0.96, "npc"), gp=gpar(fontsize=18))
# grid.text(expression(bold("c")), x = unit(0.07, "npc"), y = unit(0.46, "npc"), gp=gpar(fontsize=18))
# grid.text(expression(bold("d")), x = unit(0.57, "npc"), y = unit(0.46, "npc"), gp=gpar(fontsize=18))



#### FIG 2 #### ----

year_flag <- 2050

scen.cols <- c('CO2_soil' = 'firebrick3',   # 'tan4', 
               'N2O_dir' ='goldenrod',         # 'goldenrod', 
               'albedo_snowTRUE' = 'cornflowerblue',
               'albedo_snowFALSE' = 'darkblue')
scen.lbls <- c('CO2_soil' = bquote('CO'[2]), 
               'N2O_dir' = bquote('N'[2]*'O'), 
               'albedo_snowTRUE' = 'Albedo with snow',
               'albedo_snowFALSE' = 'Albedo snow free')
scen.line <- c('CO2_soil' = 1, 
               'N2O_dir' = 1, 
               'albedo_snowTRUE' = 1,
               'albedo_snowFALSE' = 2)

lab <- expression("Cumulative emissions (Mg CO"[2]*"e ha"^-1*")")   ##labels

# add dummy title on top for projections
GHGbdg$Simulation <- 'Projected temporal trends'

g.futprojection <- ggplot(GHGbdg, aes(x = year, y = dSOC, colour = scenarios)) + 
  geom_line(aes(linetype = scenarios), size = 0.7) + 
  geom_ribbon(aes(ymin = dSOC_2q, ymax = dSOC_1q, fill = scenarios), 
              linetype = 0, alpha = 0.2) +
  facet_grid(ccType~Simulation) +
  scale_y_continuous(lab) +
  scale_x_continuous(expand = c(0,0)) +
  scale_linetype_manual('', values = scen.line, labels = scen.lbls) +
  scale_fill_manual('', values = scen.cols, labels = scen.lbls) +
  scale_color_manual('', values = scen.cols, labels = scen.lbls) +
  geom_hline(yintercept = 0, linetype = "solid", colour = 'grey10') +
  geom_vline(xintercept = year_flag, linetype = "dashed", colour = 'grey50') +
  coord_cartesian(ylim = c(-25, 10)) +
  # labs(tag = 'a') +
  theme(legend.position = c(0.4,0.5),
        legend.title = element_blank(),
        legend.background = element_rect(fill = 'white', colour = 'gray40'),
        #axis.line = element_line(colour = 'gray20'),
        axis.ticks = element_line(colour = 'gray40'),
        axis.title.x = element_blank(),
        panel.border = element_rect (fill = 'NA', colour = 'gray20'),
        panel.background = element_rect (fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = 'bold', size = '12'),
        plot.tag = element_text(face = "bold"))

pts_sf_GHGsen$TimeHorizonLbl <- paste('Spatial distribution for time horizon:', pts_sf_GHGsen$TimeHorizon)
pts_sf_GHGsen$snow_lbl <- factor(pts_sf_GHGsen$snow, levels = c(TRUE, FALSE), labels = c('With snow', 'Snow free'))

g.map.AlbRelImp <- ggplot(pts_sf_GHGsen %>% filter(TimeHorizon == year_flag)) + 
  geom_sf(data = europe_laea, fill = landColor) +
  geom_sf(aes(colour = GHGr), size = pointSize) +
  # geom_point(aes(colour = GHGr, 
  #                x = st_coordinates(geometry)[,1],
  #                y = st_coordinates(geometry)[,2]),
  #            size = pointSize) +
  scale_colour_viridis_c("Ratio of\n albedo\n forcing",
                         limits = c(0, 2),
                         option = 'plasma', 
                         oob = squish) +
  facet_grid(ccType ~ snow_lbl) +
  coord_sf(xlim = xLims, ylim = yLims) +
  # labs(tag = 'b') +
  theme(legend.position = 'right',
        legend.key.height = unit(0.9,'in'),
        axis.title = element_blank(),
        strip.text = element_text(face = 'bold', size = '12'),
        plot.tag = element_text(face = "bold")) +
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))



fname <- 'Figure2_Projections'
figW <- 15; figH <- 8
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
dat.perCountry$snow_lbl <- factor(dat.perCountry$snow, levels = c(TRUE, FALSE), 
                              labels = c('With snow', 'Snow free'))

tot.perCountry <- dat.perCountry %>% 
  filter(yr %in% c('2050')) %>% 
  group_by(ccType, snow_lbl) %>% 
  summarise(TotalMit = round(sum(CO2.eq), digits = 0)) %>%
  mutate(x = 12.5, y = -350)

bar.lims <- c(-700, 80)

g.country <- ggplot(dat.perCountry %>% 
                      filter(yr %in% c('2050'))) +
  geom_bar(aes(x = reorder(Country, Area_Mha), y = CO2.eq, fill = Type),
          stat = 'identity', position = 'stack') +
  geom_hline(yintercept = 0, linetype = "solid", colour = 'grey10') +
  coord_polar(theta = "x", direction = -1) +
  geom_label(data = tot.perCountry, size = 3, 
             aes(x = x, y = y, 
                 label = paste('Combined:\n', TotalMit, 'Tg CO2e'))) +
                 #label = bquote("Total mitigation potential:\n" ~.(TotalMit) ~ "Tg COe"))) +
  facet_grid(ccType ~ snow_lbl) +
  scale_y_continuous(limits = bar.lims) +
  scale_x_discrete('') + 
  scale_fill_manual('Type of effect:', 
                      values = effects.cols, 
                      labels = effects.lbls) +
  theme(legend.position = 'top',
        strip.text = element_text(face = 'bold', size = '12')) +
  ylab(expression("Cumulative emissions (Tg CO"[2]*"e ha"^-1*")")) +
  labs(title = 'Cumulative emissions by country for year 2050',
       caption = 'Note: there are insufficient data points for LU, MT, CY and HR.')

fname <- 'Figure3_perCountry'
figW <- 8; figH <- 8
fullfname <- paste0(fpath, fname, '.', fmt)

if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}

print(g.country, vp = viewport(width = 1, height = 1, x = 0.00, y = 0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.14, "npc"), y = unit(0.85, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("b")), x = unit(0.55, "npc"), y = unit(0.85, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("c")), x = unit(0.14, "npc"), y = unit(0.44, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("d")), x = unit(0.55, "npc"), y = unit(0.44, "npc"), gp=gpar(fontsize=18))

dev.off()




#### FIG S5 #### ----

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

fname <- 'FigureS5_transmition'
figW <- 6; figH <- 6
fullfname <- paste0(fpath, fname, '.', fmt)

ggsave(filename = fullfname, plot = g.map.TransSWin, width = figW, height = figH)

#### FIG S6 #### ----

effects.cols <- c('BGC' = 'coral', 
                  'BPH' = 'cornflowerblue')
effects.lbls <- c('BGC' = bquote('Biogeochemical (CO'[2]*'+ N'[2]*'O)'), 
                  'BPH' = 'Biogeophysical (albedo)')

dat.perCountry$yr_lbl <- paste('Time horizon:', dat.perCountry$yr)
dat.perCountry$snow_lbl <- factor(dat.perCountry$snow, levels = c(TRUE, FALSE), 
                                  labels = c('With snow', 'Snow free'))

tot.perCountry <- dat.perCountry %>% 
  filter(yr %in% c('2030', '2100')) %>% 
  filter(ccType == 'Normal cover crop') %>%
  group_by(yr, snow_lbl) %>% 
  summarise(TotalMit = round(sum(CO2.eq), digits = 0)) %>%
  mutate(x = 12.5, y = -300)



g.country <- ggplot(dat.perCountry %>% 
                      filter(ccType %in% c('Normal cover crop'),
                             yr %in% c('2030', '2100'))) +
  geom_bar(aes(x = reorder(Country, Area_Mha), y = CO2.eq, fill = Type),
           stat = 'identity', position = 'stack') +
  geom_hline(yintercept = 0, linetype = "solid", colour = 'grey10') +
  coord_polar(theta = "x", direction = -1) +
  geom_label(data = tot.perCountry, size = 3, 
             aes(x = x, y = y, 
                 label = paste('Combined:\n', TotalMit, 'Tg CO2e'))) +
  #label = bquote("Total mitigation potential:\n" ~.(TotalMit) ~ "Tg COe"))) +
  facet_grid(yr ~ snow_lbl) +
  scale_y_continuous(limits = bar.lims) +
  scale_x_discrete('') + 
  scale_fill_manual('Type of effect:', 
                    values = effects.cols, 
                    labels = effects.lbls) +
  theme(legend.position = 'top',
        strip.text = element_text(face = 'bold', size = '12')) +
  ylab(expression("Cumulative emissions (Tg CO"[2]*"e ha"^-1*")")) +
  labs(title = 'Cumulative emissions by country for a normal cover crop',
       caption = 'Note: there are insufficient data points for LU, MT, CY and HR.')

fname <- 'FigureS6_perCountry_normal'
figW <- 8; figH <- 8
fullfname <- paste0(fpath, fname, '.', fmt)

if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}

print(g.country, vp = viewport(width = 1, height = 1, x = 0.00, y = 0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.14, "npc"), y = unit(0.85, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("b")), x = unit(0.55, "npc"), y = unit(0.85, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("c")), x = unit(0.14, "npc"), y = unit(0.44, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("d")), x = unit(0.55, "npc"), y = unit(0.44, "npc"), gp=gpar(fontsize=18))

dev.off()



#### FIG S7 #### ----

tot.perCountry <- dat.perCountry %>% 
  filter(yr %in% c('2030', '2100')) %>% 
  filter(ccType == 'Chlorophyll-deficient mutant') %>%
  group_by(yr, snow_lbl) %>% 
  summarise(TotalMit = round(sum(CO2.eq), digits = 0)) %>%
  mutate(x = 12.5, y = -300)

#bar.lims <- c(-700, 80)

g.country <- ggplot(dat.perCountry %>% 
                      filter(ccType %in% c('Chlorophyll-deficient mutant'),
                             yr %in% c('2030', '2100'))) +
  geom_bar(aes(x = reorder(Country, Area_Mha), y = CO2.eq, fill = Type),
           stat = 'identity', position = 'stack') +
  geom_hline(yintercept = 0, linetype = "solid", colour = 'grey10') +
  coord_polar(theta = "x", direction = -1) +
  geom_label(data = tot.perCountry, size = 3, 
             aes(x = x, y = y, 
                 label = paste('Combined:\n', TotalMit, 'Tg CO2e'))) +
  #label = bquote("Total mitigation potential:\n" ~.(TotalMit) ~ "Tg COe"))) +
  facet_grid(yr ~ snow_lbl) +
  scale_y_continuous(limits = bar.lims) +
  scale_x_discrete('') + 
  scale_fill_manual('Type of effect:', 
                    values = effects.cols, 
                    labels = effects.lbls) +
  theme(legend.position = 'top',
        strip.text = element_text(face = 'bold', size = '12')) +
  ylab(expression("Cumulative emissions (Tg CO"[2]*"e ha"^-1*")")) +
  labs(title = 'Cumulative emissions by country for a chlorophyll-deficient cover crop',
       caption = 'Note: there are insufficient data points for LU, MT, CY and HR.')

fname <- 'FigureS7_perCountry_mutant'
figW <- 8; figH <- 8
fullfname <- paste0(fpath, fname, '.', fmt)

if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}

print(g.country, vp = viewport(width = 1, height = 1, x = 0.00, y = 0, just=c(0,0)))

grid.text(expression(bold("a")), x = unit(0.14, "npc"), y = unit(0.85, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("b")), x = unit(0.55, "npc"), y = unit(0.85, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("c")), x = unit(0.14, "npc"), y = unit(0.44, "npc"), gp=gpar(fontsize=18))
grid.text(expression(bold("d")), x = unit(0.55, "npc"), y = unit(0.44, "npc"), gp=gpar(fontsize=18))

dev.off()


