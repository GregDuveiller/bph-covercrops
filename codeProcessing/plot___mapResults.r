### exploratory plots results... 

require(tidyverse)
require(scales)
require(grid)
require(RColorBrewer)
require(sf)
require(here)

# Locate data
dpath <- 'dataProcessing/step1_dataExtractionFromGEE/'
opath <- 'dataProcessing/step2_deriveCoverCropAlbedo/'
vpath <- '/ESS_Datasets/USERS/Duveiller/AncillaryDatasets/WorldVector/'
fpath <- 'xplrFigures/albedo_perPts_EU'



# Needs to be amended and improved... 

load(file = paste0(opath,'deltaCC_df.RData')) # pts0


# prepare map 
world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
  st_transform(laes_prj)


pts_sf <- st_as_sf(pts0) %>% st_transform(laes_prj)

require(RColorBrewer)
require(scales)

dir.create(fpath)



iYear <- 2009
g.map.delta <- ggplot(filter(pts_sf,
                             ref_year==iYear,
                             BareSoil.Albedo.Source=='Landsat',
                             MaxVgt.Albedo.Source=='MODIS')) + 
  geom_sf(data=europe_laea,fill='grey50')+
  geom_sf(aes(colour=MaxVgt.Albedo-BareSoil.Albedo))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  coord_sf(xlim=c(2.5e6,6e6),ylim=c(1.5e6,4.5e6))+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))
ggsave(filename = paste0('DeltaCC_',iYear,'_LbMv','.png'), plot = g.map.delta, path = fpath, width = 12, height = 10)



g.map.combo <- ggplot(filter(pts_sf,
                             ref_year==iYear)) + 
  geom_sf(data=europe_laea,fill='grey50')+
  geom_sf(aes(colour=MaxVgt.Albedo-BareSoil.Albedo))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  facet_grid(BareSoil.Albedo.Source~MaxVgt.Albedo.Source,labeller = label_both)+    
  coord_sf(xlim=c(2.5e6,6e6),ylim=c(1.5e6,4.5e6))+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))
ggsave(filename = paste0('DeltaCC_',iYear,'_combo.png'), plot = g.map.combo, path = fpath, width = 12, height = 10)



g.map.delta.ts <- ggplot(filter(pts_sf,
                                BareSoil.Albedo.Source=='Landsat',
                                MaxVgt.Albedo.Source=='MODIS')) + 
  geom_sf(data=europe_laea,fill='grey50')+
  geom_sf(aes(colour=MaxVgt.Albedo-BareSoil.Albedo))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  facet_wrap(~ref_year)+
  coord_sf(xlim=c(2.5e6,6e6),ylim=c(1.5e6,4.5e6))+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))
ggsave(filename = paste0('DeltaCC_','TS','_LbMv.png'), plot = g.map.delta.ts, path = fpath, width = 12, height = 10)




g.map.combo <- ggplot(pts_sf %>% 
                        group_by(ref_year, BareSoil.Albedo.Source, MaxVgt.Albedo.Source) %>%
                        summarize(
                          BareSoil.Albedo.MU = mean(BareSoil.Albedo, na.rm=T),
                          MaxVgt.Albedo.MU = mean(MaxVgt.Albedo , na.rm=T))) +
  geom_sf(data=europe_laea,fill='grey50')+
  geom_sf(aes(colour=MaxVgt.Albedo-BareSoil.Albedo))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  facet_grid(BareSoil.Albedo.Source~MaxVgt.Albedo.Source,labeller = label_both)+    
  coord_sf(xlim=c(2.5e6,6e6),ylim=c(1.5e6,4.5e6))+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))
ggsave(filename = paste0('DeltaCC_','MEAN','_combo.png'), plot = g.map.combo, path = fpath, width = 12, height = 10)

