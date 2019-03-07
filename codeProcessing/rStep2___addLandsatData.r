### Script to add the landsat data and precompute mean values of soil
# (bph-covercrops)
#
# ===+++~~~---~~~+++===
# | Gregory Duveiller |
# ===+++~~~---~~~+++===

require(readr)


# read LUCAS points
ptsAll <- sf::st_read('dataInput/LUCAS_ARABLE.shp', quiet = TRUE)



### get Landsat NDVI values...

# read min-max LANDSAT NDVI
ptsNDVImin <- readr::read_csv(paste0(dpath,'LUCAS_Landsat_NDVI_MIN.csv'))
ptsNDVImax <- readr::read_csv(paste0(dpath,'LUCAS_Landsat_NDVI_MAX.csv'))
ptsNDVImin_std <- readr::read_csv(paste0(dpath,'LUCAS_Landsat_NDVI_MIN_STD.csv'))
ptsNDVImax_std <- readr::read_csv(paste0(dpath,'LUCAS_Landsat_NDVI_MAX_STD.csv'))


# some harmonization is required
ptsNDVImin <- ptsNDVImin %>% 
  left_join(ptsNDVImin_std %>%
              rename(NDVI_std = NDVI) %>%
              select(NDVI_std, sample_ID, date), by = c('sample_ID', 'date')) %>%
  mutate(ref_year = date + 1) %>% # as year reported is year of start of season
  select(-date)

ptsNDVImax <- ptsNDVImax %>%
  left_join(ptsNDVImax_std %>%
              rename(NDVI_std = NDVI) %>%
              select(NDVI_std, sample_ID, date), by = c('sample_ID', 'date')) %>%
  rename(ref_year = date) %>% # as year reported is year of main growing season
  mutate(DOI365 = round(DOY + 365)) %>%
  select(-DOY)

## NOTE: We could filter out based on the STD of NDVI
## ... if STD is too high, too variable 
## ... (either due to diff land cover or diff dates of composite within 9 pixel zone)


# get fit regress data...
load(paste0('dataProcessing/step2_deriveCoverCropAlbedo/regres_df.RData'))

  
# join dataset of LANDSAT values with those from MODIS
outDF0 <- outDF %>% 
  left_join(select(ptsNDVImin, POINT_ID, ref_year, NDVI),
            by = c('POINT_ID','ref_year')) %>%
  dplyr::rename(NDVI.L.min=NDVI) %>% 
  left_join(select(ptsNDVImax, POINT_ID, ref_year, NDVI), 
            by = c('POINT_ID','ref_year')) %>%
  dplyr::rename(NDVI.L.max=NDVI)


# calc all albedos
outDF1 <- outDF0 %>%
  mutate(Alb.Min.L = b0 + b1 * NDVI.L.min,
         Alb.Min.M = b0 + b1 * NDVI.M.min,
         Alb.Max.L = b0 + b1 * NDVI.L.max,
         Alb.Max.M = b0 + b1 * NDVI.M.max) %>%
  left_join(select(ptsAll, POINT_ID, GPS_LAT, GPS_LONG), by='POINT_ID')  




## exploratory plots... 

ggplot(outDF1)+
  geom_point(aes(x=GPS_LONG,y=GPS_LAT,colour=cut(adj.r.sqr, c(-1,0,0.1,0.5,0.75,1)))) + 
  scale_color_viridis_d('adj.R2')

ggplot(outDF1)+
  geom_point(aes(x=GPS_LONG,y=GPS_LAT,colour=cut(n, c(0,10,100,200,300)))) + 
  scale_color_viridis_d('n') + 
  facet_wrap(~ref_year)

ggplot(outDF1)+
  geom_bar(aes(x=ref_year, fill=cut(n, c(0,10,30,100,300,1000,3000)))) + 
  scale_fill_viridis_d('n')

ggplot(outDF1)+
  geom_bar(aes(x=ref_year, fill=cut(adj.r.sqr, c(-1,0,0.1,0.5,0.75,1)))) + 
  scale_fill_viridis_d('adj.r.sqr')



outDF_soft <- outDF1 %>%
  filter(n > 30, adj.r.sqr > 0.1) %>%
  group_by(POINT_ID) %>%
  summarize(BareSoil.Albedo = mean(Alb.Min.L, na.rm = T), 
            MaxVgt.Albedo = mean(Alb.Max.M, na.rm = T)) %>%
  left_join(select(ptsAll, POINT_ID, GPS_LAT, GPS_LONG), by='POINT_ID') 

outDF_hard <- outDF1 %>%
  filter(n > 100, adj.r.sqr > 0.5) %>%
  group_by(POINT_ID) %>%
  summarize(BareSoil.Albedo = mean(Alb.Min.L, na.rm = T), 
            MaxVgt.Albedo = mean(Alb.Max.M, na.rm = T)) %>%
  left_join(select(ptsAll, POINT_ID, GPS_LAT, GPS_LONG), by='POINT_ID') 


dir.create(opath)
save('outDF_hard', file = paste0(opath,'deltaCC_df_hard.RData'))
save('outDF_soft', file = paste0(opath,'deltaCC_df_soft.RData'))






################
outDF2 <- outDF1 %>%
  filter(n > 30, adj.r.sqr > 0.1) %>%
  group_by(POINT_ID) %>%
  summarize(mu.Alb.Min.L = mean(Alb.Min.L, na.rm = T), 
            mu.Alb.Max.L = mean(Alb.Max.L, na.rm = T),
            mu.Alb.Min.M = mean(Alb.Min.M, na.rm = T), 
            mu.Alb.Max.M = mean(Alb.Max.M, na.rm = T)) %>%
  left_join(select(ptsAll, POINT_ID, GPS_LAT, GPS_LONG), by='POINT_ID') 



outDF3 <- outDF1 %>%
  filter(n > 100, adj.r.sqr > 0.5) %>%
  group_by(POINT_ID) %>%
  summarize(mu.Alb.Min.L = mean(Alb.Min.L, na.rm = T), 
            mu.Alb.Max.L = mean(Alb.Max.L, na.rm = T),
            mu.Alb.Min.M = mean(Alb.Min.M, na.rm = T), 
            mu.Alb.Max.M = mean(Alb.Max.M, na.rm = T)) %>%
  left_join(select(ptsAll, POINT_ID, GPS_LAT, GPS_LONG), by='POINT_ID') 





ggplot(outDF2) + 
  geom_point(aes(x = GPS_LONG, y = GPS_LAT, colour = mu.Alb.Max.L-mu.Alb.Min.L))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))

ggplot(outDF2) + 
  geom_point(aes(x = GPS_LONG, y = GPS_LAT, colour = mu.Alb.Max.M-mu.Alb.Min.L))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))

ggplot(outDF2) + 
  geom_point(aes(x = GPS_LONG, y = GPS_LAT, colour = mu.Alb.Max.M-mu.Alb.Min.M))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))





outDF3 <- outDF1 %>%
  filter(n > 100, adj.r.sqr > 0.5) %>%
  group_by(POINT_ID) %>%
  summarize(mu.Alb.Min.L = mean(Alb.Min.L, na.rm = T), 
            mu.Alb.Max.L = mean(Alb.Max.L, na.rm = T),
            mu.Alb.Min.M = mean(Alb.Min.M, na.rm = T), 
            mu.Alb.Max.M = mean(Alb.Max.M, na.rm = T)) %>%
  left_join(select(ptsAll, POINT_ID, GPS_LAT, GPS_LONG), by='POINT_ID') 

ggplot(outDF3) + 
  geom_point(aes(x = GPS_LONG, y = GPS_LAT, colour = mu.Alb.Max.L-mu.Alb.Min.L))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))

ggplot(outDF3) + 
  geom_point(aes(x = GPS_LONG, y = GPS_LAT, colour = mu.Alb.Max.M-mu.Alb.Min.L))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))

ggplot(outDF3) + 
  geom_point(aes(x = GPS_LONG, y = GPS_LAT, colour = mu.Alb.Max.M-mu.Alb.Min.M))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))




ggplot(outDF3) + 
  geom_point(aes(x = GPS_LONG, y = GPS_LAT, colour = mu.Alb.Min.M-mu.Alb.M.L))+
  scale_colour_gradientn(limits=c(-0.1,+0.1),colors=brewer.pal(9,'RdBu'),oob=squish)+
  theme(legend.position = 'bottom',
        legend.key.width = unit(1,'in'))+
  guides(colour = guide_colourbar(title.position='top',title.hjust=0.5))



# 
# # combine
# pts0.bare <- outDF1 %>%
#   transmute(Landsat = b0 + b1 * NDVI.L.min,
#             MODIS = b0 + b1 * NDVI.M.min,
#             POINT_ID = POINT_ID,
#             ref_year = ref_year) %>%
#   gather(BareSoil.Albedo.Source, BareSoil.Albedo, Landsat, MODIS)
# 
# pts0.vgtn <- outDF1 %>%
#   transmute(Landsat = b0 + b1 * NDVI.L.max,
#             MODIS = b0 + b1 * NDVI.M.max,
#             POINT_ID = POINT_ID,
#             ref_year = ref_year) %>%
#   gather(MaxVgt.Albedo.Source, MaxVgt.Albedo , Landsat, MODIS)

pts0 <- inner_join(pts0.bare,pts0.vgtn,by=c('POINT_ID','ref_year')) %>%
  left_join(select(ptsAll, POINT_ID, GPS_LAT, GPS_LONG), by='POINT_ID')  


dir.create(opath)
save('pts0', file = paste0(opath,'deltaCC_df.RData'))


# some plots...

ggplot(outDF0) +
  geom_density(aes(x=b0,colour=ref_year, group=ref_year),na.rm = FALSE)+
  scale_x_continuous(limits = c(-1,1))


ggplot(outDF0 %>% dplyr::filter(n > 50, adj.r.sqr > 0.3)) +
  geom_point(aes(x=b1,y=adj.r.sqr, colour=ref_year))

outDF0agr <- outDF0 %>% 
  dplyr::filter(n > 50, adj.r.sqr > 0.3)  %>%
  group_by(POINT_ID) %>%
  summarize(mean_b0 = mean(b0, na.rm = T),
            mean_b1 = mean(b1, na.rm = T),
            sd_b0 = sd(b0, na.rm = T),
            sd_b1 = sd(b1, na.rm = T))

ggplot(outDF0agr) +
  geom_point(aes(x=mean_b0,y=sd_b0))


ggplot(outDF0agr) +
  geom_density(aes(x=sd_b0),na.rm = FALSE)+
  scale_x_continuous(limits = c(-1,1))


# ## ## OLD WAY ## ## # may have issues... 
# ### This is to use the yearly regression on the yearly min and max NDVI
# 
# # get values of albedo  
# pts0 <- outDF0 %>%
#   mutate(Landsat = b0 + b1 * NDVI.L.min,
#          MODIS = b0 + b1 * NDVI.M.min) %>%
#   gather(BareSoil.Albedo.Source, BareSoil.Albedo, Landsat, MODIS) %>%
#   mutate(Landsat = b0 + b1 * NDVI.L.max,
#          MODIS = b0 + b1 * NDVI.M.max) %>%
#   gather(MaxVgt.Albedo.Source, MaxVgt.Albedo, Landsat, MODIS) %>%
#   left_join(select(ptsAll, POINT_ID, GPS_LAT, GPS_LONG), by='POINT_ID')
# 
# 
# 




