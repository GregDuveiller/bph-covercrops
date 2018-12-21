### Script to add the landsat data and precompute mean values of soil
# (bph-covercrops)
#
# ===+++~~~---~~~+++===
# | Gregory Duveiller |
# ===+++~~~---~~~+++===




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


### This is to use the yearly regression on the yearly min and max NDVI
### Another option could/should be to use the mean regression on the mean NDVIs

# get values of albedo  
pts0 <- outDF0 %>%
  mutate(Landsat=b0+b1*NDVI.L.min,
         MODIS=b0+b1*NDVI.M.min) %>%
  gather(BareSoil.Albedo.Source,BareSoil.Albedo,Landsat,MODIS) %>%
  mutate(Landsat=b0+b1*NDVI.L.max,
         MODIS=b0+b1*NDVI.M.max) %>%
  gather(MaxVgt.Albedo.Source,MaxVgt.Albedo,Landsat,MODIS) %>%
  left_join(select(ptsAll,POINT_ID,GPS_LAT,GPS_LONG),by='POINT_ID')

dir.create(opath)
save('pts0',file = paste0(opath,'deltaCC_df.RData'))

