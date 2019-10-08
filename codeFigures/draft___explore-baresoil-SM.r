# harvest SM over LUCAS points


library(ncdf4)
library(raster)
library(sf)

pt_info <-  sf::st_read('dataInput/LUCAS_ARABLE.shp')


fl <- list.files(path = '/data/C3S-SOILMOISTURE', pattern = 'C3S-SOILMOISTURE.*.1001000', 
                 full.names = T)

out <- matrix(data = NA, nrow = dim(pt_info)[1], ncol = length(fl))
for(i in 1:length(fl)){
  r <- raster(fl[i], varname = 'sm')  
  out[,i] <- extract(r, pt_info)
}

out <- data.frame(SM = apply(out, MARGIN = 1, FUN = mean, na.rm = T))

pt <- pt_info %>% 
  dplyr::select(geometry, POINT_ID, CaCO3, CEC, OC) %>% 
  bind_cols(out)


# load albedo
dat <- read.csv(file = 'dataFigures/alb_elabo3.1.PET.csv') %>%
  dplyr::select(POINT_ID, BareSoil.Albedo, PET, PPT)

pt_all <- left_join(pt, dat, by = 'POINT_ID')



g.clsp.BareSoilA.SM <- ggplot(pt_all%>% 
                                filter(!is.na(BareSoil.Albedo))) + 
  geom_point(aes(x = PPT/PET, y = SM, colour = BareSoil.Albedo), size = pointSize)+
  scale_colour_viridis_c("Bare soil albedo",
                         limits = c(0.05,0.25), 
                         option = "viridis", 
                         oob = squish)+
  labs(tag = 'a') + 
  custom_theme +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))



g.clsp.BareSoilA.SM <- ggplot(pt_all %>% 
                                filter(!is.na(BareSoil.Albedo)) %>%
                                mutate(CaCO3_cat = cut(CaCO3, c(0,1,20,250,900),include.lowest = T))) + 
  geom_point(aes(x = PPT/PET, y = SM, colour = BareSoil.Albedo), size = pointSize)+
  scale_colour_viridis_c("Bare soil albedo",
                         limits = c(0.05,0.20), 
                         option = "viridis", 
                         oob = squish) +
  facet_wrap(~CaCO3_cat, labeller = label_both) + 
  labs(tag = 'a') + 
  custom_theme +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))

ggsave(filename = 'testfig___albedo_SM_CaCO3_cat.png', path = fpath,
       plot = g.clsp.BareSoilA.SM, width = 10, height = 10)

