# Draft for figure with country level statistics...
require(dplyr)
require(ggplot2)
require(sf)
require(here)


# get point data
pt_info <-  sf::st_read('dataInput/LUCAS_ARABLE.shp') %>%
  as.data.frame() %>%
  dplyr::select(POINT_ID, NUT0) %>%
  dplyr::rename(Country = NUT0)


# function to grab the data
get.df <- function(yr = '2100', ccType = 'Normal'){
  
  bph.sub <- bph %>% 
    dplyr::select('POINT_ID', paste0('yr_', yr)) %>%
    dplyr::rename('BPH' = paste0('yr_', yr))
  
  bgc.sub <- bgc %>% 
    dplyr::select('POINT_ID', paste0('yr_', yr)) %>%
    dplyr::rename('BGC' = paste0('yr_', yr)) 
  
  dat <- pt_info %>%
    right_join(bph.sub, by = c('POINT_ID')) %>%
    right_join(bgc.sub, by = c('POINT_ID')) %>%
    group_by(Country) %>%
    dplyr::summarise(BPH = mean(BPH, na.rm = T),
                     BGC = mean(BGC, na.rm = T)) %>%
    tidyr::pivot_longer(cols = c('BPH', 'BGC'),
                        names_to = 'Type', values_to = 'C02.eq') %>%
    mutate(yr = yr, ccType = ccType)
  
  return(dat)}


# start getting data for normal case ----
load(file = 'dataFigures/new_data4fig_normal.Rda') # 'GHGbdg','GHGsen', 'aLCS', "N2O_dif", "SOC_dif", "alfa_dif"

# prepare biogeochemical effects
bgc <- as.data.frame(N2O_dif + SOC_dif)
colnames(bgc) <- paste0('yr_',seq(2015,2100))
bgc$POINT_ID <- aLCS$POINT_ID 

# prepare biophysical effects
bph <- as.data.frame(alfa_dif)
colnames(bph) <- paste0('yr_',seq(2015,2100))
bph$POINT_ID <- aLCS$POINT_ID 

# get the needed data for the normal case
dat.normal <- bind_rows(
  get.df(yr = '2030', ccType = 'Normal'),
  get.df(yr = '2050', ccType = 'Normal'),
  get.df(yr = '2100', ccType = 'Normal'))
  

# start getting data for mutant case ----
load(file = 'dataFigures/new_data4fig_mutant.Rda') # 'GHGbdg','GHGsen', 'aLCS', "N2O_dif", "SOC_dif", "alfa_dif"

# prepare biogeochemical effects
bgc <- as.data.frame(N2O_dif + SOC_dif)
colnames(bgc) <- paste0('yr_',seq(2015,2100))
bgc$POINT_ID <- aLCS$POINT_ID 

# prepare biophysical effects
bph <- as.data.frame(alfa_dif)
colnames(bph) <- paste0('yr_',seq(2015,2100))
bph$POINT_ID <- aLCS$POINT_ID 

# get the needed data for the normal case
dat.mutant <- bind_rows(
  get.df(yr = '2030', ccType = 'Mutant'),
  get.df(yr = '2050', ccType = 'Mutant'),
  get.df(yr = '2100', ccType = 'Mutant'))



# make the final plot -----

dat <- bind_rows(dat.normal, dat.mutant)

g.country <- ggplot(dat) +
  geom_bar(aes(x = reorder(Country, -C02.eq), y = C02.eq, fill = Type), 
           stat = 'identity', position = 'stack') + 
  coord_flip() +
  facet_grid(ccType ~ yr) +
  scale_y_reverse('Potential mitigation potential in CO2 eq.') +
  scale_x_discrete('') + 
  scale_fill_discrete('Type of effect', labels = c('BGC' = 'Biogeochemical', 'BPH' = 'Biogeophysical')) +
  theme(legend.position = 'bottom')

ggsave('textFigures/test_fig___CountryStat.png', plot = g.country, width = 10, height = 8)
