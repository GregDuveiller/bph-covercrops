# Draft for figure with country level statistics...
require(dplyr)
require(ggplot2)
require(sf)
require(here)

load(file = 'dataFigures/redux_data4fig_normal.Rda') # 'GHGbdg','GHGsen', 'aLCS'


#dat <- read.csv(file = 'dataFigures/alb_elabo3.1.PET.csv')

pt_info <-  sf::st_read('dataInput/LUCAS_ARABLE.shp')
dat.all <- left_join(pt_info, GHGsen, by = c('POINT_ID', 'EOBid', 'sample_ID', 'GPS_LAT', 'GPS_LONG')) 

dat.all <-left_join(GHGsen, pt_info %>% select(POINT_ID, NUT0), 
                     by = c('POINT_ID')) 

df.summary <- dat.all %>%
  select(GHGt00, GHGt30, NUT0) %>%
  group_by(NUT0) %>%
  dplyr::summarize(GHGt00.mu = mean(GHGt00, na.rm = T),
                   GHGt00.sd = sd(GHGt00, na.rm = T),
                   GHGt30.mu = mean(GHGt30, na.rm = T),
                   GHGt30.sd = sd(GHGt30, na.rm = T))

# NEED TO ASK LUGATO
# not sure if what I need to use is GHSt or that time GHGr to get bph and bgc


ggplot(df.summary) +
  geom_bar(aes(x = NUT0, y = GHGt30.mu), stat = 'identity')


