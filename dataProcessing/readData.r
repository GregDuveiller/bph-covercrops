library(tidyverse)

require(wesanderson)
pal <- wes_palette('Darjeeling',3,type='continuous')

library(RColorBrewer)
pal <- rev(brewer.pal(9,'YlGn'))

as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
}

rawCsv <- readr::read_csv('/ESS_Datasets/USERS/Duveiller/Workspace/SOIL_LUCAS/FileGregAll.csv')

subDat <- rawCsv %>% 
  rename(SNR_vct=`SNR_filter[, 22]`, ALB_vct=AlbedoMean, NPP_vct=PsnNet) %>%
  filter(sample_ID.x==10745, !is.na(ALB_vct)) 

# 10745
# 11388

dat <- data.frame(idpt=rep(1:441,each=dim(subDat)[1]),time=as.Date(rep(subDat$time,times=441),format = '%Y_%m_%d'))

dat$SNR=as.integer(as.vector(unlist(strsplit(subDat$SNR_vct, ","))))
dat$ALB=as.integer(as.vector(unlist(strsplit(subDat$ALB_vct, ","))))
dat$NPP=as.integer(as.vector(unlist(strsplit(subDat$NPP_vct, ","))))

ggplot(filter(dat,idpt==242))+
  geom_point(aes(x=time,y=NPP))
  

ggplot(filter(dat,NPP!=0,ALB!=0,SNR>15)) + 
  geom_point(aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  geom_smooth(aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  theme(legend.position = c(0.8,0.2))
  

