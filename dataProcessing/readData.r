library(tidyverse)

require(wesanderson)
pal <- wes_palette('Darjeeling',3,type='continuous')

library(RColorBrewer)
pal <- rev(brewer.pal(9,'YlGn'))

as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
}


# set window size
ws = 21

rawCsv <- readr::read_csv('/DATA/scratch/SOIL_LUCAS/FileGregAll.csv')

subDat <- rawCsv %>% 
  rename(SNR_vct=`SNR_filter[, 22]`, ALB_vct=AlbedoMean, NPP_vct=PsnNet) %>%
  filter(sample_ID.x==11388) 

subDat$ALB_vct[is.na(subDat$ALB_vct)] <- paste(rep('NA',ws^2),collapse=',')
# 10745
# 11388


dat <- data.frame(idpt=rep(1:ws^2,each=dim(subDat)[1]),
                  row=rep(1:ws,times=dim(subDat)[1]*ws),
                  col=rep(1:ws,each=dim(subDat)[1]*ws),
                  time=as.Date(rep(subDat$time,times=ws^2),format = '%Y_%m_%d'))


dat$SNR=as.integer(as.vector(unlist(strsplit(subDat$SNR_vct, ","))))
dat$ALB=as.integer(as.vector(unlist(strsplit(subDat$ALB_vct, ","))))
dat$NPP=as.integer(as.vector(unlist(strsplit(subDat$NPP_vct, ","))))

iPT <- 25

ggplot(filter(dat,idpt==iPT))+
  geom_point(aes(x=time,y=NPP,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  theme(legend.position = 'none')

ggplot(filter(dat,idpt==iPT))+
  geom_point(aes(x=time,y=ALB,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  theme(legend.position = 'none')

ggplot(filter(dat,idpt==iPT,NPP!=0,ALB!=0))+
  geom_point(aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  geom_smooth(aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  theme(legend.position = c(0.8,0.2))
  
ggplot(filter(dat,time=='2009-04-23'))+
  geom_raster(aes(x=col,y=row,fill=NPP))



ggplot(filter(dat,NPP!=0,ALB!=0,SNR>10)) + 
  geom_point(aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  geom_smooth(aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  theme(legend.position = c(0.8,0.2))
  


