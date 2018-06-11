plotFigs4LucasPt <- function(Lpt=Lpt,fpath=fpath,rawCsv=rawCsv,minSNR=minSNR,minDist=minDist){

require(tidyverse)
require(scales)
  require(grid)
  require(RColorBrewer)

pal <- rev(brewer.pal(9,'YlGn'))
as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
}

# declare window size
ws = 21

# fucntion to get flags (including ambiguity)
bin2decXlist <- function(x){   
  # function to get binary values
  bin2dec <- function(x) sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))
  # ID of 'x's
  vars=which(unlist(strsplit(as.character(x), ""))=='x')
  # if none...
  if(identical(vars,integer(0))) {outlist=bin2dec(x)} else {
    a=x 
    for(i in 1:length(vars)){     
      a=matrix(rep(a,2),ncol=2)
      for(j in 1:dim(a)[1]){
        dum=unlist(strsplit(as.character(a[j,1]), ""))
        dum[vars[i]]=0
        a[j,1]=paste(dum,collapse='')
        dum=unlist(strsplit(as.character(a[j,2]), ""))
        dum[vars[i]]=1
        a[j,2]=paste(dum,collapse='')
      }
      a=as.vector(a)
    }
    outlist= sort(apply(matrix(a),1,bin2dec))
  }
  return(outlist)
}
NPP_ok_flags <- bin2decXlist('00x000x0')

# subset data and sort out some column names
subDat <- rawCsv %>% 
  rename(SNR_vct=`SNR_filter[, 22]`, 
         NDV_vct=NDVI,
         NDV_qlty_vct=BRDF_Albedo_Band_Mandatory_Quality_Band1,
         ALB_vct=AlbedoMean, 
         ALB_qlty_vct=BRDF_Albedo_Band_Mandatory_Quality_shortwave,
         NPP_vct=PsnNet,NPP_qlty_vct=Psn_QC) %>%
  filter(POINT_ID.x==Lpt) 

subDat$NDV_vct[is.na(subDat$NDV_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$NDV_qlty_vct[is.na(subDat$NDV_qlty_vct)] <- paste(rep('NA',ws^2),collapse=',')

subDat$ALB_vct[is.na(subDat$ALB_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$ALB_qlty_vct[is.na(subDat$ALB_qlty_vct)] <- paste(rep('NA',ws^2),collapse=',')

# prepare dataframe to host exploded values from the window
dat <- data.frame(idpt=rep(1:ws^2,times=dim(subDat)[1]),
                  time=as.Date(rep(subDat$time,each=ws^2),format = '%Y_%m_%d'))

# explose the cells with multiple values of the window
dat$SNR=as.numeric(as.vector(unlist(strsplit(subDat$SNR_vct, ","))))
dat$NDV=as.numeric(as.vector(unlist(strsplit(subDat$NDV_vct, ","))))
dat$ALB=as.numeric(as.vector(unlist(strsplit(subDat$ALB_vct, ","))))*0.001
dat$NPP=as.numeric(as.vector(unlist(strsplit(subDat$NPP_vct, ","))))*0.0001
dat$NPP_qlty=as.integer(as.vector(unlist(strsplit(subDat$NPP_qlty_vct, ","))))
dat$ALB_qlty=as.integer(as.vector(unlist(strsplit(subDat$ALB_qlty_vct, ","))))
dat$NDV_qlty=as.integer(as.vector(unlist(strsplit(subDat$NDV_qlty_vct, ","))))

# calculate distances within the window
dd <- data.frame(idpt=1:ws^2,row=rep(1:ws,each=ws),col=rep(1:ws,times=ws))
dd$dist <- sqrt((dd$col-11)^2 + (dd$row-11)^2)

# further prepare the dataframe
dat <- dat %>%
  filter(SNR!=0)%>% # removes areas that have been masked in GEE
  left_join(dd,by = "idpt") # join it with a filter

# Now we can start the plots/...
fname <- paste0('pt',Lpt,'_minSNR',minSNR,'_minDist',minDist)
fmt <- '.png'



require(sf)

world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)

laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"


europe_laea <- sf::st_intersection(world,st_set_crs(st_as_sf(as(raster::extent(-10, 50, 25, 70), "SpatialPolygons")), st_crs(nc)))%>%
  st_transform(laes_prj)
  
pts <- st_sfc(st_point(c(1,50), dim='XY'), crs=4326) %>%
  st_transform(laes_prj)

  
ggplot(europe_laea) +
  geom_sf() +
  geom_sf(data=pts,colour="red",size=4)





# Plot of the SNR geography around the point
g1 <- ggplot(filter(dat,time=='2008-10-07'))+
  geom_raster(aes(x=col,y=row,fill=SNR))+
  geom_point(data=filter(dat,idpt==221,time=='2008-10-07'),aes(x=col,y=row),size=5,shape=3,colour='cyan')+
  geom_point(data = filter(dat,time=='2008-10-07', SNR > minSNR,dist<minDist),aes(x=col,y=row),
             shape=21,fill='grey80',colour='grey50',size=4)+
  scale_fill_viridis_c(option='C',limits=c(0,70),oob=squish)+
  coord_fixed()+
  ggtitle(paste('LUCAS pt',Lpt,' SNR >',minSNR, ' dist <',minDist))
ggsave(filename = paste0(fpath,fname,'_1SNR.',fmt),plot=g1,width=7,height = 7)



# base filter
datf0 <- filter(dat,SNR>minSNR,ALB>0,ALB<0.3,dist<minDist)

# get maxtime
dum <-datf0 %>% group_by(time) %>% summarise(meanNDV=mean(NDV,na.rm=T))
timeMax <- dum$time[which(dum$meanNDV==max(dum$meanNDV))]

# plot time series for central pixel (including its flags)
datT <- filter(dat,idpt==221)%>%gather(variable,value,ALB,NDV,NPP)

g4 <- ggplot(datT)+
  geom_point(aes(x=time,y=value,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  geom_point(data=filter(datT,!NDV_qlty==0),aes(x=time,y=value), shape=3, colour='red',size=4)+
  geom_point(data=filter(datT,!ALB_qlty==0),aes(x=time,y=value), size=4,shape=2, colour='orange')+
  geom_point(data=filter(datT,!NPP_qlty %in% NPP_ok_flags),aes(x=time,y=value), size=4,shape=2, colour='orange')+
  facet_wrap(~variable,nc=1,scales="free") + 
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  scale_y_continuous()+
  ggtitle(paste('LUCAS pt',Lpt, ' for central pixel'))+
  theme(legend.position = 'none')


# explore results with different filtering types
dat.filter.all <- filter(datf0,ALB_qlty==0,NDV_qlty==0)


g5 <- ggplot(datf0) + 
  geom_point(aes(x=NDV,y=ALB,fill=as.integer(time)), shape=21, colour='grey40',size=3)+
  geom_smooth(data=filter(datf0,time<=timeMax),aes(x=NDV,y=ALB),
              method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[2],fullrange=T)+
  geom_smooth(data=filter(datf0,time>=timeMax),aes(x=NDV,y=ALB),
              method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[8],fullrange=T)+
  geom_point(data=filter(datf0,dist==0),aes(x=NDV,y=ALB,fill=as.integer(time)), shape=21, colour='grey20',size=6)+
  # geom_smooth(aes(x=NDV,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  scale_y_continuous(limits=c(0.1,0.3))+
  scale_x_continuous(limits=c(0.1,0.9))+
  ggtitle(paste('LUCAS pt ',Lpt,' SNR >',minSNR, ' dist <',minDist,' '))+
  theme(legend.position = c(0.8,0.2))



g6 <- ggplot(dat.filter.all) + 
  geom_point(aes(x=NDV,y=ALB,fill=as.integer(time)), shape=21, colour='grey40',size=3)+
  geom_smooth(data=filter(dat.filter.all,time<=timeMax),aes(x=NDV,y=ALB),
              method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[2],fullrange=T)+
  geom_smooth(data=filter(dat.filter.all,time>=timeMax),aes(x=NDV,y=ALB),
              method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[8],fullrange=T)+
  geom_point(data=filter(datf0,dist==0),aes(x=NDV,y=ALB,fill=as.integer(time)), shape=21, colour='grey20',size=6)+
  # geom_smooth(aes(x=NDV,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  scale_y_continuous(limits=c(0.1,0.3))+
  scale_x_continuous(limits=c(0.1,0.9))+
  ggtitle(paste('LUCAS pt ',Lpt,' SNR >',minSNR, ' dist <',minDist,' '))+
  theme(legend.position = c(0.8,0.2))




ggsave(filename = paste0(fpath,fname,'_5_ALBvsNDV_noFilter.',fmt),plot=g5,width=7,height = 7)


}
