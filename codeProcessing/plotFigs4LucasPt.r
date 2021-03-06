plotFigs4LucasPt <- function(Lpt=Lpt,fpath=fpath,rawCsv=rawCsv,minSNR=minSNR,minDist=minDist){

require(tidyverse)
library(RColorBrewer)

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
         BSA_vct=Albedo_BSA_shortwave, WSA_vct=Albedo_WSA_shortwave, 
         ALB_qlty_vct=BRDF_Albedo_Band_Mandatory_Quality_shortwave,
         NPP_vct=PsnNet,NPP_qlty_vct=Psn_QC) %>%
  filter(POINT_ID.x==Lpt) 

subDat$BSA_vct[is.na(subDat$BSA_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$WSA_vct[is.na(subDat$WSA_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$ALB_qlty_vct[is.na(subDat$ALB_qlty_vct)] <- paste(rep('NA',ws^2),collapse=',')


# prepare dataframe to host exploded values from the window
dat <- data.frame(idpt=rep(1:ws^2,times=dim(subDat)[1]),
                  time=as.Date(rep(subDat$time,each=ws^2),format = '%Y_%m_%d'))

# explose the cells with multiple values of the window
dat$SNR=as.numeric(as.vector(unlist(strsplit(subDat$SNR_vct, ","))))
dat$WSA=as.numeric(as.vector(unlist(strsplit(subDat$WSA_vct, ","))))*0.001
dat$BSA=as.numeric(as.vector(unlist(strsplit(subDat$BSA_vct, ","))))*0.001
dat$NPP=as.numeric(as.vector(unlist(strsplit(subDat$NPP_vct, ","))))*0.0001
dat$NPP_qlty=as.integer(as.vector(unlist(strsplit(subDat$NPP_qlty_vct, ","))))
dat$ALB_qlty=as.integer(as.vector(unlist(strsplit(subDat$ALB_qlty_vct, ","))))

# calculate distances within the window
dd <- data.frame(idpt=1:ws^2,row=rep(1:ws,each=ws),col=rep(1:ws,times=ws))
dd$dist <- sqrt((dd$col-11)^2 + (dd$row-11)^2)

# further prepare the dataframe
dat <- dat %>%
  filter(SNR!=0)%>% # removes areas that have been masked in GEE
  mutate(ALB=0.5*(WSA+BSA))%>% # combines black sky and white sky albedos evenly
  left_join(dd,by = "idpt") # join it with a filter

# Now we can start the plots/...
fname <- paste0('pt',Lpt,'_minSNR',minSNR,'_minDist',minDist)
fmt <- '.png'

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

#ggplot(filter(dat,NPP_qlty %in% NPP_ok_flags,SNR>minSNR))+
g2 <- ggplot(filter(dat,SNR>minSNR,dist<minDist))+
  geom_raster(aes(x=col,y=row,fill=NPP))+
  facet_wrap(~time,nc=9)+
  scale_fill_viridis_c()+  
  coord_fixed(ylim=c(1,ws),xlim=c(1,ws))+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave(filename = paste0(fpath,fname,'_2tsNPP.',fmt),plot=g2,width=11,height = 7)

g3 <- ggplot(filter(dat,SNR>minSNR,dist<minDist))+
  geom_raster(aes(x=col,y=row,fill=ALB))+
  facet_wrap(~time,nc=9)+
  scale_fill_viridis_c(option='B',limits=c(0.1,0.3),oob=squish)+
  coord_fixed(ylim=c(1,ws),xlim=c(1,ws))+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave(filename = paste0(fpath,fname,'_3tsALB.',fmt),plot=g3,width=11,height = 7)

# base filter
datf0 <- filter(dat,SNR>minSNR,ALB>0,ALB<0.4,dist<minDist)

# get maxtime
dum <-datf0 %>% group_by(time) %>% summarise(meanNPP=mean(NPP,na.rm=T))
timeMax <- dum$time[which(dum$meanNPP==max(dum$meanNPP))]

# plot time series for central pixel (including its flags)
datT <- filter(dat,idpt==221)%>%gather(variable,value,ALB,NPP)
g4 <- ggplot(datT)+
  geom_point(aes(x=time,y=value,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  geom_point(data=filter(datT,!ALB_qlty==0),aes(x=time,y=value), size=4,shape=2, colour='orange')+
  geom_point(data=filter(datT,!NPP_qlty %in% NPP_ok_flags),aes(x=time,y=value), shape=3, colour='red',size=4)+
  facet_wrap(~variable,nc=1,scales="free") + 
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  scale_y_continuous()+
  ggtitle(paste('LUCAS pt',Lpt, ' for central pixel'))+
  theme(legend.position = 'none')
ggsave(filename = paste0(fpath,fname,'_4centralPix.',fmt),plot=g4,width=11,height = 7)


# explore results with different filtering types
datf1 <- filter(datf0,ALB_qlty==0)
datf2 <- filter(datf0,NPP_qlty %in% NPP_ok_flags)
datf3 <- filter(datf0,ALB_qlty==0,NPP_qlty %in% NPP_ok_flags)


g5 <- ggplot(datf0) + 
  geom_point(aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey40',size=3)+
  geom_smooth(data=filter(datf0,time<=timeMax),aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[2])+
  geom_smooth(data=filter(datf0,time>=timeMax),aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[8])+
  geom_point(data=filter(datf0,dist==0),aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey20',size=6)+
  # geom_smooth(aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  ggtitle(paste('LUCAS pt ',Lpt,' SNR >',minSNR, ' dist <',minDist,' NO flag filtering'))+
  theme(legend.position = c(0.8,0.2))
ggsave(filename = paste0(fpath,fname,'_5_ALBvsNPP_noFilter.',fmt),plot=g5,width=7,height = 7)


g6 <- ggplot(datf1) + 
  geom_point(aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey40',size=3)+
  geom_smooth(data=filter(datf1,time<=timeMax),aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[2])+
  geom_smooth(data=filter(datf1,time>=timeMax),aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[8])+
  geom_point(data=filter(datf1,dist==0),aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey20',size=6)+
  # geom_smooth(aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  ggtitle(paste('LUCAS pt ',Lpt,' SNR >',minSNR, ' dist <',minDist,' Filtering ALB'))+
  theme(legend.position = c(0.8,0.2))
ggsave(filename = paste0(fpath,fname,'_6_ALBvsNPP_filterALB.',fmt),plot=g6,width=7,height = 7)

g7 <- ggplot(datf2) + 
  geom_point(aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey40',size=3)+
  geom_smooth(data=filter(datf2,time<=timeMax),aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[2])+
  geom_smooth(data=filter(datf2,time>=timeMax),aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[8])+
  geom_point(data=filter(datf2,dist==0),aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey20',size=6)+
  # geom_smooth(aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  ggtitle(paste('LUCAS pt',Lpt,' SNR >',minSNR, ' dist <',minDist,' Filtering NPP'))+
  theme(legend.position = c(0.8,0.2))
ggsave(filename = paste0(fpath,fname,'_7_ALBvsNPP_filterNPP.',fmt),plot=g7,width=7,height = 7)

g8 <- ggplot(datf3) + 
  geom_point(aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey40',size=3)+
  geom_smooth(data=filter(datf3,time<=timeMax),aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[2])+
  geom_smooth(data=filter(datf3,time>=timeMax),aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"),colour=pal[8])+
  geom_point(data=filter(datf3,dist==0),aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey20',size=6)+
  # geom_smooth(aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  ggtitle(paste('LUCAS pt',Lpt,' SNR >',minSNR, ' dist <',minDist,' Filtering both NPP and ALB'))+
  theme(legend.position = c(0.8,0.2))
ggsave(filename = paste0(fpath,fname,'_8_ALBvsNPP_filterBOTH.',fmt),plot=g8,width=7,height = 7)


}
