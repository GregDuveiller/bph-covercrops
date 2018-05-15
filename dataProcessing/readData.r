library(tidyverse)

require(wesanderson)
pal <- wes_palette('Darjeeling',3,type='continuous')

library(RColorBrewer)
pal <- rev(brewer.pal(9,'YlGn'))

as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
}


#dpath <- '/ESS_Datasets/USERS/Duveiller/Workspace/SOIL_LUCAS/'
dpath <- '/DATA/scratch/SOIL_LUCAS/'

# set window size
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



rawCsv <- readr::read_csv(paste0(dpath,'FileGregAllFlags.csv'))

subDat <- rawCsv %>% 
  rename(SNR_vct=`SNR_filter[, 22]`, 
         BSA_vct=Albedo_BSA_shortwave, WSA_vct=Albedo_WSA_shortwave, 
         ALB_qlty_vct=BRDF_Albedo_Band_Mandatory_Quality_shortwave,
         NPP_vct=PsnNet,NPP_qlty_vct=Psn_QC) %>%
  filter(POINT_ID.x==48103100) 

subDat$BSA_vct[is.na(subDat$BSA_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$WSA_vct[is.na(subDat$WSA_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$ALB_qlty_vct[is.na(subDat$ALB_qlty_vct)] <- paste(rep('NA',ws^2),collapse=',')

#36822858
#37362822  # reasonably
#42662462  # Nothing there!
#41862472  # Nothing there!


dat <- data.frame(idpt=rep(1:ws^2,times=dim(subDat)[1]),
                  # col=rep(rep(1:ws,times=ws),each=dim(subDat)[1]),
                  # row=rep(rep(1:ws,each=ws),each=dim(subDat)[1]),
                  time=as.Date(rep(subDat$time,each=ws^2),format = '%Y_%m_%d'))

dat$SNR=as.numeric(as.vector(unlist(strsplit(subDat$SNR_vct, ","))))
dat$WSA=as.numeric(as.vector(unlist(strsplit(subDat$WSA_vct, ","))))*0.001
dat$BSA=as.numeric(as.vector(unlist(strsplit(subDat$BSA_vct, ","))))*0.001
dat$NPP=as.numeric(as.vector(unlist(strsplit(subDat$NPP_vct, ","))))*0.0001
dat$NPP_qlty=as.integer(as.vector(unlist(strsplit(subDat$NPP_qlty_vct, ","))))
dat$ALB_qlty=as.integer(as.vector(unlist(strsplit(subDat$ALB_qlty_vct, ","))))


dd <- data.frame(idpt=1:ws^2,row=rep(1:ws,each=ws),col=rep(1:ws,times=ws))
dd$dist <- sqrt((dd$col-11)^2 + (dd$row-11)^2)

  
dat <- dat %>%
  filter(SNR!=0)%>%
  mutate(ALB=0.5*(WSA+BSA))%>%
  left_join(dd,by = "idpt")


minSNR <- 10

ggplot(filter(dat,time=='2008-10-07'))+
  geom_raster(aes(x=col,y=row,fill=SNR))+
  facet_wrap(~time)+
  scale_fill_viridis_c(option='C',limits=c(0,70),oob=squish)

ggplot(filter(dat,time=='2008-10-07'))+
  geom_raster(aes(x=col,y=row,fill=dist))+
  facet_wrap(~time)+
  scale_fill_viridis_c(option='C')

ggplot(filter(dat,NPP_qlty %in% NPP_ok_flags,SNR>minSNR))+
  geom_raster(aes(x=col,y=row,fill=NPP))+
  facet_wrap(~time)+
  scale_fill_viridis_c()

ggplot(filter(dat,SNR>minSNR,ALB_qlty%in%c(0,NA)))+
  geom_raster(aes(x=col,y=row,fill=ALB))+
  facet_wrap(~time)+
  scale_fill_viridis_c(option='B',limits=c(0.1,0.3))


ggplot(filter(dat,SNR>minSNR,ALB>0,ALB<0.4,NPP_qlty %in% NPP_ok_flags,dist<5)) + 
  geom_point(aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  geom_smooth(aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  theme(legend.position = c(0.8,0.2))

ggplot(filter(dat,SNR>minSNR,ALB>0,ALB<0.4,NPP_qlty %in% NPP_ok_flags,dist<5)) + 
  geom_point(aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  facet_wrap(~time)+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)


ggplot(filter(dat,dist==0))+
  geom_point(aes(x=time,y=NPP,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  theme(legend.position = 'none')

ggplot(filter(dat,dist==0,ALB<0.4))+
  geom_point(aes(x=time,y=ALB,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  theme(legend.position = 'none')




ggplot(filter(dat,idpt==iPT,NPP!=0,WSA!=0))+
  geom_point(aes(x=NPP,y=WSA,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  geom_smooth(aes(x=NPP,y=WSA),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  theme(legend.position = c(0.8,0.2))
  
ggplot(filter(dat,time=='2009-05-01'))+
  geom_raster(aes(x=col,y=row,fill=SNR))



ggplot(filter(dat,NPP!=0,ALB!=0,SNR>10,dist<5)) + 
  geom_point(aes(x=NPP,y=ALB,fill=as.integer(time)), shape=21, colour='grey30',size=3)+
  geom_smooth(aes(x=NPP,y=ALB),method = 'gam', formula = y ~ s(x, bs = "cs"))+
  scale_fill_gradientn('Time',colours=pal, labels=as.Date_origin)+
  theme(legend.position = c(0.8,0.2))
  


