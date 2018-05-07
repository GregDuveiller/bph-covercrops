library(tidyverse)

rawCsv <- readr::read_csv('/ESS_EarthObs/DATA_PRODUCTS/SOIL_LUCAS/FileGregAll.csv')

subDat <- rawCsv %>% 
  rename(SNR_vct=`SNR_filter[, 22]`, ALB_vct=AlbedoMean, NPP_vct=PsnNet) %>%
  filter(sample_ID.x==3636) 

dat <- data.frame(idpt=rep(1:441,each=45),time=as.Date(rep(subDat$time,times=441),format = '%Y_%m_%d'))
dat$SNR=as.integer(as.vector(unlist(strsplit(subDat$SNR_vct, ","))))
dat$ALB=as.integer(as.vector(unlist(strsplit(subDat$ALB_vct, ","))))
dat$NPP=as.integer(as.vector(unlist(strsplit(subDat$NPP_vct, ","))))

