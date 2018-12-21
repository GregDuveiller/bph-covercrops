rm(list=ls())
gc()
gc()
gc()
gc()
gc()



library(dplyr) 
library (plyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)



setwd("/mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/ICEARC/LUCAS/R_code")
#############################################################
####SED: use from terminal to remove and replace brackets
#############################################################

# sed -e 's/\[//g' -e 's/\]//g' /data/cecchgu/LUCAS/DATA_CSV_GEE/LabeledPatchesgpp.csv > /data/cecchgu/LUCAS/DATA_CSV_GEE/LabeledPatchesgpp_clean.csv
# 
# sed -e 's/\[//g' -e 's/\]//g' /data/cecchgu/LUCAS/DATA_CSV_GEE/LabeledPatchesalbedo.csv > /data/cecchgu/LUCAS/DATA_CSV_GEE/LabeledPatchesalbedo_clean.csv
# 
# sed -e 's/\[//g' -e 's/\]//g' /data/cecchgu/LUCAS/DATA_CSV_GEE/LabeledPatchesSNR.csv > /data/cecchgu/LUCAS/DATA_CSV_GEE/LabeledPatchesSNR_clean.csv
# 
# sed -e 's/\[//g' -e 's/\]//g' /data/cecchgu/LUCAS/DATA_CSV_GEE/LabeledPatchesReflect.csv > /data/cecchgu/LUCAS/DATA_CSV_GEE/LabeledPatchesReflect_clean.csv


#############################################################

##open files

for (year in 2003:2016){


SNR <- read_csv(paste0('/mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/ICEARC/LUCAS//Data_CSV_GEE DEC18//LabeledPatchesSNR_clean',year,'.csv'))#, stringsAsFactors=FALSE)
Albedo <- read_csv(paste0('/mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/ICEARC/LUCAS/Data_CSV_GEE DEC18/LabeledPatchesalbedo_clean',year,'.csv'))#, stringsAsFactors=FALSE)
Refl <- read_csv(paste0('/mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/ICEARC/LUCAS/Data_CSV_GEE DEC18/LabeledPatchesReflect_clean',year,'.csv'))#, stringsAsFactors=FALSE)

GPP <- read_csv(paste0('/mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/ICEARC/LUCAS/Data_CSV_GEE DEC18/LabeledPatchesgpp_clean',year,'.csv'))#, stringsAsFactors=FALSE)
QUAL <- read_csv(paste0('/mnt/cidstorage/cid_bulk_22/cid-bulk22/Shared/projectData/ICEARC/LUCAS/Data_CSV_GEE DEC18/LabeledPatchesQUAL_clean',year,'.csv'))#, stringsAsFactors=FALSE)


###### create new dataframe

##use sample_ID POINT_ID time (fist col-first part -> substr(GPP[1,1],1,10)) albedo, PsnNet, Albedo SNR

#explore a little bit
GPP <- as.data.frame(GPP)
Albedo <- as.data.frame(Albedo)
SNR <- as.data.frame(SNR)
Refl <- as.data.frame(Refl)
QUAL <- as.data.frame(QUAL)

unique_ID_GPP <- unique(GPP$POINT_ID)
Time_step_GPP <- substr(GPP[,1],1,10)
length(unique(Time_step_GPP))
GPP$time <- Time_step_GPP

unique_ID_Refl <- unique(Refl$POINT_ID)
Time_step_Refl <- substr(Refl[,1],1,10)
length(unique(Time_step_Refl))
Refl$time <- Time_step_Refl


unique_ID_Qual <- unique(QUAL$POINT_ID)
Time_step_Qual <- substr(QUAL[,1],1,10)
length(unique(Time_step_Qual))
QUAL$time <- Time_step_Qual

unique_ID_SNR <- unique(SNR$POINT_ID)

unique_ID_ALBEDO <- unique(Albedo$POINT_ID)
Time_step_ALBEDO <- substr(Albedo[,1],1,10)
length(unique(Time_step_ALBEDO))
Albedo$time <- Time_step_ALBEDO



datalist = list()


for (i in 1:length(unique_ID_GPP)){
  
  GPP_filter <- GPP[GPP$POINT_ID == unique_ID_GPP[[i]],] ## c(26,22,15,16,17)
  Refl_filter <- Refl[Refl$POINT_ID == unique_ID_GPP[[i]],] ##c(26,22,17,12,2)
  SNR_filter <- SNR[SNR$POINT_ID == unique_ID_GPP[[i]],]
  Albedo_filter <- Albedo[Albedo$POINT_ID == unique_ID_GPP[[i]],] ##c(26,22,17,2,3)
  Qual_filter <- QUAL[QUAL$POINT_ID == unique_ID_GPP[[i]],c(25,21,15,16)]
  

  if (nrow(SNR_filter) >0 & nrow(Albedo_filter) >0){
    dat_1 <- cbind(GPP_filter,SNR_filter[,22])
    # dat_1b <- cbind(dat_1,Refl_filter)
    dat_1b <- dplyr::left_join(dat_1,Refl_filter, by='time')
  dat_2 <- dplyr::left_join(dat_1b,Albedo_filter, by='time' )
  dat_2b <- dplyr::left_join(dat_2,Qual_filter, by='time' )
  # dat_2 <- cbind(Albedo_filter[,],SNR_filter)
  
  dat_2b <- dat_2b[,c(1:8,14,20,21,24)]
  
  datalist[[i]] <- dat_2b # add it to your list
  # datalist_albedo[[i]] <- dat_2 # add it to your list
  # datalist_GPP[[i]] <- dat_1 # add it to your list
}
}

##trasform list -> dataframe and save
# big_data_albedo = do.call(rbind, datalist_albedo)
# big_data_GPP = do.call(rbind, datalist_GPP)
big_data = do.call(rbind, datalist)

# big_data$PsnNet <- as.list(as.integer(as.vector(unlist(strsplit(big_data$PsnNet, ",")))))
# big_data$SN <- as.list(as.integer(as.vector(unlist(strsplit(big_data$SN, ",")))))
# big_data$AlbedoMean <- as.list(as.double(as.vector(unlist(strsplit(big_data$AlbedoMean, ",")))))


# write.csv(big_data_albedo, file = "FileGregAlbedo.csv")
# write.csv(big_data_GPP, file = "FileGregGPP.csv")

namefile = paste0("FileGregAllFlagsNDVI",year,".csv")
write.csv(big_data, file =namefile)



}

##### distance matrix
## TODO : better redine the extent to have a general idea of the distance in a second phase!
# 
# r <- raster(ncol=21,nrow=21, xmn=24.957, xmx=25.043, ymn=42.957, ymx=43.043, resolution = c(0.00416667,0.00416667))
# xy <- c(25,43)
# d1 <- distanceFromPoints(r, xy) 
# crs(r) = '+init=epsg:4326'
# d2 <- distanceFromPoints(r, xy) 
# 
# plot(d2)

