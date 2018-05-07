rm(list=ls())
gc()
gc()
gc()
gc()
gc()



library(dplyr) 
library (plyr)
library(ggplot2)


setwd("~/Documents/LUCAS")

#############################################################
####SED: use from terminal to remove and replace brackets
#############################################################

sed -e 's/\[//g' -e 's/\]//g' /home/cecchgu/Downloads/LabeledPatchesgpp.csv > /home/cecchgu/Downloads/LabeledPatchesgpp_clean.csv

sed -e 's/\[//g' -e 's/\]//g' /home/cecchgu/Downloads/LabeledPatchesalbedo.csv > /home/cecchgu/Downloads/LabeledPatchesalbedo_clean.csv

sed -e 's/\[//g' -e 's/\]//g' /home/cecchgu/Downloads/LabeledPatchesSNR.csv > /home/cecchgu/Downloads/LabeledPatchesSNR_clean.csv


#############################################################

##open files

GPP <- read.csv('/home/cecchgu/Downloads/LabeledPatchesgpp_clean.csv', stringsAsFactors=FALSE)

Albedo <- read.csv('/home/cecchgu/Downloads/LabeledPatchesalbedo_clean.csv', stringsAsFactors=FALSE)

SNR <- read.csv('/home/cecchgu/Downloads/LabeledPatchesSNR_clean.csv', stringsAsFactors=FALSE)

###### create new dataframe

##use sample_ID POINT_ID time (fist col-first part -> substr(GPP[1,1],1,10)) albedo, PsnNet, Albedo SNR

#explore a little bit
unique_ID_GPP <- unique(GPP$POINT_ID)
Time_step_GPP <- substr(GPP[,1],1,10)
length(unique(Time_step_GPP))
GPP$time <- Time_step_GPP

unique_ID_SNR <- unique(SNR$POINT_ID)

unique_ID_ALBEDO <- unique(Albedo$POINT_ID)
Time_step_ALBEDO <- substr(Albedo[,1],1,10)
length(unique(Time_step_ALBEDO))
Albedo$time <- Time_step_ALBEDO


##create a list to append dataframes
# datalist_albedo = list()
# datalist_GPP = list()
datalist = list()


for (i in 1:length(unique_ID_GPP)){
  
  GPP_filter <- GPP[GPP$POINT_ID == unique_ID_GPP[[i]],c(26,22,15,16)]
  SNR_filter <- SNR[SNR$POINT_ID == unique_ID_GPP[[i]],]
  Albedo_filter <- Albedo[Albedo$POINT_ID == unique_ID_GPP[[i]],c(26,21,16,2)]
  

  if (nrow(SNR_filter) >0 & nrow(Albedo_filter) >0){
  dat_1 <- cbind(GPP_filter,SNR_filter[,22])
  
  dat_2 <- dplyr::left_join(dat_1,Albedo_filter, by='time' )
  # dat_2 <- cbind(Albedo_filter[,],SNR_filter)
  
  dat_2 <- dat_2[,c(1:5,8)]
  
  datalist[[i]] <- dat_2 # add it to your list
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
write.csv(big_data, file = "FileGregAll.csv")


##### distance matrix
## TODO : better redine the extent to have a general idea of the distance in a second phase!

r <- raster(ncol=21,nrow=21, xmn=24.957, xmx=25.043, ymn=42.957, ymx=43.043, resolution = c(0.00416667,0.00416667))
xy <- c(25,43)
d1 <- distanceFromPoints(r, xy) 
crs(r) = '+init=epsg:4326'
d2 <- distanceFromPoints(r, xy) 

plot(d2)
