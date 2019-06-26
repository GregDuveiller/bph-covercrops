# harvest out some point data from the raw_Csv files extracted from GEE

require(readr)
require(dplyr)
require(here)

iYear <- 2016
rawCsv <- readr::read_csv(paste0('dataProcessing/step1_dataExtractionFromGEE/FileGregAllFlagsNDVI',iYear,'.csv'))

Lpt <- '28761622'
Lpt <- '34362088'


# parametrization
minSNR <- 25
minDist <- 7
ndays <- 150

no.pts <- data.frame(NULL)


# function to extract infor from a single point

# declare window size
ws = 21 

# subset data and sort out some column names
subDat <- rawCsv %>% 
  rename(SNR_vct = `SNR_filter[, 22]`, 
         NDV_vct = NDVI,
         ALB_vct = AlbedoMean, 
         ALB_qlty_vct = BRDF_Albedo_Band_Mandatory_Quality_shortwave,
         SNW_vct = Snow_BRDF_Albedo
  ) %>%
  filter(POINT_ID.x == Lpt) 

subDat$NDV_vct[is.na(subDat$NDV_vct)] <- paste(rep('NA',ws^2),collapse=',')
# subDat$NDV_qlty_vct[is.na(subDat$NDV_qlty_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$ALB_vct[is.na(subDat$ALB_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$ALB_qlty_vct[is.na(subDat$ALB_qlty_vct)] <- paste(rep('NA',ws^2),collapse=',')
subDat$SNW_vct[is.na(subDat$SNW_vct)] <- paste(rep('NA',ws^2),collapse=',')

# prepare dataframe to host exploded values from the window
dat <- data.frame(idpt=rep(1:ws^2,times=dim(subDat)[1]),
                  time=as.Date(rep(subDat$time,each=ws^2),format = '%Y_%m_%d'))

# explose the cells with multiple values of the window
dat$SNR = as.numeric(as.vector(unlist(strsplit(subDat$SNR_vct, ","))))
dat$NDV = as.numeric(as.vector(unlist(strsplit(subDat$NDV_vct, ","))))/255
dat$ALB = as.numeric(as.vector(unlist(strsplit(subDat$ALB_vct, ","))))*0.001
dat$ALB_qlty = as.integer(as.vector(unlist(strsplit(subDat$ALB_qlty_vct, ","))))
dat$SNW = as.integer(as.vector(unlist(strsplit(subDat$SNW_vct, ","))))

# calculate distances within the window
dd <- data.frame(idpt=1:ws^2,row=rep(1:ws,each=ws),col=rep(1:ws,times=ws))
dd$dist <- sqrt((dd$col-11)^2 + (dd$row-11)^2)

# further prepare the dataframe
dat <- dat %>%
  filter(SNR!=0)%>% # removes areas that have been masked in GEE
  left_join(dd,by = "idpt") # join it with a filter

# base filter
datf0 <- filter(dat, SNR > minSNR, SNW == 0, ALB > 0, dist < minDist)

# get all times 
timeVct <- unique(datf0$time)
# max time limits
TIME_lims <- c(min(timeVct), max(timeVct))
# get maxtime
dum1 <-datf0 %>% group_by(time) %>% summarise(meanNDV = mean(NDV, na.rm=T))
timeMax <- dum1$time[which(dum1$meanNDV == max(dum1$meanNDV))]
# get mintime
dum2 <- filter(dum1, time <= timeMax, time > timeMax-ndays)
timeMin <- dum2$time[which(dum2$meanNDV == min(dum2$meanNDV))]


save('dat','datf0','timeMax','timeMin','ndays','minDist','minSNR','iYear',
     file = paste0('dataFigures/dat4figLUCASpt/pt_',Lpt,'/dat_',Lpt,'.Rda'))

# fit <- lm(ALB ~ NDV, data = filter(datf0, time <= timeMax, time > timeMin))  
# 
# 
# 
# dfout <- data.frame(POINT_ID=Lpt,
#                     ref_year=iYear+1,
#                     NDVI.M.min = min(dum2$meanNDV),
#                     NDVI.M.max = max(dum1$meanNDV),
#                     b0=fit$coefficients[1],
#                     b1=fit$coefficients[2],
#                     n=length(fit$residuals),
#                     adj.r.sqr=summary(fit)$adj.r.squared,
#                     rmse=sqrt(mean((fit$residuals)^2)),
#                     mae=mean(abs(fit$residuals))) 
# 

