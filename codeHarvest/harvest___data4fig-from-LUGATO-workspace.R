# harvest data from Lugato's workspace...

getLugatoVars <- function(LugWorkSpace){

    # FOR DEBUGGING
  # LugWorkSpace <- 'dataFigures/NCC_WP1.RData'
  
  load(LugWorkSpace)

### (Horrible) code from Lugato to get the necessary vars...
################################################################################

base <- base[order(base$sample_ID),]				####remove 2 duplicated sample_ID
dupl <- which(duplicated(base$sample_ID))
base <- base[-c(dupl[1],dupl[1],dupl[2],dupl[2]),]

CA <- CA[order(CA$sample_ID),]
CA <- CA[-c(dupl[1],dupl[1],dupl[2],dupl[2]),]


base <- merge(aLCS[,7:13], base,  by.x="sample_ID", sort=F)	
CA <- merge(aLCS[,7:13], CA,  by.x="sample_ID", sort=F)


#########CO2e mitigation################################################

###CO2###
SOC_dif <- (CA[,94:179] - base[,94:179])*-3.67

dSOC <- apply(SOC_dif, 2, median, na.rm=T)
dSOC_1q <- apply(SOC_dif, 2, quantile, probs=c(0.25), na.rm=T)
dSOC_2q <- apply(SOC_dif, 2, quantile, probs=c(0.75), na.rm=T)


###N2O###
N2O_dif <- array(data=NA, dim=c(nrow(base),86))						###nrow!!
N2O_dif[,1] <- (CA[,8] - base[,8])*(44/28*265)/1000

for (j in 3:87){
  N2O_dif[,j-1] <- N2O_dif[,j-2] + (CA[,j+6] - base[,j+6])*(44/28*265)/1000
}

dN2O <- apply(N2O_dif, 2, median, na.rm=T)                              
dN2O_1q <- apply(N2O_dif, 2, quantile, probs=c(0.25), na.rm=T)
dN2O_2q <- apply(N2O_dif, 2, quantile, probs=c(0.75), na.rm=T)


###albedo###
alfa_dif <- array(data=NA, dim=c(nrow(base),86))						###nrow!!
alfa_dif[,1] <- 0

for (j in 3:87){
  alfa_dif[,j-1] <- alfa_dif[,j-2] + aLCS[,j+12]*-1
}

alfa <- apply(alfa_dif, 2, median, na.rm=T)                                
alfa_1q <- apply(alfa_dif, 2, quantile, probs=c(0.25), na.rm=T)
alfa_2q <- apply(alfa_dif, 2, quantile, probs=c(0.75), na.rm=T)

################################################################################

lab <- expression(Delta* "Soil fluxes (Mg CO"[2]*"eq Ha"^-1*")")   ##labels

scenarios <- rep(c("CO2_soil", "N2O_dir", "albedo" ), each=86)
year <- c(seq(2015,2100),seq(2015,2100),seq(2015,2100) )
dataCO2 <- cbind( dSOC, dSOC_1q, dSOC_2q); dataN2O<- cbind( dN2O, dN2O_1q, dN2O_2q) ; dataalfa<- cbind( alfa, alfa_1q, alfa_2q)
data <- rbind(dataCO2, dataN2O, dataalfa)


GHGbdg <- data.frame(scenarios, year , data)

################################################################################

GHGt30 <- abs(SOC_dif[,16]+ N2O_dif[,16])
GHGr30 <- ifelse(abs(alfa_dif[,16])/GHGt30>1, 1, abs(alfa_dif[,16])/GHGt30)
N2O_30 <- N2O_dif[,16]
CO2_30 <- SOC_dif[,16]

GHGt50 <- abs(SOC_dif[,36]+ N2O_dif[,36])
GHGr50 <- ifelse(abs(alfa_dif[,36])/GHGt50>1, 1, abs(alfa_dif[,36])/GHGt50)
N2O_50 <- N2O_dif[,36]
CO2_50 <- SOC_dif[,36]

GHGt70 <- abs(SOC_dif[,56]+ N2O_dif[,56])
GHGr70 <- ifelse(abs(alfa_dif[,56])/GHGt70>1, 1, abs(alfa_dif[,56])/GHGt70)
N2O_70 <- N2O_dif[,56]
CO2_70 <- SOC_dif[,56]

GHGt00 <- abs(SOC_dif[,86]+ N2O_dif[,86])
GHGr00 <- ifelse(abs(alfa_dif[,86])/GHGt00>1, 1, abs(alfa_dif[,86])/GHGt00)
N2O_00 <- N2O_dif[,86]
CO2_00 <- SOC_dif[,86]


GHGsen <- cbind(aLCS[,1:13], 
                GHGt30, GHGr30, 
                GHGt50, GHGr50, 
                GHGt70, GHGr70,
                GHGt00, GHGr00) 

################################################################################

# rename arable land area
arableLand <- Mha %>%
  transmute(Country = factor(NUT0, levels = NUT0),
            Area_Mha = Mha)


# prepare biogeochemical effects
bgc <- as.data.frame(N2O_dif + SOC_dif)
colnames(bgc) <- paste0('yr_',seq(2015,2100))
bgc$POINT_ID <- aLCS$POINT_ID 

# prepare biophysical effects
bph <- as.data.frame(alfa_dif)
colnames(bph) <- paste0('yr_',seq(2015,2100))
bph$POINT_ID <- aLCS$POINT_ID 


return(dat <- list("aLCS" = aLCS, "GHGbdg" = GHGbdg, "GHGsen" = GHGsen,
                   #"N2O_dif" = N2O_dif, "SOC_dif" = SOC_dif, "alfa_dif" = alfa_dif,
                   "bph" = bph, "bgc" = bgc,
                   "arableLand" = arableLand))
}


dat <- getLugatoVars('dataFigures/NCC_WP1.RData')
save(dat, file = 'dataFigures/data4scen1_normalCC-noSNOW.Rda') 

dat <- getLugatoVars('dataFigures/NCC_WP3.RData')
save(dat, file = 'dataFigures/data4scen3_brightCC-noSNOW.Rda') 

dat <- getLugatoVars('dataFigures/NCC_WP2.RData')
save(dat, file = 'dataFigures/data4scen2_normalCC-withSNOW.Rda') 

dat <- getLugatoVars('dataFigures/NCC_WP4.RData')
save(dat, file = 'dataFigures/data4scen4_mutantCC-withSNOW.Rda') 





# # harvest data from Lugato's workspace...
# 
# load('dataFigures/CC_normal_albedo.RData')
# 
# save("GHGbdg", "GHGsen", "aLCS", "N2O_dif", "SOC_dif", "alfa_dif", 
#      file = 'dataFigures/new_data4fig_normal.Rda') 
# 
# rm(list=ls())
# 
# load('dataFigures/CC_0.28_albedo_lowNPP.RData')
# 
# save("GHGbdg", "GHGsen", "aLCS", "N2O_dif", "SOC_dif", "alfa_dif", 
#      file = 'dataFigures/new_data4fig_mutant.Rda') 
