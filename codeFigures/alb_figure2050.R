library(foreign)
library(data.table)
library(RColorBrewer)
library(rasterVis)
library(colorspace)
library(lattice)
library(ggplot2)
library(foreign)
library(scales)
library(Rmisc)
library(maptools)
library(ggmap)
library(latticeExtra)
library(reshape2)

#LCS_AR30<-read.dbf("E:\\model\\daycent\\_lcs_AR_GR30cm_v2.dbf")		####merge_NUT0
#LCS_AR30<-subset(LCS_AR30, LCS_AR30$LU=="AR")	

#states.shp <- readShapeSpatial("E:\\model\\daycent\\sim\\lucas\\results\\BMP_PRJ\\shape\\MS.shp")
#states.shp.f <- fortify(states.shp)

#Mha<-read.table("Mha.txt", header=T)

############################################################################################################
############################################################################################################
#aLCS1<-read.csv("alb_elabo3.2.csv"); scen1<-"CC_RY"				####baseline vs CC actual
#aLCS2<-read.csv("alb_elabo3.2.SNOW.csv"); scen2<-"CC_RY"			####baseline vs CC actual with SNOW
#aLCS3<-read.csv("alb_elabo3.2.a28.csv"); scen3<-"CC_RY_lowA"			####baseline vs CC_high_albedo_0.8NPP				
#aLCS4<-read.csv("alb_elabo3.2.SNOW.a28.csv"); scen4<-"CC_RY_lowA"			####baseline vs CC_high_albedo_0.8NPP with SNOW	

#aLCS<-aLCS4; scen<-scen4								########select scenario!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#aLCS<-aLCS[order(aLCS$POINT_ID),]
# dupl<-which(duplicated(aLCS$POINT_ID))
# aLCS<-aLCS[-dupl,]


#aLCS<-merge(aLCS, LCS_AR30[,c(2,20)],  by.x="POINT_ID", sort=F)



fig1<- ggplot() + geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), color = "grey20", fill="grey50", size = 0.25) +
 geom_point(aes(x= GPS_LONG, y=GPS_LAT, color = SWin_Ta_Wm2), data=aLCS , size=0.8, alpha = 0.5) +   
 scale_colour_viridis_c("Ta * SWin",
                         limits = c(40,120), 
                         option = "magma",
                         oob = squish)+
theme(legend.position="top", legend.key.width = unit(1,"cm"))+
annotate("text", x=-1.5, y=69, label= paste("mean(sd)= ",round(mean(aLCS$SWin_Ta_Wm2, na.rm=TRUE),2), "±", round(sd(aLCS$SWin_Ta_Wm2, na.rm=TRUE),2), sep="") , col="black" , size=3)


fig1.1<- ggplot() + geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), color = "grey20", fill="grey50", size = 0.25) +
 geom_point(aes(x= GPS_LONG, y=GPS_LAT, color = BareSoil.Albedo), data=aLCS , size=0.8, alpha = 0.5) +   
 scale_colour_viridis_c("Bare soil albedo",
                         limits = c(0.05,0.25), 
                         option = "viridis", 
                         oob = squish)+
theme(legend.position="top", legend.key.width = unit(1,"cm"))+
annotate("text", x=-1.5, y=69, label= paste("mean(sd)= ",round(mean(aLCS$BareSoil.Albedo, na.rm=TRUE),2), "±", round(sd(aLCS$BareSoil.Albedo, na.rm=TRUE),2), sep="") , col="black" , size=3)



fig1.2<- ggplot() + geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), color = "grey20", fill="grey50", size = 0.25) + 
 geom_point(aes(x= GPS_LONG, y=GPS_LAT, color =aLCS$a_dif), data=aLCS , size=0.8, alpha = 0.5) +   
  scale_colour_gradientn("Albedo change",
                         limits = c(-0.01,0.01), 
                         colors = rev(brewer.pal(9,'RdBu')), 
                         oob = squish)+
theme(legend.position="top", legend.key.width = unit(1,"cm"))+
annotate("text", x=-1.5, y=69, label= paste("mean(sd)= ",round(mean(aLCS$a_dif, na.rm=TRUE),4), "±", round(sd(aLCS$a_dif, na.rm=TRUE),4), sep="") , col="black" , size=3)



fig1.3<- ggplot() + geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), color = "grey20", fill="grey50", size = 0.25) + 
 geom_point(aes(x= GPS_LONG, y=GPS_LAT, color = aLCS$RFa_Wm2), data=aLCS , size=0.8, alpha = 0.5) +   
 scale_colour_gradientn("Albedo radiative forcing",
                         limits = c(-0.5,0.5), 
                         colors = rev(brewer.pal(9,'RdBu')), 
                         oob = squish)+
theme(legend.position="top", legend.key.width = unit(1,"cm"))+
annotate("text", x=-1.5, y=69, label= paste("mean(sd)= ",round(mean(aLCS$RFa_Wm2, na.rm=TRUE),2), "±", round(sd(aLCS$RFa_Wm2, na.rm=TRUE),2), sep="") , col="black" , size=3)



multiplot(fig1, fig1.2, fig1.1, fig1.3, cols = 2)


#####################################################################################################################################################
#####################################################################################################################################################

#########################################################################################################

#########################################################################################################

#base<-read.dbf('E:\\model\\daycent\\sim\\lucas\\results\\BMP_PRJ\\BA_30cm\\albedo\\baseline\\N2O_SOC_prj.dbf')
#  CA<-read.dbf(paste('E:\\model\\daycent\\sim\\lucas\\results\\BMP_PRJ\\BA_30cm\\albedo\\', scen, '\\N2O_SOC_prj.dbf', sep=""))


base<-base[order(base$sample_ID),]				####remove 2 duplicated sample_ID
 dupl<-which(duplicated(base$sample_ID))
base<-base[-c(dupl[1],dupl[1],dupl[2],dupl[2]),]

CA<-CA[order(CA$sample_ID),]
CA<-CA[-c(dupl[1],dupl[1],dupl[2],dupl[2]),]


base<-merge(aLCS[,7:13], base,  by.x="sample_ID", sort=F)	
 CA<-merge(aLCS[,7:13], CA,  by.x="sample_ID", sort=F)


#########CO2e mitigation################################################

###CO2###
SOC_dif<- (CA[,94:179] - base[,94:179])*-3.67

 dSOC<-apply(SOC_dif, 2, median, na.rm=T)
 dSOC_1q<-apply(SOC_dif, 2, quantile, probs=c(0.25), na.rm=T)
 dSOC_2q<-apply(SOC_dif, 2, quantile, probs=c(0.75), na.rm=T)


###N2O###
N2O_dif<-array(data=NA, dim=c(nrow(base),86))						###nrow!!
N2O_dif[,1]<-(CA[,8] - base[,8])*(44/28*265)/1000
 
for (j in 3:87){
 N2O_dif[,j-1]<- N2O_dif[,j-2] + (CA[,j+6] - base[,j+6])*(44/28*265)/1000
}

dN2O<-apply(N2O_dif, 2, median, na.rm=T)                              
dN2O_1q<-apply(N2O_dif, 2, quantile, probs=c(0.25), na.rm=T)
dN2O_2q<-apply(N2O_dif, 2, quantile, probs=c(0.75), na.rm=T)


###albedo###
alfa_dif<-array(data=NA, dim=c(nrow(base),86))						###nrow!!
alfa_dif[,1]<-0
 
for (j in 3:87){
 alfa_dif[,j-1]<- alfa_dif[,j-2] + aLCS[,j+12]*-1
}


###################################################################################################################
####correction for RF with IRF														#	
###################################################################################################################
RFa_wm2_y<-(aLCS[,15:99]*(0.908*0.48)*10)*(10000/5.1E+14)  	#W/m2 *A/Earth - riconversion old values to W/m2	#
 RFa_wm2_y<-t(apply(RFa_wm2_y, 1, cumsum))			#cumulative wm2 *A/Earth					#
																			#
																			#
IRS<-matrix(NA, nrow = nrow(aLCS), ncol = 85)												#
for (i in 0:84){																	#
IRS[,i+1]<-0.217+0.259*exp(-i/172.9)+0.338*exp(-i/18.51)+0.186*exp(-i*1.186)							#
}																			#
																			#
IRS<-t(apply(IRS, 1, cumsum))															#
																			#
RF_IRS<- RFa_wm2_y/(1.76E-15*IRS)/1000				#cumulative RF Mg/Ha/y						#
																			#
###################################################################################################################
alfa_dif[,2:86]<- -1*RF_IRS															#		
###################################################################################################################

alfa<-apply(alfa_dif, 2, median, na.rm=T)                                
alfa_1q<-apply(alfa_dif, 2, quantile, probs=c(0.25), na.rm=T)
alfa_2q<-apply(alfa_dif, 2, quantile, probs=c(0.75), na.rm=T)



#####################################################################################################################################################figure 2a
#####################################################################################################################################################

lab<- expression(Delta* "Soil fluxes (Mg CO"[2]*"eq Ha"^-1*")")   ##labels

scenarios=rep(c("CO2_soil", "N2O_dir", "albedo" ), each=86)
year<-c(seq(2015,2100),seq(2015,2100),seq(2015,2100) )
dataCO2<- cbind( dSOC, dSOC_1q, dSOC_2q); dataN2O<- cbind( dN2O, dN2O_1q, dN2O_2q) ; dataalfa<- cbind( alfa, alfa_1q, alfa_2q)
data<-rbind(dataCO2, dataN2O, dataalfa)


GHGbdg<-data.frame(scenarios, year , data)

fig2<-ggplot(GHGbdg, aes(year, dSOC ,  colour = scenarios, fill=scenarios)) + 
  geom_line(size=1) + 
  geom_ribbon(aes(ymin=dSOC_2q, ymax=dSOC_1q),  linetype=0, alpha=0.2) +
 scale_y_continuous(limits=c(-30, 20)) +
  labs(x = "year" , y =  lab) +
  geom_hline(yintercept=0, linetype="solid", alpha=0.4) +
 geom_vline(xintercept=2030, linetype="dashed", alpha=0.2) +
 geom_vline(xintercept=2100, linetype="dashed", alpha=0.2) +
  theme(legend.position = c(.25, 0.83)) +
theme_bw()+ 
theme(legend.background = element_rect(fill="white")) + 
theme(legend.background = element_rect(colour = "black")) +
theme(legend.key = element_rect(colour = "white")) + 
theme(legend.position = "none")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#####################################################################################################################################################
#####################################################################################################################################################

GHGt30<-abs(SOC_dif[,36]+ N2O_dif[,36])
GHGr30<-ifelse(abs(alfa_dif[,36])/GHGt30>1, 1, abs(alfa_dif[,36])/GHGt30)
N2O_30<- N2O_dif[,36]
CO2_30<- SOC_dif[,36]

GHGt00<-abs(SOC_dif[,86]+ N2O_dif[,86])
GHGr00<-ifelse(abs(alfa_dif[,86])/GHGt00>1, 1, abs(alfa_dif[,86])/GHGt00)
N2O_00<- N2O_dif[,86]
CO2_00<- SOC_dif[,86]


GHGsen<-cbind(aLCS[,1:13], GHGt30, GHGr30, GHGt00, GHGr00) 

#GHGsen<-subset(GHGsen, RFa_Wm2>0)####!!!!


#####################################################################################################################################################figure 2c,d
#####################################################################################################################################################
fig2.1<-ggplot() + geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), color = "grey20", fill="grey50", size = 0.25) + 
 geom_point(data=GHGsen, aes(x=GPS_LONG,  y=GPS_LAT, colour = GHGr30), alpha=0.4, size=0.8)+
scale_colour_viridis_c("aFR",
                         limits = c(0, 1),
                         option = 'plasma', 
                         oob = squish) +
theme_bw()+ 
theme(legend.background = element_rect(fill="white")) + 
theme(legend.background = element_rect(colour = "black")) +
theme(legend.key = element_rect(colour = "white")) + 
 theme(legend.position = "none") 

fig2.2<-ggplot() + geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), color = "grey20", fill="grey50", size = 0.25) + 
 geom_point(data=GHGsen, aes(x=GPS_LONG,  y=GPS_LAT, colour = GHGr00), alpha=0.4, size=0.8)+
scale_colour_viridis_c("",
                         limits = c(0, 1),
                         option = 'plasma', 
                         oob = squish) +
theme_bw()+ 
theme(legend.background = element_rect(fill="transparent")) + 
theme(legend.background = element_rect(colour = "transparent")) +
theme(legend.key = element_rect(colour = "white")) + 
 #theme(legend.position = "right") 
 theme(legend.position = c(.15, 0.852))

windows() 
multiplot(fig2, fig2.1, fig2.2, cols = 3)


#####################################################################################################################################################
#####################################################################################################################################################


ylab<- "Tg CO2e"
xlab<- "Country"


#################2030
MCO2e_NUT0<-cbind(aLCS[,c(1,103)], "CO2"=SOC_dif[,36], "N2O"=N2O_dif[,36], "a"=alfa_dif[,36])
 MCO2_NUT0<-aggregate(MCO2e_NUT0, by = list("NUT0"=MCO2e_NUT0$NUT0), FUN = "mean", na.rm=T)
 MCO2_NUT0[3]<-NULL

MCO2_NUT0<-merge(MCO2_NUT0, Mha,  by.x="NUT0", sort=F)

MCO2_NUT0<-MCO2_NUT0[complete.cases(MCO2_NUT0), ]
MCO2_NUT0<- cbind(MCO2_NUT0, MCO2_NUT0[,3:5]* MCO2_NUT0[,6])
 MCO2_NUT0[3:5]<-NULL
  MCO2_NUT0<- cbind(MCO2_NUT0, "TOT"=MCO2_NUT0[,4]+MCO2_NUT0[,5]+MCO2_NUT0[,6])

MCO2_NUT0<- melt(MCO2_NUT0, id=c("NUT0", "POINT_ID", "Mha", "TOT"))

C30<-by( MCO2_NUT0$value, MCO2_NUT0$variable, sum, na.rm=T)

MS30<-ggplot(MCO2_NUT0, aes(x = NUT0, y =  value, fill=variable)) + geom_bar(stat = "identity", width = 1) + 
geom_abline(intercept = 0, slope = 0, col='blue')+
geom_point(aes(x = NUT0, y=TOT))+
#geom_text(aes(label=round(TOT,0)), size=3, check_overlap=T)+
labs(x = xlab , y = ylab) + 
scale_y_continuous(limits = c(-500, 100)) +
coord_polar()  +
annotate(geom = 'text', x = 0.5, y = -400, label = round(C30[1]+C30[2]+C30[3],0))






#################2100

MCO2e_NUT0<-cbind(aLCS[,c(1,103)], "CO2"=SOC_dif[,86], "N2O"=N2O_dif[,86], "a"=alfa_dif[,86])
 MCO2_NUT0<-aggregate(MCO2e_NUT0, by = list("NUT0"=MCO2e_NUT0$NUT0), FUN = "mean", na.rm=T)
 MCO2_NUT0[3]<-NULL

MCO2_NUT0<-merge(MCO2_NUT0, Mha,  by.x="NUT0", sort=F)

MCO2_NUT0<-MCO2_NUT0[complete.cases(MCO2_NUT0), ]
MCO2_NUT0<- cbind(MCO2_NUT0, MCO2_NUT0[,3:5]* MCO2_NUT0[,6])
 MCO2_NUT0[3:5]<-NULL
  MCO2_NUT0<- cbind(MCO2_NUT0, "TOT"=MCO2_NUT0[,4]+MCO2_NUT0[,5]+MCO2_NUT0[,6])

MCO2_NUT0<- melt(MCO2_NUT0, id=c("NUT0", "POINT_ID", "Mha", "TOT"))

C00<-by( MCO2_NUT0$value, MCO2_NUT0$variable, sum, na.rm=T)

MS00<-ggplot(MCO2_NUT0, aes(x = NUT0, y =  value, fill=variable)) + geom_bar(stat = "identity", width = 1) + 
geom_abline(intercept = 0, slope = 0, col='blue')+
geom_point(aes(x = NUT0, y=TOT))+
#geom_text(aes(label=round(TOT,0)), size=3, check_overlap=T)+
labs(x = xlab , y = ylab) + 
scale_y_continuous(limits = c(-500, 100)) +
coord_polar() +
annotate(geom = 'text', x = 0.5, y = -400, label = round(C00[1]+C00[2]+C00[3],0))




windows() 
multiplot(MS30, MS00, cols = 2)

#####################################################################################################################################################
#####################################################################################################################################################
apply(SOC_dif, 2,quantile, probs=c(0.25), na.rm=T)[36]/36 			#intro
apply(SOC_dif, 2,quantile, probs=c(0.75), na.rm=T)[36]/36 			#
apply(alfa_dif, 2, mean, na.rm=T)[36]/36   					#

apply(SOC_dif, 2, mean, na.rm=T)[36]/36 						#1

mean(aLCS$BareSoil.Albedo, na.rm=T); sd(aLCS$BareSoil.Albedo, na.rm=T)	#3

mean(aLCS$a_dif, na.rm=T); sd(aLCS$a_dif, na.rm=T)				#5

apply(SOC_dif, 2,quantile, probs=c(0.25), na.rm=T)[36]/36 			#7
apply(SOC_dif, 2,quantile, probs=c(0.75), na.rm=T)[36]/36 			#8

apply(SOC_dif, 2, mean, na.rm=T)[c(36,86)] + apply(N2O_dif, 2, mean, na.rm=T)[c(36,86)]	#9

C30[1]+C30[2] 	+C30[3]								#10 +C30[3]
C00[1]+C00[2] 	+C00[3]								#11 +C00[3]

apply(alfa_dif, 2,quantile, probs=c(0.25), na.rm=T)[86]/86 			#12
apply(alfa_dif, 2,quantile, probs=c(0.75), na.rm=T)[86]/86 			#13



windows() 
p <- ggplot(aLCS, aes(BareSoil.Albedo, MAX_a)) + geom_point(alpha=0.2, size=1, col="blue") + theme_classic()+
scale_y_continuous(limits=c(0, 0.3))+
scale_x_continuous(limits=c(0, 0.3))+
geom_abline(intercept = 0, slope = 1)+
xlab("bare soil albedo")+ ylab("maximun albedo")
ggExtra::ggMarginal(p, type = "histogram")




fig4<- ggplot() + geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), color = "grey20", fill="grey50", size = 0.25) +
 geom_point(aes(x= GPS_LONG, y=GPS_LAT, color = snowDJF), data=aLCS , size=1, alpha = 0.5) +   
 scale_colour_viridis_c("SWE",
                         limits = c(0,5), 
                         option = "viridis", 
                         oob = squish)+
theme(legend.position="top", legend.key.width = unit(1,"cm"))



windows() 
multiplot(fig1.2, MS30 ,fig1.3 ,  MS00, cols = 2)



