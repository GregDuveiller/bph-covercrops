
################################################################################
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


#####################################################################################################################################################figure 1a,b,c,d
#####################################################################################################################################################

states.shp <- readShapeSpatial("E:\\model\\daycent\\sim\\lucas\\results\\BMP_PRJ\\shape\\MS.shp")
states.shp.f <- fortify(states.shp)


fig1 <- ggplot() + 
  geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), 
               color = "grey20", fill = "grey90", size = 0.25) +
  geom_point(aes(x= GPS_LONG, y=GPS_LAT, color = SWin_Ta_Wm2), 
             data = aLCS , size=0.8, alpha = 0.5) +   
  scale_colour_gradient2(limits=c(40, 120), 
                        low="blue", mid= "green", high="red", midpoint=80,  na.value = "grey80", name="(W/m2)") +
  ggtitle("Ta * SWin") +
  annotate("text", x=-1.5, y=69, label= paste("mean(sd)= ",round(mean(aLCS$SWin_Ta_Wm2, na.rm=TRUE),2), "?", round(sd(aLCS$SWin_Ta_Wm2, na.rm=TRUE),2), sep="") , col="black" , size=3)


fig1.1<- ggplot() + 
  geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), 
               color = "grey20", fill="grey90", size = 0.25) +
 geom_point(aes(x= GPS_LONG, y=GPS_LAT, color = BareSoil.Albedo), 
            data=aLCS , size=0.8, alpha = 0.5) +   
 scale_colour_distiller(limits=c(0.03, 0.33), breaks=c(0.03, 0.1, 0.17, 0.24, 0.32), type = "div", palette ="Spectral", name="") +
 ggtitle("bare soil albedo") +
annotate("text", x=-1.5, y=69, label= paste("mean(sd)= ",round(mean(aLCS$BareSoil.Albedo, na.rm=TRUE),2), "?", round(sd(aLCS$BareSoil.Albedo, na.rm=TRUE),2), sep="") , col="black" , size=3)


alb=cut(aLCS$a_dif, c(-0.03, -0.0015, 0, 0.005,  0.01, 0.04))
fig1.2 <- ggplot() + 
  geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), 
               color = "grey20", fill="grey90", size = 0.25) + 
  geom_point(aes(x= GPS_LONG, y=GPS_LAT, color = alb),
             data=aLCS , size=0.8, alpha = 0.5) +   
  scale_colour_manual(values=c("#8B0A50","#FF0000", "#bae4b3","#66CD00","#006400"), name="", labels=c("<-0.03", "-0.0015 - 0", "0 - 0.005", "1 - 0.01", ">0.01")) +
  ggtitle("albedo change") +
  guides(colour = guide_legend(override.aes = list(size=1.5))) +
  annotate("text", x=-1.5, y=69, label= paste("mean(sd)= ",round(mean(aLCS$a_dif, na.rm=TRUE),4), "?", round(sd(aLCS$a_dif, na.rm=TRUE),4), sep="") , col="black" , size=3)



albWm=cut(aLCS$RFa_Wm2, c(-1.5, -0.25, 0, 0.25,  0.5, 2.5))
fig1.3<- ggplot() + 
  geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group),
               color = "grey20", fill="grey90", size = 0.25) + 
  geom_point(aes(x= GPS_LONG, y=GPS_LAT, color = albWm), 
            data=aLCS , size=0.8, alpha = 0.5) +   
  scale_colour_manual(values=c("#8B0A50","#FF0000", "#bae4b3","#66CD00","#006400"), name="(W/m2)", labels=c("<-1.5", "-0.25 - 0", "0 - 0.25", "0 - 0.5", ">0.5")) +
  ggtitle("albedo radative forcing") +
  guides(colour = guide_legend(override.aes = list(size=1.5)) )+
  annotate("text", x=-1.5, y=69, label= paste("mean(sd)= ",round(mean(aLCS$RFa_Wm2, na.rm=TRUE),2), "?", round(sd(aLCS$RFa_Wm2, na.rm=TRUE),2), sep="") , col="black" , size=3)



multiplot(fig1, fig1.2, fig1.1, fig1.3, cols = 2)


#####################################################################################################################################################
#####################################################################################################################################################

#########################################################################################################
scen<-"BA_CC_30cm_RY"			##select the scenario and the dummy (0 or 1)


#########################################################################################################

base<-read.dbf('E:\\model\\daycent\\sim\\lucas\\results\\BMP_PRJ\\BA_30cm\\BA_baseline_30cm\\N2O_SOC_prj.dbf')
  CA<-read.dbf(paste('E:\\model\\daycent\\sim\\lucas\\results\\BMP_PRJ\\BA_30cm\\', scen, '\\N2O_SOC_prj.dbf', sep=""))


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
 N2O_dif[,j-1]<- N2O_dif[,j-2] + (CA[,j] - base[,j])*(44/28*265)/1000
}

dN2O<-apply(N2O_dif, 2, median, na.rm=T)                              
dN2O_1q<-apply(N2O_dif, 2, quantile, probs=c(0.25), na.rm=T)
dN2O_2q<-apply(N2O_dif, 2, quantile, probs=c(0.75), na.rm=T)


###albedo###
alfa_dif<-array(data=NA, dim=c(nrow(base),86))						###nrow!!
alfa_dif[,1]<-0
 
for (j in 3:87){
 alfa_dif[,j-1]<- alfa_dif[,j-2] + aLCS[,j+(11)]*-1
}

alfa<-apply(alfa_dif, 2, median, na.rm=T)                                
alfa_1q<-apply(alfa_dif, 2, quantile, probs=c(0.25), na.rm=T)
alfa_2q<-apply(alfa_dif, 2, quantile, probs=c(0.75), na.rm=T)



#####################################################################################################################################################figure 2a
#####################################################################################################################################################

lab <- expression(Delta* "Soil fluxes (Mg CO"[2]*"eq Ha"^-1*")")   ##labels

scenarios=rep(c("CO2_soil", "N2O_dir", "albedo" ), each=86)
year<-c(seq(2015,2100),seq(2015,2100),seq(2015,2100) )
dataCO2<- cbind( dSOC, dSOC_1q, dSOC_2q); dataN2O<- cbind( dN2O, dN2O_1q, dN2O_2q) ; dataalfa<- cbind( alfa, alfa_1q, alfa_2q)
data<-rbind(dataCO2, dataN2O, dataalfa)


GHGbdg<-data.frame(scenarios, year , data)

fig2 <- ggplot(GHGbdg, aes(year, dSOC ,  colour = scenarios, fill = scenarios)) + 
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

GHGt30<-abs(SOC_dif[,16]+ N2O_dif[,16])
GHGr30<-ifelse(abs(alfa_dif[,16])/GHGt30>1, 1, abs(alfa_dif[,16])/GHGt30)
N2O_30<- N2O_dif[,16]
CO2_30<- SOC_dif[,16]

GHGt00<-abs(SOC_dif[,86]+ N2O_dif[,86])
GHGr00<-ifelse(abs(alfa_dif[,86])/GHGt00>1, 1, abs(alfa_dif[,86])/GHGt00)
N2O_00<- N2O_dif[,86]
CO2_00<- SOC_dif[,86]


GHGsen<-cbind(aLCS[,1:13], GHGt30, GHGr30, GHGt00, GHGr00) 

GHGsen<-subset(GHGsen, RFa_Wm2>0)####!!!!


#####################################################################################################################################################figure 2c,d
#####################################################################################################################################################
fig2.1<-ggplot() + geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), color = "grey20", fill="grey90", size = 0.25) + 
  geom_point(data=GHGsen, aes(x=GPS_LONG,  y=GPS_LAT, colour = GHGr30), alpha=0.4, size=0.8)+
  scale_colour_gradient2(limits=c(0, 1), low="blue", mid= "green", high="yellow", midpoint=0.5,  na.value = "grey80", name="") +
  theme_bw()+ 
  theme(legend.background = element_rect(fill="white")) + 
  theme(legend.background = element_rect(colour = "black")) +
  theme(legend.key = element_rect(colour = "white")) + 
  theme(legend.position = "none") 

fig2.2<-ggplot() + geom_polygon(data = states.shp.f, aes(x = long, y = lat, group = group), color = "grey20", fill="grey90", size = 0.25) + 
  geom_point(data=GHGsen, aes(x=GPS_LONG,  y=GPS_LAT, colour = GHGr00), alpha=0.4, size=0.8)+
  scale_colour_gradient2(limits=c(0, 1), low="blue", mid= "green", high="yellow", midpoint=0.5,  na.value = "grey80", name="") +
  theme_bw()+ 
  theme(legend.background = element_rect(fill="transparent")) + 
  theme(legend.background = element_rect(colour = "transparent")) +
  theme(legend.key = element_rect(colour = "white")) + 
  #theme(legend.position = "right") 
  theme(legend.position = c(.15, 0.852))

multiplot(fig2, fig2.1, fig2.2, cols = 3)

#####################################################################################################################################################
#####################################################################################################################################################












