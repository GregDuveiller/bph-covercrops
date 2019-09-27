require(ggplot2)
require(here)


dat <- read.csv(file = 'dataFigures/alb_elabo3.1.PET.csv')

# 
# ggplot(dat %>%
#          filter(!is.na(BareSoil.Albedo))) +
#   geom_point(aes(x = PPT, y = PET, 
#                  colour = cut(BareSoil.Albedo, 
#                               breaks = c(0,0.05,seq(0.06,0.15,0.01),0.2,1)))) +
#   scale_color_viridis_d('')


fpath <- 'textFigures/'
dir.create(fpath, recursive = T, showWarnings = F)

custom_theme <- theme(legend.position = 'top',
                      legend.key.width = unit(0.8,'in'),
                      # panel.background = element_rect(fill = 'grey20'),
                      # panel.grid = element_line(color = 'grey30'),
                      plot.tag = element_text(face = "bold"))


pointSize = 1

g.clsp.TransSWin <- ggplot(dat) +
  geom_point(aes(x = PPT, y = PET, colour = SWin_Ta_Wm2), size = pointSize) +
   scale_colour_viridis_c("Ta * SWin",
                          limits = c(40,120), 
                          option = "magma",
                          oob = squish)+
  labs(tag = 'a') + 
  custom_theme +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


g.clsp.BareSoilA <- ggplot(dat) + 
  geom_point(aes(x = PPT, y = PET, colour = BareSoil.Albedo), size = pointSize)+
  scale_colour_viridis_c("Bare soil albedo",
                         limits = c(0.05,0.25), 
                         option = "viridis", 
                         oob = squish)+
  labs(tag = 'b') + 
  custom_theme +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))


g.clsp.AlbedoChg <- ggplot(dat) +
  geom_point(aes(x = PPT, y = PET, colour = a_dif), size = pointSize) +
  scale_colour_gradientn("Albedo change",
                         limits = c(-0.01,0.01), 
                         colors = brewer.pal(9,'PiYG'), 
                         oob = squish)+
  labs(tag = 'c') + 
  custom_theme +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))



g.clsp.AlbRadFor <- ggplot(dat) + 
  geom_point(aes(x = PPT, y = PET, colour = RFa_Wm2), size = pointSize)+
  scale_colour_gradientn("Albedo radiative forcing",
                         limits = c(-0.5,0.5), 
                         colors = brewer.pal(9,'RdBu'), 
                         oob = squish)+
  labs(tag = 'd') + 
  custom_theme +  
  guides(colour = guide_colourbar(title.position = 'top', title.hjust = 0.5))




fname <- 'Figure2'
figW <- 10; figH <- 11; fmt <- 'png'
fullfname <- paste0(fpath, fname, '.', fmt)
if(fmt=='png'){png(fullfname, width = figW, height = figH, units = "in", res= 150)}
if(fmt=='pdf'){pdf(fullfname, width = figW, height = figH)}
print(g.clsp.TransSWin, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.5, just=c(0,0)))
print(g.clsp.BareSoilA, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.5, just=c(0,0)))
print(g.clsp.AlbedoChg, vp = viewport(width = 0.5, height = 0.5, x = 0.0, y = 0.0, just=c(0,0)))
print(g.clsp.AlbRadFor, vp = viewport(width = 0.5, height = 0.5, x = 0.5, y = 0.0, just=c(0,0)))

dev.off()
