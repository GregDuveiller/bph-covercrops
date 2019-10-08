# harvest data from Lugato's workspace...

load('dataFigures/CC_normal_albedo.RData')

save("GHGbdg", "GHGsen", "aLCS", "N2O_dif", "SOC_dif", "alfa_dif", 
     file = 'dataFigures/new_data4fig_normal.Rda') 

rm(list=ls())

load('dataFigures/CC_0.28_albedo_lowNPP.RData')

save("GHGbdg", "GHGsen", "aLCS", "N2O_dif", "SOC_dif", "alfa_dif", 
     file = 'dataFigures/new_data4fig_mutant.Rda') 
