#load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedFactors.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/complete_cases.rda")
#load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/tshirt-wave.rda")
library(tidyverse); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
library(viridis)
library(plotly)
#-----------------------------------------


#----------------------------------------------
#Ignore this part
dfT <- dfDataSel
dfT$Veg <- as.factor(dfT$Veg)
dfT$ID <- as.factor(dfT$ID)
dfT$MediaType <- as.factor(dfT$MediaType)
dfT$TrainGroup <- as.factor(dfT$TrainGroup)

save(dfT, file = "Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedFactors.rda")

#----------------------------
#start here
#load the cleanedFactors.rda above

#set color here.
colorsScale <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

#-----------------------------
#Using scatter3D:
#requires library("plot3D")
# scatter3D(data = dfT, x = dfT$Nitrate, y= dfT$COD, z = dfT$Selenium, phi = 0, 
#           bty = "g",  type = "h", ticktype = "detailed", pch = 19, cex = 0.5)
#NOTE: cannot rotate! Not ideal. Thus using plotly instead.

#----------------------------

#Temp vs Nit on Veg
fig <- plot_ly(dfT, x = ~Nitrate, y = ~Temp..Celsius, 
               z = ~Selenium, color = ~Veg, colors = colorsScale)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                   yaxis = list(title = 'Temp Celsius'),
                                   zaxis = list(title = 'Selenium mg/L')))
fig

#---------

#Using color scaling
#NOTE: doesn't work :(
figS <- plot_ly(dfT, x = ~Nitrate, y = ~Temp..Celsius, 
               z = ~Selenium, color = ~Selenium) 
figS <- figS %>% add_markers()
figS <- figS %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                   yaxis = list(title = 'Temp. Celsius'),
                                   zaxis = list(title = 'Selenium mg/L'))
                      )
figS

#----------

#using log and size scaling...
fig2 <- plot_ly(dfT, x = ~Temp..Celsius, y = ~Nitrate, z = ~Selenium, 
               color = ~Veg, size = ~Selenium, colors = colorsScale,
               marker = list(symbol = 'circle', sizemode = 'diameter'), 
               sizes = c(5, 100))
fig2 <- fig2 %>% layout(title = 'Selenium vs VegType',
                       scene = list(xaxis = list(title = 'Temp Celsius',
                                                 gridcolor = 'rgb(255, 255, 255)',
                                                 zerolinewidth = 1,
                                                 ticklen = 5,
                                                 gridwidth = 2),
                                    yaxis = list(title = 'Nitrate mg/L',
                                                 gridcolor = 'rgb(255, 255, 255)',
                                                 type = 'log',
                                                 zerolinewidth = 1,
                                                 ticklen = 5,
                                                 gridwith = 2),
                                    zaxis = list(title = 'Selenium',
                                                 gridcolor = 'rgb(255, 255, 255)',
                                                 type = 'log',
                                                 zerolinewidth = 1,
                                                 ticklen = 5,
                                                 gridwith = 2)),
                       paper_bgcolor = 'rgb(243, 243, 243)',
                       plot_bgcolor = 'rgb(243, 243, 243)')
fig2

#Temp vs Nit on Media
figM <- plot_ly(dfT, x = ~Nitrate, y = ~Temp..Celsius, 
               z = ~Selenium, color = ~MediaType, colors = colorsScale)
figM <- figM %>% add_markers()
figM <- figM %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                   yaxis = list(title = 'Temp Celsius'),
                                   zaxis = list(title = 'Selenium mg/L')))
figM

#------------------
#Nit vs COD on Veg
figNCV <- plot_ly(dfT, x = ~Nitrate, y = ~COD, 
                z = ~Selenium, color = ~Veg, colors = colorsScale)
figNCV <- figNCV %>% add_markers()
figNCV <- figNCV %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                     yaxis = list(title = 'COD'),
                                     zaxis = list(title = 'Selenium mg/L')))
figNCV

#---

#Nit vs COD on Media
figNCM <- plot_ly(dfT, x = ~Nitrate, y = ~COD, 
                 z = ~Selenium, color = ~MediaType, colors = colorsScale)
figNCM <- figNCM %>% add_markers()
figNCM <- figNCM %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                       yaxis = list(title = 'COD'),
                                       zaxis = list(title = 'Selenium mg/L')))
figNCM

#----------------

#Temp vs DO on Veg
figTDOV <- plot_ly(dfT, x = ~Temp..Celsius, y = ~DO.mg.L, 
                   z = ~Selenium, color = ~Veg, colors = colorsScale)
figTDOV <- figTDOV %>% add_markers()
figTDOV <- figTDOV %>% layout(scene = list(xaxis = list(title = 'Temp. Celsius'),
                                           yaxis = list(title = 'DO.mg.L'),
                                           zaxis = list(title = 'Selenium mg/L')))
figTDOV

#---

#Temp vs COD on Veg
figTCV <- plot_ly(dfT, x = ~Temp..Celsius, y = ~COD, 
                  z = ~Selenium, color = ~Veg, colors = colorsScale)
figTCV <- figTCV %>% add_markers()
figTCV <- figTCV %>% layout(scene = list(xaxis = list(title = 'Temp. Celsius'),
                                         yaxis = list(title = 'COD mg/L'),
                                         zaxis = list(title = 'Selenium mg/L')))
figTCV

#---

#COD vs DO

figCDOV <- plot_ly(dfD, x = ~COD, y = ~DO.mg.L, 
                   z = ~Selenium, color = ~Veg, colors = colorsScale)
figCDOV <- figCDOV %>% add_markers()
figCDOV <- figCDOV %>% layout(scene = list(xaxis = list(title = 'COD'),
                                           yaxis = list(title = 'DO mg/L'),
                                           zaxis = list(title = 'Selenium mg/L')))
figCDOV

#------------------------------
#--- using dfDiff dataset ---
#------------------------------

glimpse(dfDiff)
dfD <- dfDiff
dfD$Veg <- as.factor(dfD$Veg)
dfD$diff_ID <- as.factor(dfD$diff_ID)
dfD$MediaType <- as.factor(dfD$MediaType)
dfD$TrainGroup <- as.factor(dfD$TrainGroup)
glimpse(dfD)

save(dfT, dfD, 
     file = "Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedFactors.rda")

#---------------
#Diff Nit and Temp on Veg
figNTVD <- plot_ly(dfD, x = ~Nitrate, y = ~Temp..Celsius, 
                 z = ~diff_Selenium, color = ~Veg, colors = colorsScale)
figNTVD <- figNTVD %>% add_markers()
figNTVD <- figNTVD %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                     yaxis = list(title = 'Temp Celsius'),
                                     zaxis = list(title = 'Difference Selenium mg/L')))
figNTVD

#high T and low Nit for more selenium removal
#Veg Type B is best, Veg Type A is okay, Type C is worst

#---
#Diff Nit and Temp on Media
figNTMD <- plot_ly(dfD, x = ~Nitrate, y = ~Temp..Celsius, 
                 z = ~diff_Selenium, color = ~MediaType, colors = colorsScale)
figNTMD <- figNTMD %>% add_markers()
figNTMD <- figNTMD %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                         yaxis = list(title = 'Temp Celsius'),
                                         zaxis = list(title = 'Difference Selenium mg/L')))
figNTMD
#High T and low Nit favors Selenium removal.
#EIther temp or Nit alone isn't enough,except for PM as it just scatters.
#GW is good, MM and PM is about the same. Soil is very stable.

#---

#Diff Temp vs COD on Veg
figTCVD <- plot_ly(dfD, x = ~Temp..Celsius, y = ~COD, 
                   z = ~diff_Selenium, color = ~Veg, colors = colorsScale)
figTCVD <- figTCVD %>% add_markers()
figTCVD <- figTCVD %>% layout(scene = list(xaxis = list(title = 'Temp Celsius'),
                                           yaxis = list(title = 'COD mg/L'),
                                           zaxis = list(title = 'Difference Selenium mg/L')))
figTCVD

#---
#Diff Temp vs COD on Media
figTCMD <- plot_ly(dfD, x = ~Temp..Celsius, y = ~COD, 
                   z = ~diff_Selenium, color = ~MediaType, colors = colorsScale)
figTCMD <- figTCMD %>% add_markers()
figTCMD <- figTCMD %>% layout(scene = list(xaxis = list(title = 'Temp Celsius'),
                                           yaxis = list(title = 'COD mg/L'),
                                           zaxis = list(title = 'Difference Selenium mg/L')))
figTCMD

#---

#Diff Temp vs DO on Veg
figTDOVD <- plot_ly(dfD, x = ~Temp..Celsius, y = ~DO.mg.L, 
                   z = ~diff_Selenium, color = ~Veg, colors = colorsScale)
figTDOVD <- figTDOVD %>% add_markers()
figTDOVD <- figTDOVD %>% layout(scene = list(xaxis = list(title = 'Temp Celsius'),
                                           yaxis = list(title = 'DO mg/L'),
                                           zaxis = list(title = 'Difference Selenium mg/L')))
figTDOVD

#---
#Diff Temp vs DO on Media
figTDOMD <- plot_ly(dfD, x = ~Temp..Celsius, y = ~DO.mg.L, 
                   z = ~diff_Selenium, color = ~MediaType, colors = colorsScale)
figTDOMD <- figTDOMD %>% add_markers()
figTDOMD <- figTDOMD %>% layout(scene = list(xaxis = list(title = 'Temp Celsius'),
                                           yaxis = list(title = 'DO mg/L'),
                                           zaxis = list(title = 'Difference Selenium mg/L')))
figTDOMD

#---

#Diff Temp vs Arsenic on Veg
figTArVD <- plot_ly(dfD, x = ~Temp..Celsius, y = ~Arsenic, 
                    z = ~diff_Selenium, color = ~Veg, colors = colorsScale)
figTArVD <- figTArVD %>% add_markers()
figTArVD <- figTArVD %>% layout(scene = list(xaxis = list(title = 'Temp Celsius'),
                                             yaxis = list(title = 'Arsenic mg/L'),
                                             zaxis = list(title = 'Difference Selenium mg/L')))
figTArVD

#---
#Diff Temp vs Arsenic on Media
figTArMD <- plot_ly(dfD, x = ~Temp..Celsius, y = ~Arsenic, 
                    z = ~diff_Selenium, color = ~MediaType, colors = colorsScale)
figTArMD <- figTArMD %>% add_markers()
figTArMD <- figTArMD %>% layout(scene = list(xaxis = list(title = 'Temp Celsius'),
                                             yaxis = list(title = 'Arsenic mg/L'),
                                             zaxis = list(title = 'Difference Selenium mg/L')))
figTArMD

#---

#Diff Nit vs COD on Veg
figNCVD <- plot_ly(dfD, x = ~Nitrate, y = ~COD, 
                 z = ~diff_Selenium, color = ~Veg, colors = colorsScale)
figNCVD <- figNCVD %>% add_markers()
figNCVD <- figNCVD %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                         yaxis = list(title = 'COD mg/L'),
                                         zaxis = list(title = 'Difference Selenium mg/L')))
figNCVD

#---
#Diff Nit vs COD on Media
figNCMD <- plot_ly(dfD, x = ~Nitrate, y = ~COD, 
                  z = ~diff_Selenium, color = ~MediaType, colors = colorsScale)
figNCMD <- figNCMD %>% add_markers()
figNCMD <- figNCMD %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                         yaxis = list(title = 'COD mg/L'),
                                         zaxis = list(title = 'Difference Selenium mg/L')))
figNCMD



#----------------------------------
#----------------------------------

#The difference with differences on media - Nit and COD
figDNDCMD <- plot_ly(dfD, x = ~diff_Nitrate, y = ~diff_COD, 
                   z = ~diff_Selenium, color = ~MediaType, colors = colorsScale)
figDNDCMD <- figDNDCMD %>% add_markers()
figDNDCMD <- figDNDCMD %>% layout(scene = list(xaxis = list(title = 'Difference Nitrate mg/L'),
                                           yaxis = list(title = 'Difference COD mg/L'),
                                           zaxis = list(title = 'Difference Selenium mg/L')))
figDNDCMD

#---

#The difference with differences on media - Nit and Temp
figDNDTMD <- plot_ly(dfD, x = ~diff_Nitrate, y = ~diff_Temp..Celsius, 
                     z = ~diff_Selenium, color = ~MediaType, colors = colorsScale)
figDNDTMD <- figDNDTMD %>% add_markers()
figDNDTMD <- figDNDTMD %>% layout(scene = list(xaxis = list(title = 'Difference Nitrate mg/L'),
                                               yaxis = list(title = 'Difference Temp Celsius'),
                                               zaxis = list(title = 'Difference Selenium mg/L')))
figDNDTMD

#---

#The difference with differences on Veg - Nit and COD
figDNDCVD <- plot_ly(dfD, x = ~diff_Nitrate, y = ~diff_COD, 
                     z = ~diff_Selenium, color = ~Veg, colors = colorsScale)
figDNDCVD <- figDNDCVD %>% add_markers()
figDNDCVD <- figDNDCVD %>% layout(scene = list(xaxis = list(title = 'Difference Nitrate mg/L'),
                                               yaxis = list(title = 'Difference COD mg/L'),
                                               zaxis = list(title = 'Difference Selenium mg/L')))
figDNDCVD

#---

#The difference with differences on Veg - Nit and Temp
figDNDTVD <- plot_ly(dfD, x = ~diff_Nitrate, y = ~diff_Temp..Celsius, 
                     z = ~diff_Selenium, color = ~Veg, colors = colorsScale)
figDNDTVD <- figDNDTVD %>% add_markers()
figDNDTVD <- figDNDTVD %>% layout(scene = list(xaxis = list(title = 'Difference Nitrate mg/L'),
                                               yaxis = list(title = 'Difference Temp Celsius'),
                                               zaxis = list(title = 'Difference Selenium mg/L')))
figDNDTVD

#----------------------



#-----
