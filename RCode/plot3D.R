load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/complete_cases.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/tshirt-wave.rda")
library(tidyverse); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
library( viridis)
#library("plot3D")
library("plotly")
#-----------------------------------------


#----------------------------------------------

dfT <- dfDataSel
dfT$Veg <- as.factor(dfT$Veg)
dfT$ID <- as.factor(dfT$ID)
dfT$MediaType <- as.factor(dfT$MediaType)
dfT$TrainGroup <- as.factor(dfT$TrainGroup)

save(dfT, file = "Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedFactors.rda")

#----------------------------
#start here
#load the cleanedFactors.rda above

fig <- plot_ly(dfT, x = ~Nitrate, y = ~Temp..Celsius, 
               z = ~Selenium, color = ~Veg, colors = colorsScale)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                   yaxis = list(title = 'Temp Celsius'),
                                   zaxis = list(title = 'Selenium mg/L')))
fig
ggsave()

colorsScale <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

fig2 <- plot_ly(dfT, x = ~Temp..Celsius, y = ~Nitrate, z = ~Selenium, 
               color = ~Veg, size = ~Selenium, colors = colorsScale,
               marker = list(symbol = 'circle', sizemode = 'diameter'), 
               sizes = c(5, 100))
fig2 <- fig %>% layout(title = 'Selenium vs VegType',
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

figM <- plot_ly(dfT, x = ~Nitrate, y = ~Temp..Celsius, 
               z = ~Selenium, color = ~MediaType, colors = colorsScale)
figM <- figM %>% add_markers()
figM <- figM %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                   yaxis = list(title = 'Temp Celsius'),
                                   zaxis = list(title = 'Selenium mg/L')))
figM

#------------------
#Nit vs COD
figVL <- plot_ly(dfT, x = ~Nitrate, y = ~COD, 
                z = ~Selenium, color = ~Veg, colors = colorsScale)
figVL <- figVL %>% add_markers()
figVL <- figVL %>% layout(scene = list(xaxis = list(title = 'Nitrate mg/L'),
                                     yaxis = list(title = 'COD'),
                                     zaxis = list(title = 'Selenium mg/L')))
figVL


