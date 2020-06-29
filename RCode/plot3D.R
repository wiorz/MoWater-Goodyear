load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/complete_cases.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/tshirt-wave.rda")
library(tidyverse); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())

#-----------------------------------------
library("plot3D")
library("plotly")

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

fig <- plot_ly(dfT, x = ~Nitrate, y = ~Temp..Celsius, z = ~Selenium, color = ~Veg)
fig <- fig %>% add_markers()
#fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
#                                   yaxis = list(title = 'Gross horsepower'),
#                                   zaxis = list(title = '1/4 mile time')))
fig
