load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/lmResults.rda")
library( tidyverse); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
library( lubridate)
library( rcartocolor)
library( RColorBrewer)
library( viridis)
library( scales)
library( rstatix)
library( dplyr)
library( ggpubr)
library( emmeans)
suppressMessages(library( fields))

#-----------------------------------
#Confirmative analysis (???)

#-----------------------------------

#--- init ---

#Init df to hold all mrs values
dfColnames <- c("Name", "mrsValue")
allMRS <- as.data.frame(matrix(rep(NA, 2), nrow = 1))
names(allMRS) <- dfColnames

#-----------------------------------

#--- Functions ---

GetMRS <- function(cvrList){
    sumResult <- 0.0
    for(val in cvrList){
        sumResult<- val * val + sumResult #square the values and sum
        mrsResult <- sumResult / length(cvrList)
    }
    return(mrsResult)
}

AddMRSToDF <- function(dfset, nameStr, val){
    tmpRow <- data.frame(nameStr, val)
    names(tmpRow) <- dfColnames
    dfset <- rbind(dfset, tmpRow)  
    return(dfset)
}

#----------------------------------

#Model using only Nitrate
cvrVectNit <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ Nitrate, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectNit[irow] <- cvResid
}

mrsNit <- GetMRS(cvrVectNit)
mrsNit
# mean R square = 7.360115e-05

allMRS <- AddMRSToDF(allMRS, "Nitrate", mrsNit)

#---

#pH
cvrVectpH <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ pH, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectpH[irow] <- cvResid
}

mrspH <- GetMRS(cvrVectpH)
mrspH
# mean R Square = 8.224113e-05

allMRS <- AddMRSToDF(allMRS, "pH", mrspH)

#-----

#Temp
cvrVectT <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ Temp..Celsius, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectT[irow] <- cvResid
}

mrsT <- GetMRS(cvrVectT)
mrsT
# mean R Square = 4.617402e-05

allMRS <- AddMRSToDF(allMRS, "Temp", mrsT)

#-----

#Arsenic
cvrVectAr <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ Arsenic, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectAr[irow] <- cvResid
}

mrsAr <- GetMRS(cvrVectAr)
mrsAr
# mean R Square = 4.617402e-05

allMRS <- AddMRSToDF(allMRS, "Arsenic", mrsAr)

#-----

#Model using only COD
cvrVectCOD <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectCOD[irow] <- cvResid
}

mrsCOD <- GetMRS(cvrVectCOD)
mrsCOD
# mean R square = 4.592776e-05

allMRS <- AddMRSToDF(allMRS, "COD", mrsCOD)

#-----

#Model using only DO.mg.L
cvrVectDO <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ DO.mg.L, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectDO[irow] <- cvResid
}

mrsDO <- GetMRS(cvrVectDO)
mrsDO
# mean R square = 6.91475e-05

allMRS <- AddMRSToDF(allMRS, "DO", mrsDO)

#-----

#Model using only COD and DO.mg.L base on Veg
cvrVectCODDOV <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ (COD + DO.mg.L) * Veg, data = .) #TODO: check if we should use the * or just plain +
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectCODDOV[irow] <- cvResid
}

mrsCODDOV <- GetMRS(cvrVectCODDOV)
mrsCODDOV
# mean R square = 5.081343e-05

allMRS <- AddMRSToDF(allMRS, "COD, DO on Veg", mrsCODDOV)

#-----

#Model using only COD and DO.mg.L base on Veg and MediaType
cvrVectVM <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + DO.mg.L + Veg + MediaType, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectVM[irow] <- cvResid
}

mrsVM <- GetMRS(cvrVectVM)
mrsVM
# mean R square = 5.742702e-05

allMRS <- AddMRSToDF(allMRS, "COD, DO on Veg and Media", mrsVM)
#-----

#Model using only COD and DO.mg.L and Temp base on Veg
cvrVectTV <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + DO.mg.L + Temp..Celsius + Veg, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectTV[irow] <- cvResid
}

mrsTV <- GetMRS(cvrVectTV)
mrsTV
# mean R square = 8.603784e-05

allMRS <- AddMRSToDF(allMRS, "COD, DO, Temp on Veg", mrsTV)

#-----

#Model using only COD and DO.mg.L and pH base on Veg
cvrVectpHV <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + DO.mg.L + pH + Veg, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectpHV[irow] <- cvResid
}

mrspHV <- GetMRS(cvrVectpHV)
mrspHV
# mean R square = 8.210108e-05

allMRS <- AddMRSToDF(allMRS, "COD, DO, pH on Veg", mrspHV)

#-----

#Model using only COD and DO.mg.L and Arsenic base on Veg
cvrVectArV <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + DO.mg.L + Arsenic + Veg, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectArV[irow] <- cvResid
}

mrsArV <- GetMRS(cvrVectArV)
mrsArV
# mean R square = 6.463256e-05


allMRS <- AddMRSToDF(allMRS, "COD, DO, Arsenic on Veg", mrsArV)
#-----

#Model using only COD and DO.mg.L and Nitrate base on Veg
cvrVectNitV <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + DO.mg.L + Nitrate + Veg, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectNitV[irow] <- cvResid
}

mrsNitV <- GetMRS(cvrVectNitV)
mrsNitV
# mean R square = 5.919261e-05

allMRS <- AddMRSToDF(allMRS, "COD, DO, Nitrate on Veg", mrsNitV)
#-----

#Model using only COD and DO.mg.L and pH base on Veg and Media
cvrVectpHVM <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + DO.mg.L + pH + Veg + MediaType, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectpHVM[irow] <- cvResid
}

mrspHVM <- GetMRS(cvrVectpHVM)
mrspHVM
# mean R square = 6.828949e-05

allMRS <- AddMRSToDF(allMRS, "COD, DO, pH on Veg and Media", mrspHVM)

#-----

#Model using only DO.mg.L on Veg
cvrVectDOV <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ DO.mg.L + Veg, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectDOV[irow] <- cvResid
}

mrsDOV <- GetMRS(cvrVectDOV)
mrsDOV
# mean R square = 8.030651e-05

allMRS <- AddMRSToDF(allMRS, "DO on Veg", mrsDOV)

#-----

#Model using only COD on Veg
cvrVectCODV <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + Veg, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectCODV[irow] <- cvResid
}

mrsCODV <- GetMRS(cvrVectCODV)
mrsCODV
# mean R square = 4.944044e-05

allMRS <- AddMRSToDF(allMRS, "COD on Veg", mrsCODV)
#----

#Model using only COD on Veg and Media
cvrVectCODVM <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + Veg + MediaType, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectCODVM[irow] <- cvResid
}

mrsCODVM <- GetMRS(cvrVectCODVM)
mrsCODVM
# mean R square = 5.248441e-05

allMRS <- AddMRSToDF(allMRS, "COD on Veg and Media", mrsCODVM)

#-----

#Model using only COD, Nitrate and Phosphorus
cvrVectCODNitPho <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + Nitrate + Phosphorus, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectCODNitPho[irow] <- cvResid
}

mrsCODNitPho <- GetMRS(cvrVectCODNitPho)
mrsCODNitPho
# mean R Square = 7.62546e-05

allMRS <- AddMRSToDF(allMRS, "COD, Nitrate and Phosphorus", mrsCODNitPho)

#----
#Model using only COD, Nitrate and Phosphorus on Veg
cvrVectCODNitPhoV <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + Nitrate + Phosphorus + Veg, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectCODNitPhoV[irow] <- cvResid
}

mrsCODNitPhoV <- GetMRS(cvrVectCODNitPhoV)
mrsCODNitPhoV
# mean R Square = 7.82875e-05

allMRS <- AddMRSToDF(allMRS, "COD, Nitrate and Phosphorus on Veg", mrsCODNitPhoV)
#-----

#pH, DO and Temp
cvrVectDOpHT <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ pH + DO.mg.L + Temp..Celsius, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectDOpHT[irow] <- cvResid
}

mrsDOpHT <- GetMRS(cvrVectDOpHT)
mrsDOpHT
# mean R Square = 2.558565e-04

allMRS <- AddMRSToDF(allMRS, "pH, DO and Temp", mrsDOpHT)

#-----

#-----

#Model using only COD and pH
cvrVectCODpH <- rep(NA, nrow(dfDataSel))

for (irow in 1:nrow(dfDataSel)) {
    tmp <- dfDataSel[-irow, ]
    fitTmp <- tmp %>% 
        lm( Selenium ~ COD + pH, data = .)
    resultTmp <- predict(fitTmp, newx = dfDataSel[irow, ])
    cvResid <- dfDataSel[irow, "Selenium"] - resultTmp
    cvrVectCODNitPho[irow] <- cvResid
}

sumResultCODNitPho <- 0.0
for(val in cvrVectCODNitPho){
    sumResultCODNitPho <- val * val + sumResultCODNitPho #square the values and sum
    mrsCODNitPho <- sumResultCODNitPho / length(cvrVectCODNitPho)
}
mrsCODNitPho

#------------------------------------------
