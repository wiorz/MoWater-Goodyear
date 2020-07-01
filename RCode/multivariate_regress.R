load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
library( rstatix)

#----------------------------------------------

#--- Multivariate regression analysis ---
mods

fitAll <- dfDataSel %>% 
    lm( Selenium ~ Nitrate + COD + DO.mg.L + pH + Veg + MediaType, data = .)
summary(fitAll)
#Adj R = 0.575

fitVegDO <- dfDataSel %>% 
    lm( Selenium ~ DO.mg.L + Veg, data = .)
summary(fitVegDO)
#Mult R = 0.2288

fitVeg <- dfDataSel %>% 
    lm( Selenium ~ COD + DO.mg.L + Veg, data = .)
summary(fitVeg)
predict(fitVeg)
#Adj R = 0.489

# See the result
df3 %>% as.data.frame()

fitVegMedia <- dfDataSel %>% 
    lm( Selenium ~ COD + DO.mg.L + Veg + MediaType, data = .)
summary(fitVegMedia)
#Adj R = 0.593

fitMedia <- dfDataSel %>% 
    lm( Selenium ~ COD + DO.mg.L + pH + Nitrate + MediaType, data = .)
summary(fitMedia)
#Adj R = 0.403

fitVegMediaWpH <- dfDataSel %>% 
    lm( Selenium ~ COD + DO.mg.L + pH + Veg + MediaType, data = .)
summary(fitVegMediaWpH)
#Adj R = 0.576

fitVegMediaWNit <- dfDataSel %>% 
    lm( Selenium ~ COD + DO.mg.L + Nitrate + Veg + MediaType, data = .)
summary(fitVegMediaWNit)
#Adj R = 0.599

fitAllTypes <- dfDataSel %>% 
    lm( Selenium ~ COD + DO.mg.L + Veg + MediaType + ID + TrainGroup, data = .)
summary(fitAllTypes)
#Adj R = 0.5432

fitDiff <- dfDataSel %>% 
    lm( Selenium ~ Nitrate + COD + DO.mg.L + pH + Veg + MediaType, data = .)
summary(fitDiff)


modelDO <- lm(Selenium ~ DO.mg.L + ID, data = dfDataSel)
modelDO.metrics <- augment(modelDO) %>%
    select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details
modelDO

#--------------------------------------------

#--- Check Temp pairing ---

fitTC <- dfDataSel %>% 
    lm( Selenium ~ COD * Temp..Celsius, data = .)
summary(fitTC)
# Multiple R-squared:  0.4887,	Adjusted R-squared:  0.4448 
# p-value: 2.766e-05