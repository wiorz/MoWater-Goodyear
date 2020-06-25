load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
library( emmeans)


#ANOVA
#Uses library(emmeans)
#test for homogeneity; compares the behavior (slope) with the addition of covariate

#-- by Bin --

#COD vs sel
anoSelvCOD <- anova_test(Selenium ~ COD * ID, data = dfDataSel)
get_anova_table(anoSelvCOD)
# maybe covariant! p = .1

#COD + Temp vs sel
anoSelvCODT <- anova_test(Selenium ~ (COD + Temp..Celsius) * ID, data = dfDataSel)
get_anova_table(anoSelvCODT)
# maybe covariant! p = .306

#pH vs sel
anoSelvpH <- anova_test(Selenium ~ pH * ID, data = dfDataSel)
get_anova_table(anoSelvpH)
# is covariant! p = 0.469

#T vs sel
anoSelvT <- anova_test(Selenium ~ Temp..Celsius * ID, data = dfDataSel)
get_anova_table(anoSelvT)
# is covariant! p = 0.45

#DO vs sel
anoSelvDO <- anova_test(Selenium ~ DO.mg.L * ID, data = dfDataSel)
get_anova_table(anoSelvDO)
# highest is covariant! p = 0.67

#Nit vs Sel
anoSelvNit <- anova_test(Selenium ~ Nitrate * ID, data = dfDataSel)
get_anova_table(anoSelvNit)
#P is low so it's bad = = 1.15e-7

#Phosphorus vs Sel
anoSelvPho <- anova_test(Selenium ~ Phosphorus * ID, data = dfDataSel)
get_anova_table(anoSelvPho)
# maybe is covariant! p = 0.21

#-- by veg --
# DO test but for veg type
anoSelvDOVeg <- anova_test(Selenium ~ DO.mg.L * Veg, data = dfDataSel)
get_anova_table(anoSelvDOVeg)
# p = 0.986

#pH test but for veg type
anoSelvpHVeg <- anova_test(Selenium ~ pH * Veg, data = dfDataSel)
get_anova_table(anoSelvpHVeg)
# p = 0.187!

#COD test but for veg type
anoSelvCODVeg <- anova_test(Selenium ~ COD * Veg, data = dfDataSel)
get_anova_table(anoSelvCODVeg)
# p > 0.7

#T test but for veg type
anoSelvTVeg <- anova_test(Selenium ~ Temp..Celsius * Veg, data = dfDataSel)
get_anova_table(anoSelvTVeg)
# p = 0.35

#Nit test but for veg type
anoSelvNitVeg <- anova_test(Selenium ~ Nitrate * Veg, data = dfDataSel)
get_anova_table(anoSelvNitVeg)
# p = 0.02

#Phosphorus test but for media type
anoSelvPhoVeg <- anova_test(Selenium ~ Phosphorus * Veg, data = dfDataSel)
get_anova_table(anoSelvPhoVeg)
# p = 0.04

#-------------------------------------

#-- by train -- 

# DO test but for train type
anoSelvDOTrain <- anova_test(Selenium ~ DO.mg.L * TrainGroup, data = dfDataSel)
get_anova_table(anoSelvDOTrain)
# p > 0.459!

#pH test but for train type
anoSelvpHTrain <- anova_test(Selenium ~ pH * TrainGroup, data = dfDataSel)
get_anova_table(anoSelvpHTrain)
# p = 0.57!

#COD test but for train type
anoSelvCODTrain <- anova_test(Selenium ~ COD * TrainGroup, data = dfDataSel)
get_anova_table(anoSelvCODTrain)
# p > 0.069

#T test but for train type
anoSelvTTrain <- anova_test(Selenium ~ Temp..Celsius * TrainGroup, data = dfDataSel)
get_anova_table(anoSelvTTrain)
# p = 0.24

#Nit test but for train type
anoSelvNitTrain <- anova_test(Selenium ~ Nitrate * TrainGroup, data = dfDataSel)
get_anova_table(anoSelvNitTrain)
# p = 4.14e-9

#Phosphorus test but for train type
anoSelvPhoTrain <- anova_test(Selenium ~ Phosphorus * TrainGroup, data = dfDataSel)
get_anova_table(anoSelvPhoTrain)
# p = 0.1

#--- by Media ---
# DO test but for media type
anoSelvDOMedia <- anova_test(Selenium ~ DO.mg.L * MediaType, data = dfDataSel)
get_anova_table(anoSelvDOMedia)
# p > 0.825!

#pH test but for media type
anoSelvpHMedia <- anova_test(Selenium ~ pH * MediaType, data = dfDataSel)
get_anova_table(anoSelvpHMedia)
# p = 0.81.

#COD test but for media type
anoSelvCODMedia <- anova_test(Selenium ~ COD * MediaType, data = dfDataSel)
get_anova_table(anoSelvCODMedia)
# p > 0.95

#T test but for media type
anoSelvTMedia <- anova_test(Selenium ~ Temp..Celsius * MediaType, data = dfDataSel)
get_anova_table(anoSelvTMedia)
# p = 0.115

#Nit test but for media type
anoSelvNitMedia <- anova_test(Selenium ~ Nitrate * MediaType, data = dfDataSel)
get_anova_table(anoSelvNitMedia)
# p = 0.99!!!

#Phosphorus test but for media type
anoSelvPhoMedia <- anova_test(Selenium ~ Phosphorus * MediaType, data = dfDataSel)
get_anova_table(anoSelvPhoMedia)
# p = 0.33

#----------------------------------------------

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

#------------------------------------------------------

