load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
library( leaps)


#--- Variable Selection ---

# Finding the 10 best selection models
#Uses library(leaps)
leapsResult <- regsubsets(Selenium ~ pH + DO.mg.L + Temp..Celsius + Nitrate + 
                              COD + Phosphorus + Veg + MediaType,
                          data = dfDataSel, nbest = 10)
# view results
summary(leapsResult)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResult, scale = "r2", main = "10 Best Subsets Regression on Selenium",
     cex.lab = 3, cex.axis = 3)
