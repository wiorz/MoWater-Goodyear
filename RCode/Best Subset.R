load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/complete_cases.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/tshirt-wave.rda")
library(tidyverse); theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank())
library(leaps)

#--- Variable Selection ---

GetLeapTable <- function(leapSummaryIn){
    result <- cbind(leapSummaryIn$adjr2, leapSummaryIn$cp, leapSummaryIn$bic)
    return(result)
}

GetMinMax <- function(leapSummaryIn){
    result <- data.frame(
        Adj.R2 = which.max(leapSummaryIn$adjr2),
        CP = which.min(leapSummaryIn$cp),
        BIC = which.min(leapSummaryIn$bic)
    )
    return(result)
}

#---------------------------

#Finding the 10 best subset selection models
#Uses library(leaps)
#Using the many datasets ... but reduced to 17 observations after removing NAs
leapsResult <- regsubsets(Selenium ~ pH + DO.mg.L + Temp..Celsius + Nitrate + 
                              COD + Phosphorus + Arsenic + Veg + MediaType ,
                          data = dfDataSel, nvmax = 10)
# view results
leapSummary <- summary(leapsResult)
leapTable <- GetLeapTable(leapSummary)
minMaxLeap <- GetMinMax(leapSummary)

minMaxLeap
#minMaxLeap result
#max adj.r2     min cp      min bic
#6              2           5
#---
leapTable 
#[2,] 0.917 -3.925 -36.171
#[5,] 0.953 -1.853 -41.444
#[6,] 0.954 -0.169 -40.389
#---

leapSummary
#[2,] Temp, Nit
#[5,] Temp, Nit, COD, Arsenic, Veg Type C
#[6,] pH, Temp, Nit, COD, Arsenic, Veg Type C

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResult, scale = "r2", 
     main = "10 variable Best Subsets Regression on Selenium with 17 observ.")


#----------------------------------------------------------

#Just using veg, no media
leapsResultVeg <- regsubsets(Selenium ~ pH + DO.mg.L + Temp..Celsius + Nitrate + 
                              COD + Phosphorus + Arsenic + Veg,
                          data = dfDataSel, nvmax = 10)
# view results
leapSummaryVeg <- summary(leapsResultVeg)
leapTableVeg <- GetLeapTable(leapSummaryVeg)

#Find the min and max
minMaxLeapVeg <- GetMinMax(leapSummaryVeg)
minMaxLeapVeg
#Adj.R2 CP BIC
#6      5  5
#---

leapTableVeg
#[5,] 0.953  2.235 -41.444
#[6,] 0.954  3.516 -40.389

leapSummaryVeg
#Temp, Nit, COD, Arsenic, Veg Type C
#pH, Temp, Nit, COD, Arsenic, Veg Type C

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResultVeg, scale = "r2", 
     main = "10 Best Subsets Regression on Selenium")

#-----

#Just using media, no veg
leapsResultMedia <- regsubsets(Selenium ~ pH + DO.mg.L + Temp..Celsius + Nitrate + 
                                 COD + Phosphorus + Arsenic + MediaType,
                             data = dfDataSel, nvmax = 10)
# view results
leapSummaryMedia <- summary(leapsResultMedia)
leapTableMedia <- GetLeapTable(leapSummaryMedia)
minMaxLeapMedia <- GetMinMax(leapSummaryMedia)

minMaxLeapMedia
#Adj.R2 CP BIC
#6      2  5
#---

leapTableMedia
# [2,] 0.917  0.852 -36.171
# [5,] 0.942  1.564 -37.717
# [6,] 0.942  2.949 -36.557

leapSummaryMedia
#[2,] Temp, Nit
#[5,] Temp, Nitrate, COD, Arsenic, Media Soil
#[6,] Temp, Nitrate, COD, Phosphorus, Arsenic, Media Soil

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResultMedia, scale = "r2", 
     main = "10 Best Subsets Regression on Selenium")

#---

#Just using train group
leapsResultTrain <- regsubsets(Selenium ~ pH + DO.mg.L + Temp..Celsius + Nitrate + 
                                   COD + Phosphorus + TrainGroup,
                               data = dfDataSel, nvmax = 10)
# view results
leapSummaryTrain <- summary(leapsResultTrain)
leapTableTrain <- GetLeapTable(leapSummaryTrain)
minMaxLeapTrain <- GetMinMax(leapSummaryTrain)

minMaxLeapTrain
#Adj.R2 CP BIC
#4      2  2
#---

leapTableTrain
#[2,] 0.917 -2.079 -36.171
#[3,] 0.920 -0.941 -35.065
#[4,] 0.920  0.395 -33.695

leapSummaryTrain
#[2,] Temp, Nit
#[4,] Temp, Nitrate, COD, Train Group 4

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResultTrain, scale = "r2", 
     main = "10 Best Subsets Regression on Selenium")

#-------------------------------------------

#--- using long data with fewer variables ---

#using dfCLong as dataset

leapsResultL <- regsubsets(Selenium ~ Nitrate + COD + Phosphorus + Arsenic + 
                               Veg + MediaType,
                          data = dfCLong, nvmax = 5)
# view results
leapSummaryL <- summary(leapsResultL)
leapTableL <- GetLeapTable(leapSummaryL)
minMaxLeapL <- GetMinMax(leapSummaryL)

minMaxLeapL
#minMaxLeap result
#max adj.r2     min cp      min bic
#5              5           3
#---
leapTableL 
# [3,] 0.589 12.143 -205.610
# [5,] 0.602  5.601 -205.134
#---
leapSummaryL
#[3,] Nit, COD, Phosphorus
#[5,] Nit, COD, Phosphorus, Arsenic, Veg Type C

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResultL, scale = "r2", 
     main = "5 variable Best Subsets Regression on Selenium with 253 observ.")

#----------------------------------------------------------

#Just using veg, no media
leapsResultVegL <- regsubsets(Selenium ~ Nitrate + COD + Phosphorus + Arsenic + 
                                Veg, data = dfCLong, nvmax = 5)
# view results
leapSummaryVegL <- summary(leapsResultVegL)
leapTableVegL <- GetLeapTable(leapSummaryVegL)
minMaxLeapVegL <- GetMinMax(leapSummaryVegL)

minMaxLeapVegL
#Adj.R2 CP BIC
#5      5  3
#---

leapTableVegL
# [3,] 0.589 14.327 -205.610
# [5,] 0.602  7.695 -205.134

leapSummaryVegL
#Nit, COD, Phosphorus
#Nit, COD, Phosphorus, Arsenic, Veg Type C

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResultVegL, scale = "r2", 
     main = "5 Best Subsets Regression on Selenium")

#-----

#Just using media, no veg
leapsResultMediaL <- regsubsets(Selenium ~ Nitrate + COD + Phosphorus + Arsenic + 
                                   MediaType, data = dfCLong, nvmax = 5)
# view results
leapSummaryMediaL <- summary(leapsResultMediaL)
leapTableMediaL <- GetLeapTable(leapSummaryMediaL)
minMaxLeapMediaL <- GetMinMax(leapSummaryMediaL)

minMaxLeapMediaL
#Adj.R2 CP BIC
#5      5  3
#---

leapTableMediaL
# [3,] 0.589 13.977 -205.610
# [5,] 0.600  8.995 -203.473

leapSummaryMediaL
#[2,] Nit,COD, Phosphorus
#[5,] Nit,COD, Phosphorus, Arsenic, Media PM

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResultMediaL, scale = "r2", 
     main = "5 Best Subsets Regression on Selenium")

#---

#Just using train group
leapsResultTrainL <- regsubsets(Selenium ~ Nitrate + COD + Phosphorus + Arsenic 
                                +  TrainGroup, data = dfCLong, nvmax = 5)
# view results
leapSummaryTrainL <- summary(leapsResultTrainL)
leapTableTrainL <- GetLeapTable(leapSummaryTrainL)
minMaxLeapTrainL <- GetMinMax(leapSummaryTrainL)

minMaxLeapTrainL
#Adj.R2 CP BIC
# 5     4  3
#---

leapTableTrainL
# [3,] 0.589 11.481 -205.610
# [4,] 0.596  7.975 -205.567
# [5,] 0.597  8.122 -201.908

leapSummaryTrainL
#[2,] Nit, COD, Phosphorus
#[4,] Nit, COD, Phosphorus, Arsenic, Train Group 2

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsResultTrainL, scale = "r2", 
     main = "5 Best Subsets Regression on Selenium")

#-------------------------------------------

#--- K Fold cross validation ---


GetModelFormula <- function(id, object, outcome){
    # get models data
    models <- summary(object)$which[id,-1]
    # Get outcome variable
    # Get model predictors
    predictors <- names(which(models == TRUE))
    predictors <- paste(predictors, collapse = "+")
    
    return(as.formula(paste0(outcome, "~", predictors)))
    
}

GetCVError <- function(modelFormula, data){
    set.seed(1)
    train.control <- trainControl(method = "cv", number = 5)
    cv <- train(modelFormula, data = data, method = "lm",
                trControl = train.control)
    return(cv$results$RMSE)
}

#----------------------------

#dfDataSel without date
#Get suggested models base on number of variables
dfND <- dfDataSel %>% 
            select(-date)
models <- regsubsets(Selenium~., data = dfND, nvmax = 5)

GetModelFormula(2, models , "Selenium")
#Temp and Nit are best 2
GetModelFormula(3, models , "Selenium")
#Nitrate + Temp..Celsius + VegType_B
GetModelFormula(4, models , "Selenium")
#Arsenic + Nitrate + COD + Temp..Celsius


tibble(predictors = 1:ncol(dfND) - 1,
       adj_R2 = leapSummaryL$adjr2,
       Cp = leapSummaryL$cp,
       BIC = leapSummaryL$bic) %>%
    gather(statistic, value, -predictors) %>%
    ggplot(aes(predictors, value, color = statistic)) +
    geom_line(show.legend = F) +
    geom_point(show.legend = F) +
    facet_wrap(~ statistic, scales = "free")

k = 10
folds = sample(1:k, nrow(coll), replace=TRUE)
cv.errors = matrix(NA, k, 17, dimnames=list(NULL, c(1:17)))

for (j in 1:k){
        best.fit = regsubsets(Apps ~., data=coll[folds!=j,], nvmax = 17)
        testmat = model.matrix(Apps ~ ., data = coll[folds==j,])
        for (i in 1:17){
             coefi = coef(best.fit, id=i)
             xvars = names(coefi)
             pred = testmat[,xvars]%*%coefi
             cv.errors[j, i] = mean((coll$Apps[folds==j] - pred)^2)
             }
        }


