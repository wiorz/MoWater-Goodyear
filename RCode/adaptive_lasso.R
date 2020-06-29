load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/complete_cases.rda")
load("Baylor/MoWater/proj6/Adaptive_LASSO/ab3_do_example.Rda")
library(xts)
library(viridis)
library(glmnet)

predict.col <- which(colnames((ab3_do_example)) == "AB3.Z7.Ammonia.mg.N.L")
yy <- as.matrix(ab3_do_example[,predict.col])
xx <- as.matrix(ab3_do_example[,-predict.col])

## Use ridge regression just to get initial coefficient estimates
mod.ridge <- cv.glmnet(xx, yy, alpha=0)

# Use the inverse of the initial coefficients as weights
coef(mod.ridge, s = mod.ridge$lambda.min)[ , 1]
weights <- 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1

#Background: I want to run the code from the adaptive lasso example but I ran 
#into some issues. 
#I copied the code over and decided to step through it while changing the 
#minimum number of things.
#I'm aware that the dfC dataset only has 17 rows, but that's what we have and
#I want to run this analysis to see what it looks like, even if it's not
#accurate.

#Issue: Got an error at line 36 (started with "mod.ridge <-")
#Error message: Error in weighted.mean.default(y, weights) : 
#               'x' and 'w' must have the same length
#               In addition: Warning message:
#               In storage.mode(y) <- "double" : NAs introduced by coercion

#Possible cause: I notice that the dataset given in the adaptive lasso 
#example has a date row, but it's not listed as a column. Maybe the coercion
#error is from when the function trying to convert dates for the dfC dataset.

#Questions:
#1. How do I convert that date column into something similar to ab3_do_example?
#2. Will the above conversion even fix the issue? If not, what else can I do?

predict.col <- which(colnames((ab3_do_example)) == "AB3.Z7.Ammonia.mg.N.L")
yy <- as.matrix(ab3_do_example[,predict.col])
xx <- as.matrix(ab3_do_example[,-predict.col])

dfCLong <- dfDataSel
dfCLong$DO.mg.L <- NULL
dfCLong$Temp..Celsius <- NULL
dfCLong$pH <- NULL
dfCLong <- dfCLong[complete.cases(dfCLong), ]

save(dfC, dfCLong, file = "Baylor/MoWater/proj6/MoWater-Goodyear//clean/tshirt-wave.rda")

#coVarCols <- c("ID", "Selenium", "Veg", "TrainGroup", "MediaType")
#yy <- as.matrix(dfC[,c("date", "Selenium")]) # this is to extract date the response variable
#Below is to get the columns (other than Selenium) that are numeric to avoid error.
#excludeCols <- names(dfC) %in% coVarCols
#x <- as.matrix(dfC[!excludeCols])

X<- dfC
X$date <- NULL
X$Selenium<- NULL
X$ID <- NULL
#X$TrainGroup<- factor( X$TrainGroup)
X$TrainGroup<- NULL
#X$Veg<- factor( X$Veg)
X$Veg<- NULL
#X$MediaType <- factor( X$MediaType)
X$MediaType <- NULL
X$Arsenic <- NULL
Y<- dfC$Selenium
Xm <- as.matrix(X)
Ym <- as.matrix(Y)

#Ym <- Xm[, 4] #change to COD if needed

#NOTE: this is where the error occurs.
## Use ridge regression just to get initial coefficient estimates
#(mod.ridge <- cv.glmnet(xx, yy, alpha=0))
#summary(mod.ridge)

(mod.ridge2 <- cv.glmnet(Xm, Ym, alpha=0))

#(obj<- lm( Y~., data=X)) #lm to get coef.

# Use the inverse of the initial coefficients as weights
#coef(mod.ridge, s = mod.ridge$lambda.min)[ , 1]
#weights <- 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1

coef(mod.ridge2, s = mod.ridge2$lambda.min)[ , 1]
weights2 <- 1/abs(matrix(coef(mod.ridge2, s=mod.ridge2$lambda.min)[, 1][-1]))^1

# Adaptive Lasso Model Fit
(mod.adaptive2 <- cv.glmnet(Xm, Ym,  alpha = 1, penalty.factor = weights2))

# Plot results

par(mfrow=c(1,1))
plot(mod.adaptive2)
rain2 <- plasma(ncol(Xm))

plot(mod.adaptive2$glmnet.fit, xvar="lambda", label=TRUE, col=rain2, 
     cex.lab=1.5, lwd = 3.5, cex.main = 1.5, cex.sub = 2, 
     main = "Adaptive Lasso on Selenium with 6 vars and 17 observations")
abline(v = log(mod.adaptive2$lambda.min), col = "black")
abline(v = log(mod.adaptive2$lambda.1se), col = "blue")
abline(v = log(6*mod.adaptive2$lambda.1se),col= "red")
abline(v = 0,col="green")

# Predict different lambdas, the min, 1se, 6 times 1se, and a very strong penalty (aka lambda = 1)
s.list2 <- c(as.numeric(mod.adaptive2$lambda.min), as.numeric(mod.adaptive2$lambda.1se), as.numeric(6*mod.adaptive2$lambda.1se), 1)

par(mfrow=c(2,2), oma = c(0,0,2,0))


for(s in s.list2) {
    coef <- coef(mod.adaptive2, s=s)
    
    #Identify selected variables
    selected_attributes <- (coef@i[-1]) 
    colnames(Xm)[selected_attributes]
    
    #Compute R-squared
    predictions <- predict(mod.adaptive2, newx=Xm, s=s)
    SSE=mean((Ym-predictions)^2); print(SSE)
    SST=mean((Ym-mean(Ym))^2); print(SST)
    Rsqu=1-SSE/SST; print(Rsqu)
    
    #Plot prediction
    data1 <- Ym
    data2 <- predictions
    max.val <- max(c(data1, data2))
    min.val <- min(c(data1, data2))
    data2plot <- cbind(data1, data2)
    
    plot(x = data2plot[,1], y = data2plot[,2]
         , xlim=c(min.val,max.val), ylim=c(min.val,max.val)
         , xlab="Actual", ylab="Predicted"
         , pch=20
         , main = paste("Lambda = ",round(s,3),", Variables = ", length(selected_attributes), collapse="")
    )
    abline(a=0,b=1,col="blue", lwd=2)
    
    legend("bottomright", 
           inset = c(-.01,-.01),
           legend = c("Observation", "Perfect Fit"
                      , paste0("R-sq = ",round(Rsqu,2))),
           col = c("black", "blue", NA),
           pch = c(20,NA, NA),
           lwd = c(NA,2, NA),
           bty = "n",
           cex=2)
}


#Plot original ammonia data with predictions overlaid
par(mfrow=c(1,1))
plot(index(dfC), as.numeric(dfC$Selenium), type="l", xlab="", ylab="")
title("Goodyear Selenium with Prediction base on 3 variables",xlab="Time", ylab="Selenium (mg/L)", cex.lab=1.5, cex.main=2)
lines(index(dfC),  predict(mod.adaptive2, newx = Xm, s=mod.adaptive2$lambda.1se), col=2)

#-------------------------------------------------------------

X1 <- dfCLong
X1$date <- NULL
X1$Selenium<- NULL
X1$ID <- NULL
#X$TrainGroup<- factor( X$TrainGroup)
X1$TrainGroup<- NULL
#X$Veg<- factor( X$Veg)
X1$Veg<- NULL
#X$MediaType <- factor( X$MediaType)
X1$MediaType <- NULL
Y1<- dfCLong$Selenium
Xm1 <- as.matrix(X1)
Ym1 <- as.matrix(Y1)

#Ym <- Xm[, 4] #change to COD if needed

#NOTE: this is where the error occurs.
## Use ridge regression just to get initial coefficient estimates
#(mod.ridge <- cv.glmnet(xx, yy, alpha=0))
#summary(mod.ridge)

(mod.ridge3 <- cv.glmnet(Xm1, Ym1, alpha=0))

#(obj<- lm( Y~., data=X)) #lm to get coef.

# Use the inverse of the initial coefficients as weights
#coef(mod.ridge, s = mod.ridge$lambda.min)[ , 1]
#weights <- 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1

coef(mod.ridge3, s = mod.ridge3$lambda.min)[ , 1]
weights3 <- 1/abs(matrix(coef(mod.ridge3, s=mod.ridge3$lambda.min)[, 1][-1]))^1

# Adaptive Lasso Model Fit
(mod.adaptive3 <- cv.glmnet(Xm1, Ym1,  alpha = 1, penalty.factor = weights3))

# Plot results

par(mfrow=c(1,1))
plot(mod.adaptive3)
rain3 <- plasma(ncol(Xm1))


plot(mod.adaptive3$glmnet.fit, xvar="lambda", label=TRUE, col=rain3, 
     cex.lab = 2, lwd = 3.5, cex.main = 1.5, cex.sub = 2, 
     main = "Adaptive Lasso on Selenium with 3 var and 253 observations")
abline(v = log(mod.adaptive3$lambda.min), col = "black")
abline(v = log(mod.adaptive3$lambda.1se), col = "blue")
abline(v = log(6*mod.adaptive3$lambda.1se),col= "red")
abline(v = 0,col="green")

#TODO: change the following to dfcLong
s.list2 <- c(as.numeric(mod.adaptive2$lambda.min), as.numeric(mod.adaptive2$lambda.1se), as.numeric(6*mod.adaptive2$lambda.1se), 1)

par(mfrow=c(2,2), oma = c(0,0,2,0))


for(s in s.list2) {
    coef <- coef(mod.adaptive2, s=s)
    
    #Identify selected variables
    selected_attributes <- (coef@i[-1]) 
    colnames(Xm)[selected_attributes]
    
    #Compute R-squared
    predictions <- predict(mod.adaptive2, newx=Xm, s=s)
    SSE=mean((Ym-predictions)^2); print(SSE)
    SST=mean((Ym-mean(Ym))^2); print(SST)
    Rsqu=1-SSE/SST; print(Rsqu)
    
    #Plot prediction
    data1 <- Ym
    data2 <- predictions
    max.val <- max(c(data1, data2))
    min.val <- min(c(data1, data2))
    data2plot <- cbind(data1, data2)
    
    plot(x = data2plot[,1], y = data2plot[,2]
         , xlim=c(min.val,max.val), ylim=c(min.val,max.val)
         , xlab="Actual", ylab="Predicted"
         , pch=20
         , main = paste("Lambda = ",round(s,3),", Variables = ", length(selected_attributes), collapse="")
    )
    abline(a=0,b=1,col="blue", lwd=2)
    
    legend("bottomright", 
           inset = c(-.01,-.01),
           legend = c("Observation", "Perfect Fit"
                      , paste0("R-sq = ",round(Rsqu,2))),
           col = c("black", "blue", NA),
           pch = c(20,NA, NA),
           lwd = c(NA,2, NA),
           bty = "n",
           cex=2)
}

par(mfrow=c(1,1))
plot(index(dfC), as.numeric(dfC$Selenium), type="l", xlab="", ylab="")
title("Goodyear Selenium with Prediction base on 3 variables",xlab="Time", ylab="Selenium (mg/L)", cex.lab=1.5, cex.main=2)
lines(index(dfC),  predict(mod.adaptive2, newx = Xm, s=mod.adaptive2$lambda.1se), col=2)