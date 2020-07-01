library(lubridate)
library(dplyr)
suppressMessages(library( fields))
#library(here) #Optional for loading files. 
#   If library not install, call it by here::here(). If installed, just here().

load(here::here("Documents","Baylor", "MoWater", "proj6", "MoWater-Goodyear", 
                "0_hand_in", "Dataset", "clean", "goodyearMoWater0.rda"))

#------------------------------------------------
#--- Set dates ---
#------------------------------------------------
#train change date: relevant for bin 2 and 4. 
#Before the change, bin 2 is train 3,
#This can be our starting date since we will only be ignoring 8 months of data.
trainChangeDate <- ymd( "2011-06-15")

#unstable periods
unstablePeriodStart <- ymd( "2014-04-01")
unstablePeriodEnd <- ymd( "2016-01-01") #rough est. according to Kate (stakeholder)

#Note: 2015-04-01 may be set to 2015-01-01 because the data doesn't look right.
#There's a spike in data around Jan 2015 that should be grouped with the next 
# performance period,hence this choice.

#set periods: most bins have different perfmance periods!
#bin1, 5, 6, 7
bin1567Period1End <- ymd( "2012-03-01")
bin1567Period2End <- ymd( "2015-04-01")
bin1567Period3End <- ymd( "2017-04-01")
bin1567Periods <- c(bin1567Period1End, bin1567Period2End, bin1567Period3End)

#periods for bin2
bin2Period1End <- ymd( "2015-04-01")
bin2Period2End <- ymd( "2017-04-01")
bin2Periods <- c(bin2Period1End, bin2Period2End)

#periods for bin3, bin4
bin34Period1End <- ymd( "2015-04-01")
bin34Period2End <- ymd( "2016-12-01")
bin34Periods <- c(bin34Period1End, bin34Period2End)

#Outliers to be removed, bad data points!
# maybe  use c() so we can add more dates later
removeDates <- ymd("2011-07-22")

#------------------------------------------------
#--- Data Cleaning ---
#------------------------------------------------

# Prep for matching size if needed! Some bins have an extra line of NA, row num 
# should be 438.
# Bins with extra rows: bin1, bin2, bin3, bin6
Bin1 <- Bin1[1:nrow(Bin1) - 1, ]
Bin2 <- Bin2[1:nrow(Bin1) - 1, ]
Bin3 <- Bin3[1:nrow(Bin1) - 1, ]
Bin6 <- Bin6[1:nrow(Bin1) - 1, ]

dfData <- dfData[!(is.na(dfData$date)), ] # also clean goodyear here

dfData <- as_tibble(goodyear)

#With data from only stable periods
dfDataSt <- dfData %>% 
    filter( date >= trainChangeDate & 
                date <= unstablePeriodStart |
                date >= unstablePeriodEnd) 

#Remove the confirmed outliers
#Can also use subset(): i.e. subset(df, B != )
dfDataSt <- filter(dfDataSt, date != removeDates)

#Identify and remove the row with very low selenium from brine
lowBrineSel <- dfDataSt %>% 
    filter(ID == "brine") %>% 
    slice(which.min(Selenium))
dfDataSt <- filter(dfDataSt, date != lowBrineSel$date)

highPho <- dfDataSt %>% 
    slice(which.max(Phosphorus))
dfDataSt <- filter(dfDataSt, date != highPho$date)


#lean data with mainly relevant variables 
dfDataStLn <- dfDataSt %>% 
    select(ID, date, Selenium, Arsenic, Nitrate, Phosphorus, 
           COD, DO.mg.L, pH, Temp..Celsius)

#Add new column TrainGroup base on the ID
dfDataStLn$TrainGroup <- ifelse(dfDataStLn$ID == "Bin1"| dfDataStLn$ID == "Bin5" |
                                    dfDataStLn$ID == "Bin7", "Train_1",
                                ifelse(dfDataStLn$ID == "Bin2" | 
                                           dfDataStLn$ID == "Bin6", "Train_2", 
                                       ifelse(dfDataStLn$ID == "Bin4", "Train_3", 
                                              ifelse(dfDataStLn$ID == "Bin3", 
                                                     "Train_4", "Brine"))))

#Add new column Vegetation base on the ID
dfDataStLn$Veg <- ifelse(dfDataStLn$ID == "Bin2", "VegType_B",
                         ifelse(dfDataStLn$ID == "Bin6" | 
                                    dfDataStLn$ID == "Bin7", "VegType_C", 
                                ifelse(dfDataStLn$ID == "brine", 
                                       "brine", "VegType_A")))

#Added new column for media type base on ID
dfDataStLn$MediaType <- ifelse(dfDataStLn$ID == "Bin7", "Soil",
                               ifelse(dfDataStLn$ID == "Bin1" | 
                                          dfDataStLn$ID == "Bin1", "MM", 
                                      ifelse(dfDataStLn$ID == "Bin4", "GW", 
                                             ifelse(dfDataStLn$ID == "brine", 
                                                    "brine", "PM"))))

#Data with Selenium focus, removing all NA rows from Selenium
dfDataSel <- dfDataStLn[!is.na(dfDataStLn$Selenium), ]

#last date for shortcut, the +1 is for adapting to use with functions due to the
#   comparison (< lastdate).
lastDate <- tail(dfDataSel$date, n = 1) + 1

save(dfDataSel, dfDataStLn, 
     file = here::here("Documents", "Baylor", "MoWater", "proj6", 
                       "MoWater-Goodyear", "clean", "cleanedObjects.rda"))

#------------------------------------------------
#--- Datasets with all NAs removed ---
#------------------------------------------------

dfC <- dfDataSel[which(complete.cases(dfDataSel)), ]

dfCLong <- dfDataSel
dfCLong$DO.mg.L <- NULL
dfCLong$Temp..Celsius <- NULL
dfCLong$pH <- NULL
dfCLong <- dfCLong[complete.cases(dfCLong), ]

save(dfC, dfCLong, 
     file = here::here("Documents", "Baylor", "MoWater", "proj6", 
                       "MoWater-Goodyear", "clean", "complete_cases.rda"))

#------------------------------------------------
#--- Make data object for the difference between influent and effluent ---
#------------------------------------------------

#NOTE: This function assumes dfDataSel exists!
#NOTE2: Requires the dataTarget having been initialized with the matching 
#       column names
AddMatchingInfoToDF <- function(dataTarget, curBinStr, prevBinStr, nameStr){
    #First get the respective dataframes. The 1:10 columns are the variables we
    #are interested in,can be expanded or subtract. Hardcoded because no reason 
    #to change here.
    tmpPrev<-dfDataSel[which(dfDataSel$ID == prevBinStr), 1:10]
    tmpCur<-dfDataSel[which(dfDataSel$ID == curBinStr), 1:10]
    
    #The following 2 steps force-match the size (row numbers) of the two
    #datasets to be the same
    tmpPrev <- tmpPrev[tmpPrev$date %in% tmpCur$date, ] 
    tmpCur <- tmpCur[tmpCur$date %in% tmpPrev$date, ]
    
    #The results of subtraction will be stored into a new dataframe.
    #Get a date column first, then add more columns to it next.
    dateTmp <- tmpPrev %>% 
        select(date)
    dfDiff1 <- tibble(ID = rep(nameStr, count(dateTmp)))
    #Adding the results as column to the new dataframe
    dfDiff1 <- add_column(dfDiff1, dateTmp)
    dfDiff1 <- add_column(dfDiff1, tmpCur[ , 3:ncol(tmpCur)] - 
                              tmpPrev[ , 3:ncol(tmpPrev)])
    
    dataTarget <-add_row(dataTarget, dfDiff1)    
    
    return (dataTarget) #return otherwise progress is lost in scope
}

#NOTE: this initialization step is required for using the function!
#add date
#Note: using dfData because other dataset may not have matching sizes
dateTmp <- dfDataSel %>% 
    filter(ID == "Bin1") %>% 
    select(date)

#Init base for brine to bin1
dfDiff <- tibble(ID = rep("B1-S", count(dateTmp)))
dfDiff <- add_column(dfDiff, dateTmp)

tmp1<-dfDataSel[which(dfDataSel$ID == "Bin1"), 2:10]
tmp2<-dfDataSel[which(dfDataSel$ID == "brine"), 2:10]
dfDiff <- add_column(dfDiff, tmp1[ , 2:ncol(tmp1)] - tmp2[ , 2:ncol(tmp2)])

#b1 to b5
dfDiff <- AddMatchingInfoToDF(dfDiff, "Bin5", "Bin1", "B5-B1")

#b5 to b7
dfDiff <- AddMatchingInfoToDF(dfDiff, "Bin7", "Bin5", "B7-B5")

#b2 - brine
dfDiff <- AddMatchingInfoToDF(dfDiff, "Bin2", "brine", "B2-S")

#b6 - b2
dfDiff <- AddMatchingInfoToDF(dfDiff, "Bin6", "Bin2", "B6-B2")

#b3 - brine
dfDiff <- AddMatchingInfoToDF(dfDiff, "Bin3", "brine", "B3-S")

#b4 - brine
dfDiff <- AddMatchingInfoToDF(dfDiff, "Bin4", "brine", "B4-S")

#renaming so that the data won't get overwritten in the next part.
dfDiff <- dfDiff %>% 
    rename(diff_ID = ID) %>% 
    rename(diff_Selenium = Selenium) %>% 
    rename(diff_Arsenic = Arsenic) %>% 
    rename(diff_Nitrate = Nitrate) %>% 
    rename(diff_Phosphorus = Phosphorus) %>% 
    rename(diff_COD = COD) %>% 
    rename(diff_DO.mg.L = DO.mg.L) %>% 
    rename(diff_pH = pH) %>% 
    rename(diff_Temp..Celsius = Temp..Celsius)

#--------------
#--- Second part of diff data, adding the effluent values ---

#TODO: refactor this into something like efficient and R-like if have time.
#WARNING: SLOW program! Don't run this 
#Description: Combine sourceDF's values into targetDf's, and return the targetDf. 
#   Add values from the sourceDF to targetDF.
#   Hard-coded some values, specifically, looking at date and the diff_IDs.
#   
#Return: the result dataframe.
#Assumptions: dfDataSel exists and has the target IDs looking for.
#               targetDF exists and has the matching date and diff_IDs.
#               NOTE: targetDF needs to have the columns that sourceDF has,
#                       but the sourceDF should ONLY has the columns we want to 
#                       add to targetDF. So if targetDF has date, col1, col2,
#                       but we only want to change col2, then sourceDF should
#                       only has date and col2. 
#Input: two valid daaframes,and a string that is value of the diff_ID rows.
#Example:
#       CombineDFFromSrcToTarget(dfDiff, tmp3, "B3-S")
#           This will add the values from tmp3 to dfDiff, matching with diff_ID 
#           that has B3-S values, and based on the shared date entries in both
#           files.

CombineDFFromSrcToTarget <- function(targetDF, sourceDF, diffIDTargetStr){
    for(jcol in colnames(sourceDF)){
        #The next logical check skips adding values on the date column.
        if(jcol != "date"){
            #Find the matching IDs
            for(irow in which(targetDF$diff_ID == diffIDTargetStr)){
                for(jrow in 1 : nrow(sourceDF)){
                    #Find the matching dates, then set targetDF value
                    if(targetDF[irow, "date"] == sourceDF[jrow, "date"]){
                        targetDF[irow, jcol] <- sourceDF[jrow, jcol] 
                    } 
                }   
            }
            message("\t\tfinished row ", irow) #status update due to slowness
        }
        message("\tfinished col ", jcol)
    }
    message("Done with ", diffIDTargetStr) #added this because it's so sloooowww
    return(targetDF)
}

#Description: Grab data from the binIDStr which is the effluent bin in the 
#               diffIDStr, and return that dataframe with the relevant columns.
#Return: dataframe with the relevant columns with matching IDs
#Assumptions: dfDataSel and dfDiff exists and has the values we need.
GetBinEffluentData <- function(binIDStr, diffIDStr){
    result <- dfDataSel[which(dfDataSel$ID == binIDStr), 2:ncol(dfDataSel)]
    matchDate <- dfDiff[which(dfDiff$diff_ID == diffIDStr), 2]
    result <- result[result$date %in% matchDate$date, ]
    return(result)
}

#Initialize dfDiff so that it'd work for the function.
#This is important! If this step fails then the entire thing fails.
#Basically, add all the new columns into dfDiff on the first run.

#This gets us the dataframe with the diff_columns and the effluent point
#columns
tmpMatched <- dfDiff %>% 
    filter(diff_ID == curDiffID) %>% 
    left_join(tmp, by = "date")

#This appends the new columns with the data onto dfDiff.
dfDiff <- left_join(dfDiff, tmpMatched)

tmp <- GetBinEffluentData("Bin1", "B1-S")
dfDiff <- CombineDFFromSrcToTarget(dfDiff, tmp, "B1-S")

#Rest/nap time!!!!!
#VERY SLOW program!!! Will require at least 5-10 mins depending on your machine.
for(curDiffID in unique(dfDiff$diff_ID)){
    if(curDiffID != "B1-S"){
        #Stupid case switches using if else chains -_-;;;
        if(curDiffID == "B2-S"){
            tmp <- GetBinEffluentData("Bin2", curDiffID)
            dfDiff <- CombineDFFromSrcToTarget(dfDiff, tmp, curDiffID)
        }
        if(curDiffID == "B3-S"){
            tmp <- GetBinEffluentData("Bin3", curDiffID)
            dfDiff <- CombineDFFromSrcToTarget(dfDiff, tmp, curDiffID)
        }
        if(curDiffID == "B4-S"){
            tmp <- GetBinEffluentData("Bin4", curDiffID)
            dfDiff <- CombineDFFromSrcToTarget(dfDiff, tmp, curDiffID)
        }
        if(curDiffID == "B5-B1"){
            tmp <- GetBinEffluentData("Bin5", curDiffID)
            dfDiff <- CombineDFFromSrcToTarget(dfDiff, tmp, curDiffID)
        }
        if(curDiffID == "B7-B5"){
            tmp <- GetBinEffluentData("Bin7", curDiffID)
            dfDiff <- CombineDFFromSrcToTarget(dfDiff, tmp, curDiffID)
        }
        if(curDiffID == "B6-B2"){
            tmp <- GetBinEffluentData("Bin6", curDiffID)
            dfDiff <- CombineDFFromSrcToTarget(dfDiff, tmp, curDiffID)
        }
        
    } 
}


#Initialize the upcoming entry. Cannot use merge or join because the IDs will
# not match even tho date does.

save(dfDataSel, dfDataStLn, dfDiff, 
     file = here::here("Documents", "Baylor", "MoWater", "proj6", 
                       "MoWater-Goodyear", "clean", "cleanedObjects.rda"))

#---------------------------------
#--- For 3D plot ---
#---------------------------------

dfT <- dfDataSel
dfT$Veg <- as.factor(dfT$Veg)
dfT$ID <- as.factor(dfT$ID)
dfT$MediaType <- as.factor(dfT$MediaType)
dfT$TrainGroup <- as.factor(dfT$TrainGroup)

dfD <- dfDiff
dfD$Veg <- as.factor(dfD$Veg)
dfD$diff_ID <- as.factor(dfD$diff_ID)
dfD$MediaType <- as.factor(dfD$MediaType)
dfD$TrainGroup <- as.factor(dfD$TrainGroup)

save(dfT, dfD, 
     file = here::here("Documents", "Baylor", "MoWater", "proj6", 
                       "MoWater-Goodyear", "clean", "cleanedFactors.rda"))

#---------------------------------
#--- t-test ---
#---------------------------------
#uses library( rstatix)

binsTTest <- dfDataSel %>% 
    t_test(Selenium ~ ID) %>%
    add_significance()
binsTTest

save(binsTTest, 
     file = here::here("Documents", "Baylor", "MoWater", "proj6", 
                        "MoWater-Goodyear", "clean", "ttestResults.rda"))

#------------------------------------

#--- Linear Regression And Modeling ----
#Performing lm on all variables/columns besides ID,date and Selenium, and store result
mods <- lapply(dfDataSel[, c(3, 5:ncol(dfDataSel))], 
               function(x) summary(lm(dfDataSel$Selenium ~ x)))
mods

#store the coeffcient of lm results
coefMat <- lapply( mods, coef)
coefMat

#store the r-squaredvalues
rssMat <- lapply( mods, "[[", "r.squared")
rssMat

save(mods, coefMat, rssMat, 
     file = here::here("Documents", "Baylor", "MoWater", "proj6", 
                       "MoWater-Goodyear", "clean", "lmResults.rda"))

#-----------------------------------

#Apply lm but skip diff_ID, date, diff_Selenium (respond var) and Selenium.
modsDiff <- lapply(dfDiff[ , c(5:10, 12:ncol(dfDiff))], 
                   function(x) summary(lm(dfDiff$diff_Selenium ~ x)))
modsDiff

#store the coeffcient of lm results
coefDiffMat <- lapply( modsDiff, coef)
coefDiffMat

#store the r-squaredvalues
rssDiffMat <- lapply( modsDiff, "[[", "r.squared")
rssDiffMat

save(modsDiff, coefDiffMat, rssDiffMat, 
     file = here::here("Documents", "Baylor", "MoWater", "proj6", 
                       "MoWater-Goodyear", "clean", "lmDiffResults.rda"))