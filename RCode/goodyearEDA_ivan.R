

#setwd("Baylor/MoWater/proj6/MoWater-Goodyear")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/goodyearMoWater0.rda")
#load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/goodyearMoWaterNew.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")
load("Baylor/MoWater/proj6/MoWater-Goodyear/clean/lmResults.rda")
ls()
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

#head(goodyear)

#------------------------------------------------------

#train change date: relevant for bin 2 and 4. 
#Before the change, bin 2 is train 3,
#This can be our starting date since we will only be ignoring 8 months of data.
trainChangeDate <- ymd( "2011-06-15")

#last date for shortcut, the +1 is for adapting to use with functions due to the
#   comparison (< lastdate).
lastDate <- tail(dfDataSel$date, n = 1) + 1

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

#----------------------------------------

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

save(dfDataSel, dfDataStLn, 
     file = "Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")

#------------------------------------------------

#--- Make data object for the difference between influent and effluent ---

#NOTE: This function assumes dfDataSel exists!
#NOTE2: Requires the dataTarget having been initialized with the matching 
#       column names
AddMatchingInfoToDF <- function(dataTarget, influentStr, effluentStr, nameStr){
    #First get the respective dataframes. The 1:10 columns are the variables we
    #are interested in,can be expanded or subtract. Hardcoded because no reason to
    #change here.
    tmp1<-dfDataSel[which(dfDataSel$ID == effluentStr), 1:10]
    tmp2<-dfDataSel[which(dfDataSel$ID == influentStr), 1:10]
    
    #The following 2 steps force-match the size (row numbers) of the two
    #datasets to be the same
    tmp1 <- tmp1[tmp1$date %in% tmp2$date, ] 
    tmp2 <- tmp2[tmp2$date %in% tmp1$date, ]
    
    #The results of subtraction willbe stored into a new dataframe.
    #Get a date column first, then add more columns to it next.
    dateTmp <- tmp1 %>% 
        select(date)
    dfDiff1 <- tibble(ID = rep(nameStr, count(dateTmp)))
    #Adding the results as column to the new dataframe
    dfDiff1 <- add_column(dfDiff1, dateTmp)
    dfDiff1 <- add_column(dfDiff1, tmp1[ , 3:ncol(tmp1)] - tmp2[ , 3:ncol(tmp2)])
    
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

#SHOW/NAP TIME!!!!!
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
     file = "Baylor/MoWater/proj6/MoWater-Goodyear/clean/cleanedObjects.rda")

#---------------------------------------------
#Set functions here

#@WARNING: Do NOT use this! this function doesn't work yet!
#Description: perform t-test on a column of each bin (including brine).
#Return: the object of t-test results
#Assumptions: the column name exists in the dataset.
#i.e.: VarTTestForIDs(dfDataSel, Selenium)
#       gives the t-test result of Selenium by the ID group for dfDataSel
VarTTestForIDs <- function(dataset, target)
{
    result <- dataset %>% 
        t_test(target ~ ID) %>%
        add_significance()
    return(result)
}


# Description: compare varX to varY for the targeted influent and effluent data, 
#   with specified start and stop period.
#   NOTE: These arguments should be strings: 
#       influent, effluent, interval_string, varX, varY
# Return: a ggplot. Note: still needs xlab, ylab and title.
#
# i.e.: GGPInVsOutXYOfPeriod(dfDataSel, "Bin1", "Bin5", bin1567Period1End, 
#                           bin1567Period2End, "date", "Selenium", "4 months")
#       will produce Bin1 as influent vs Bin5 as effluent for Selenium over time
#       for period bin1567Period1End~bin1567Period2End, in 4 months interval.
#

GGPInVSOutXYOfPeriod <- function(dataset, influent, effluent, startPeriod, 
                                     endPeriod, varX, varY, interval_string)
{
    resultPlot <- dataset %>% 
        filter(ID == influent | ID == effluent) %>% 
        filter(date >= startPeriod & date < endPeriod ) %>% 
        #aes_string because variable is not actual column factor term, but a name as string.
        ggplot(aes_string(x = varX, y = varY, group = "ID", color = "ID")) + 
        geom_point(size = 2) +  
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") + 
        scale_color_viridis_d()
    
    return(resultPlot) 
}

# Only looking at brine and train 1 (bin1, bin5, bin7) for after the train change.
GGPTrain1VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin1" | ID == "Bin5" | 
                   ID == "Bin7" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>% 
        #aes_string because variable is not actual column factor term, but a name as string.
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(alpha = 0.7, size = 3) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b%Y") +
        scale_color_brewer(palette = "Dark2") + 
        xlab("Date") + 
        ylab("Selenium Content (mg/L)") +
        labs(title= "Selenium content of Train 1 for period after train change") + 
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        geom_vline(xintercept = bin1567Period1End) + 
        geom_vline(xintercept = bin1567Period2End) + 
        geom_vline(xintercept = bin1567Period3End) + 
        annotate("text", label = "A", x = bin1567Period1End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "B", x = bin1567Period2End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "C", x = bin1567Period3End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "D", x = lastDate - 60, y = 0.2, 
                 size = 8, colour = "red")
    
    return(resultPlot) 
}

# train 2 doesn't show periods because bin 2 and 6 have different periods.
GGPTrain2VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin2" | ID == "Bin6" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>%
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(alpha = 0.7, size = 3) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_brewer(palette = "Dark2") +  
        xlab("Date") + 
        ylab("Selenium Content (mg/L)") +
        labs(title= "Selenium content of Train 2 for period after train change") + 
        geom_vline(xintercept = bin1567Period1End, color = "#D95F02", linetype = "dashed") + 
        geom_vline(xintercept = bin1567Period2End, color = "#222222" ) + 
        geom_vline(xintercept = bin1567Period3End, color = "#222222") + 
        annotate("text", label = "B6A", x = bin1567Period1End - 80, y = 0.2, 
                 size = 8, colour = "#D95F02") + 
        annotate("text", label = "B6B", x = bin1567Period2End - 80, y = 0.2, 
                 size = 8, colour = "#D95F02") + 
        annotate("text", label = "B6C", x = bin1567Period3End - 80, y = 0.2, 
                 size = 8, colour = "#D95F02") + 
        annotate("text", label = "B6D", x = lastDate, y = 0.2, 
                 size = 8, colour = "#D95F02") + 
        annotate("text", label = "B2A", x = bin2Period1End - 80, y = 0.145, 
                 size = 8, colour = "#1B9E77") + 
        annotate("text", label = "B2B", x = bin2Period2End - 80, y = 0.145, 
                 size = 8, colour = "#1B9E77") + 
        annotate("text", label = "B2C", x = lastDate, y = 0.145, 
                 size = 8, colour = "#1B9E77")
    
    return(resultPlot) 
}

GGPTrain3VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin4" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>%
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(alpha = 0.7, size = 3) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_brewer(palette = "Dark2") + 
        xlab("Date") + 
        ylab("Selenium Content (mg/L)") +
        labs(title= "Selenium content of Train 3 for period after train change") + 
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        geom_vline(xintercept = bin34Period1End) + 
        geom_vline(xintercept = bin34Period2End) + 
        annotate("text", label = "A", x = bin34Period1End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "B", x = bin34Period2End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "C", x = lastDate - 60, y = 0.2, 
                 size = 8, colour = "red")
    
    return(resultPlot) 
}

GGPTrain4VSBrine <- function(dataset, interval_string, chemical)
{
    resultPlot <- dataset %>% 
        filter(ID == "Bin3" | ID == "brine") %>% 
        filter(date >= trainChangeDate) %>%
        ggplot(aes_string(x = "date", y = chemical, group = "ID", color = "ID")) + 
        geom_point(alpha = 0.7, size = 3) +  
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        scale_x_date(date_breaks = interval_string, date_labels = "%b %Y") +
        scale_color_brewer(palette = "Dark2") + 
        xlab("Date") + 
        ylab("Selenium Content (mg/L)") +
        labs(title= "Selenium content of Train 4 for period after train change") + 
        geom_line(aes(y = 0.002), color = "red", alpha = 0.5) +
        geom_vline(xintercept = bin34Period1End) + 
        geom_vline(xintercept = bin34Period2End) + 
        annotate("text", label = "A", x = bin34Period1End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "B", x = bin34Period2End - 60, y = 0.2, 
                 size = 8, colour = "red") + 
        annotate("text", label = "C", x = lastDate - 60, y = 0.2, 
                 size = 8, colour = "red")
    
    return(resultPlot) 
}

#TODO: fix period 2 x labels overlapping each other. Maybe disable legends 
#   each plot and only show one on the right so that we have more drawing space.
# Or maybe draw at a larger scale, current the plot set at 1280 x 735. 

#Draw grid function
DrawGrid2x2 <- function(plot1, plot2, plot3, plot4)
{
    g1 <- ggplotGrob(plot1)
    g2 <- ggplotGrob(plot2)
    g3 <- ggplotGrob(plot3)
    g4 <- ggplotGrob(plot4)
    gr1 <- rbind(g1, g2)
    gr2 <- rbind(g3, g4)
    g <- cbind(gr1, gr2)
    grid.newpage()
    grid.draw(g)    
}

#--------------------------------

#Selenium contents for train 1 for different periods
plotT1vBrine <- GGPTrain1VSBrine(dfDataSel, "4 months", "Selenium")
plotT1vBrine 
ggsave(filename = "Images/Train 1 Selenium vs Brine.png", 
       plotT1vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')
#log ver
plotT1vBrineLog <- plotT1vBrine + 
    scale_y_continuous(trans = 'log10') + 
    labs(title= "Log of Selenium of Train 1 for period after train change")
plotT1vBrineLog
ggsave(filename = "Images/Train 1 Selenium vs Brine in log.png", 
       plotT1vBrineLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Train2
plotT2vBrine <- GGPTrain2VSBrine(dfDataSel, "4 months", "Selenium")
plotT2vBrine
ggsave(filename = "Images/Train 2 Selenium vs Brine.png", 
       plotT2vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')
#log ver
plotT2vBrineLog <- plotT2vBrine + 
    scale_y_continuous(trans = 'log10') + 
    labs(title= "Log of Selenium of Train 2 for period after train change")
plotT2vBrineLog
ggsave(filename = "Images/Train 2 Selenium vs Brine in log.png", 
       plotT2vBrineLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Train3
plotT3vBrine <- GGPTrain3VSBrine(dfDataSel, "4 months", "Selenium")
plotT3vBrine
ggsave(filename = "Images/Train 3 Selenium vs Brine.png", 
       plotT3vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')
#log ver
plotT3vBrineLog <- plotT3vBrine + 
    scale_y_continuous(trans = 'log10') + 
    labs(title= "Log of Selenium of Train 3 for period after train change")
plotT3vBrineLog
ggsave(filename = "Images/Train 3 Selenium vs Brine in log.png", 
       plotT3vBrineLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Train 4
plotT4vBrine <- GGPTrain4VSBrine(dfDataSel, "4 months", "Selenium")
plotT4vBrine
ggsave(filename = "Images/Train 4 Selenium vs Brine.png", 
       plotT4vBrine,
       width = 42.3, height = 23.15, units = "cm", device='png')

#log ver
plotT4vBrineLog <- plotT4vBrine + 
    scale_y_continuous(trans = 'log10') + 
    labs(title= "Log of Selenium of Train 4 for period after train change")
plotT4vBrineLog
ggsave(filename = "Images/Train 4 Selenium vs Brine in log.png", 
       plotT4vBrineLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#---------------------------------------------------
#Net Selenium linear regression check

#cleaning inflow and outflow
dfDataSelFlow <- dfDataSel[!is.na(dfDataSel$Inflow), ]
dfDataSelFlow <- dfDataSelFlow[!is.na(dfDataSelFlow$Outflow), ]
#make the result of outflow-inflow as a new column in the data
dfDataSelFlow <- add_column(dfDataSelFlow, 
                            netflow = (dfDataSelFlow$Outflow - 
                                           dfDataSelFlow$Inflow) ) 
# glimpse(dfDataFlow)

#Regression analysis of Netflow vs Selenium for Train 1
#can also use : scale_color_carto_d("Effluent Selenium") 
dfDataSelFlow %>% 
    filter(ID == "Bin1" | ID == "Bin5" | ID == "Bin7") %>% 
    ggplot(aes(Selenium, netflow)) +
    geom_point(size = 2, alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + # formula always look at the variable in terms of x
    scale_color_brewer(palette = "Dark2") + 
    xlab("selenium") +  
    ylab("netflow") +
    labs(title= "Selenium vs netflow RA for Train 1")


# regression analysis of Netflow vs Selenium for Train 4
dfDataSelFlow %>% 
    filter(ID == "Bin3") %>% 
    ggplot(aes(Selenium, netflow)) +
    geom_point(alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_brewer(palette = "Dark2") + 
    xlab("selenium") + 
    ylab("netflow") +
    labs(title= "Selenium vs netflow RA for Train 4")

#----------------------------------------------

#--- Boxplot ---

# sample code for reordering boxplot
# data$names <- factor(data$names , levels=c("A", "D", "C", "B"))
# 
# #The plot is now ordered !
# boxplot(data$value ~ data$names , col=rgb(0.3,0.5,0.4,0.6) , ylab="value" , 
#        xlab="names in desired order")


#boxplot on bons to see which bins to use - 2 tone color
boxSel2t <- dfDataSel %>% 
    filter(year(date) <= 2014 | year(date) >= 2015) %>%
    ggplot(aes(x = ID , y = Selenium, group = ID, color = ID)) +
    geom_boxplot(fill = "grey") +
    scale_color_manual(values = c(rep("black", 7), "red3")) + 
    ylab("Selenium (mg/L)") +
    labs(title = "Selenium Boxplots per Effluent") + 
    theme(legend.position = "none", axis.text=element_text(size=14))

boxSel2t

ggsave(filename = "Images/Selenium_Boxplot_two_tone.png", 
       boxSel2t,
       width = 42.3, height = 23.15, units = "cm", device='png')

#multiple color of boxplot base on train
boxSelTC <- dfDataSel %>% 
    filter(year(date) <= 2014 | year(date) >= 2015) %>%
    ggplot(aes(x = ID , y = Selenium, group = ID, color = ID)) +
    geom_boxplot(fill = c("#D95F02", "darkgreen", "royalblue3", 
                          "red4", "#D95F02", "darkgreen", 
                          "#D95F02", "black")) +
    scale_color_manual(values = c(rep("black", 7), "darkgrey")) + 
    ylab("Selenium (mg/L)") +
    labs(title= "Boxplot: Selenium by Effluent") +
    theme(legend.position = "none", axis.text=element_text(size=14))

boxSelTC

ggsave(filename = "Images/Selenium_Boxplot_train_color.png", 
       boxSelTC,
       width = 42.3, height = 23.15, units = "cm", device='png')

#log ver
boxSelTCLog <- boxSelTC + scale_y_continuous(trans = 'log10') + 
    labs(title= "Boxplot: Log of Selenium by Effluent")
boxSelTCLog
ggsave(filename = "Images/Selenium_Boxplot_train_color_log.png", 
       boxSelTCLog,
       width = 42.3, height = 23.15, units = "cm", device='png')

#different boxplot grouped by train type
dfTest <- dfDataSel
dfTest$ID <- factor(dfTest$ID , levels=c("Bin1", "Bin5", "Bin7", "Bin2", 
                                         "Bin6", "Bin4", "Bin3", "brine"))

boxSelTCGroup <- dfTest %>%
    ggplot(aes(x = ID, y = Selenium)) +
    geom_boxplot(fill = c("#D95F02", "#D95F02", "#D95F02", 
                                  "darkgreen", "darkgreen", "royalblue3", 
                                  "red3", "black")) +
    xlab("Train Type") +
    ylab("Selenium Content (mg/L)") +
    labs(title= "Boxplot: Selenium by Effluent by Train") +
    theme(legend.position = "none", axis.text=element_text(size=14))

boxSelTCGroup

ggsave(filename = "Images/Selenium_Boxplot_train_color_group.png", 
       boxSelTCGroup,
       width = 42.3, height = 23.15, units = "cm", device='png')

#log ver
boxSelTCGroupLog <- boxSelTCGroup + scale_y_continuous(trans = 'log10') + 
    labs(title= "Boxplot: Log of Selenium by Effluent by train type")
boxSelTCGroupLog
ggsave(filename = "Images/Selenium_Boxplot_train_color_group_log.png", 
       boxSelTCGroupLog,
       width = 42.3, height = 23.15, units = "cm", device='png')



#-----------------------------------------------

#Exploring linear regression with Selenium vs COD

#All bins facet 
plotSelvCODFac <- dfDataSel %>% 
    filter(ID != "brine") %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 0.75, aes(color = ID), size = 3) +
    geom_smooth(formula = y~x, method = "lm") + 
    facet_grid(.~ID) +
    scale_color_brewer(palette = "Dark2") + 
    xlab("COD mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Log of Selenium vs COD lin. reg. of Each Bins All Periods") + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10")

plotSelvCODFac
ggsave(filename = "Images/Selenium vs COD linear regression Each Bins Period All.png", 
       plotSelvCODFac,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Carbon dosing period only.
#Note: not enough data points so it's kinda inconclusive.
plotSelvCODFacLP <- dfDataSel %>% 
    filter(ID != "brine") %>% 
    filter(date >= bin1567Period3End) %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 0.75, aes(color = ID), size = 3) +
    geom_smooth(formula = y~x, method = "lm") + 
    facet_grid(.~ID) +
    scale_color_brewer(palette = "Dark2") + 
    xlab("COD mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Log of Selenium vs COD lin. reg. of Each Bins Carbon Dosing Periods") + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10")

plotSelvCODFacLP
ggsave(filename = "Images/Selenium vs COD linear regression Each Bins Carbon Dosing Period.png", 
       plotSelvCODFacLP,
       width = 42.3, height = 23.15, units = "cm", device='png')

#All Bins filtered out outlier, exclude brine!
plotSelvCODBA <- dfDataSel %>% 
    filter(ID != "brine") %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 0.75, aes(color = ID), size = 3) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_brewer(palette = "Dark2") + 
    xlab("COD mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Log of Selenium vs COD lin. reg. of All Bins All Periods") + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10")
plotSelvCODBA
ggsave(filename = "Images/Selenium vs COD linear regression All Bins Period All.png", 
       plotSelvCODBA,
       width = 42.3, height = 23.15, units = "cm", device='png')

#All Bins last period
plotSelvCODBAPL <- dfDataSel %>% 
    filter(ID != "brine") %>% 
    filter(date >= bin1567Period3End) %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 0.75, aes(color = ID), size = 3) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_brewer(palette = "Dark2") + 
    xlab("COD mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Log of Selenium vs COD lin. reg. of All Bins Carbon Dosing Period") + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10") 
plotSelvCODBAPL
ggsave(filename = "Images/Selenium vs COD linear regression All Bins Carbon Dosing Period.png", 
       plotSelvCODBAPL,
       width = 42.3, height = 23.15, units = "cm", device='png')

#All bins all periods vs last period only
plotSelvCODBAPvP <- ggarrange(plotSelvCODBA, plotSelvCODBAPL, nrow = 2, 
                             common.legend = TRUE)
plotSelvCODBAPvP
ggsave(filename = "Images/Selenium vs COD linear regression All Bins All Period vs Carbon Dosing.png", 
       plotSelvCODBAPvP,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Bin1 All periods
plotSelvCODB1 <- dfDataSel %>% 
    filter(ID == "Bin1") %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 0.65, size = 2.5, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    xlab("COD mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD lin. reg. of Bin1 All Periods") + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10")
plotSelvCODB1
ggsave(filename = "Images/Selenium vs COD linear regression Bin1 Period All.png", 
       plotSelvCODB1,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Bin2 All periods
plotSelvCODB2 <- dfDataSel %>% 
    filter(ID == "Bin2") %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 0.65, size = 2.5, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_brewer(palette = "Dark2") +
    xlab("COD mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD lin. reg. of Bin2 All Periods") + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10")
plotSelvCODB2
ggsave(filename = "Images/Selenium vs COD linear regression Bin2 Period All.png", 
       plotSelvCODB2,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Bin2 unscale
plotSelvCODB2un <- dfDataSel %>% 
    filter(ID == "Bin2") %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 0.65, size = 2.5, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_brewer(palette = "Dark2") +
    xlab("COD mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD lin. reg. of Bin2 All Periods non-scale")
plotSelvCODB2un
ggsave(filename = "Images/Selenium vs COD linear regression Bin2 Period All non-scale.png", 
       plotSelvCODB2un,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Bin1 vs Bin2 all periods
plotSelvCODB1vB2AP <- ggarrange(plotSelvCODB1, plotSelvCODB2, nrow = 2, 
                              common.legend = FALSE)
plotSelvCODB1vB2AP
ggsave(filename = "Images/Selenium vs COD linear regression Bin1 vs Bin2 All Periods.png", 
       plotSelvCODB1vB2AP,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Bin1 period D
plotSelvCODB1P4 <- dfDataSel %>% 
    filter(ID == "Bin1" & date >= bin1567Period3End ) %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 1, size = 3.5, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    xlab("COD mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD lin. reg. of Bin1 Carbon Dosing Period") + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10")
plotSelvCODB1P4
ggsave(filename = "Images/Selenium vs COD linear regression Bin1 Carbon Dosing Period.png", 
       plotSelvCODB1P4,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Bin2 period C
plotSelvCODB2P3 <- dfDataSel %>% 
    filter(ID == "Bin2") %>% 
    filter(date >= bin2Period2End) %>% 
    ggplot(aes(COD, Selenium))+ 
    geom_point(alpha = 1, size = 3.5, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_brewer(palette = "Dark2") +
    xlab("COD mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD lin. reg. for Bin2 Carbon Dosing Period") + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10")
plotSelvCODB2P3
ggsave(filename = "Images/Selenium vs COD linear regression Bin2 Carbon Dosing Period.png", 
       plotSelvCODB2P3,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Selenium vs COD for Bin1 and Bin2 during carbon dosing
plotSelvCODB1B2 <- ggarrange(plotSelvCODB1P4, plotSelvCODB2P3, nrow=2, 
                             common.legend = FALSE)
plotSelvCODB1B2
ggsave(filename = "Images/Selenium vs COD linear regression Bin 1 vs Bin2 Carbon Dosing.png", 
       plotSelvCODB1B2,
       width = 42.3, height = 23.15, units = "cm", device='png')

#---------------------------------------
#Maybe do more DO?
plotSelvDOFac <- dfDataSel %>% 
    filter(ID != "brine") %>% 
    ggplot(aes(DO.mg.L, Selenium)) + 
    geom_point(alpha = 0.75, aes(color = ID), size = 3) +
    geom_smooth(formula = y~x, method = "lm") + 
    facet_grid(.~ID) +
    scale_color_brewer(palette = "Dark2") + 
    xlab("DO mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Log of Selenium vs DO lin. reg. of Each Bins Periods All") + 
    scale_x_continuous(trans = "log10") + 
    scale_y_continuous(trans = "log10")

plotSelvDOFac
ggsave(filename = "Images/Selenium vs DO linear regression Each Bins Periods All.png", 
       plotSelvDOFac,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Only cardon dosing period
#... Not enough data! As in, no data points in the last period!


plotSelvCODFacLP
ggsave(filename = "Images/Selenium vs COD linear regression Each Bins Carbon Dosing Period.png", 
       plotSelvCODFacLP,
       width = 42.3, height = 23.15, units = "cm", device='png')



#Focus look at Bin2 DO on last period when carbon dosing happens
plotSelvDOB2P3 <- dfDataSel %>% 
    filter(ID == "Bin2" & date >= bin2Period2End) %>% 
    ggplot(aes(DO.mg.L, Selenium))+ 
    geom_point(alpha = 1) +
    geom_smooth(formula = y~x, method = "lm") + 
    xlab("DO mg/L") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs DO linear regression for Bin2 Period C")
plotSelvDOB2P3
#conclusion: not enough data to back up the claims of higher DO leads to more 
#selenium reduction

#Bin6
plotSelvCODB6P4 <- dfDataSel %>% 
    filter(ID == "Bin6") %>% 
    filter(date >= bin1567Period3End) %>% 
    ggplot(aes(COD, Selenium))+ 
    geom_point(alpha = 1) +
    geom_smooth(formula = y~x, method = "lm") + 
    xlab("COD") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD linear regression for Bin6 Period D")
plotSelvCODB6P4
ggsave(filename = "Images/Selenium vs COD linear regression Bin6 Period D.png", 
       plotSelvCODB6P4,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Bin3
plotSelvCODB1P1 <- dfDataSel %>% 
    filter(date <= bin1567Period1End & ID == "Bin3") %>% 
    ggplot(aes(COD, Selenium)) + 
    geom_point(alpha = 1, aes(color = ID)) +
    geom_smooth(formula = y~x, method = "lm") + 
    scale_color_brewer(palette = "Dark2") + 
    xlab("COD") +
    ylab("selenium mg/L") + 
    labs(title= "Selenium vs COD linear regression of Bin1 Period A")
plotSelvCODB1P1
ggsave(filename = "Images/Selenium vs COD linear regression Bin1 Period A.png", 
       plotSelvCODB1P1,
       width = 42.3, height = 23.15, units = "cm", device='png')

#----------------------------------------------

#Selenium contents for individual bins
#Focus: bin 3
#Reason: bin3 is our baseline
#Period 1

#-----------------------------------------------
#--- t-test ---
#uses library( rstatix)

binsTTest <- dfDataSel %>% 
    t_test(Selenium ~ ID) %>%
    add_significance()
binsTTest

save(binsTTest, file = "clean/ttestResults.rda")
#-----------------------------------------------
#density
plotSelDen <- dfDataSel %>% 
    ggplot(aes(Selenium, color =ID)) +
    geom_density(size = 1.5) +
    scale_color_brewer(palette = "Dark2") + 
    labs(title = "Selenium level distribution, cutoff level <= 0.15") + 
    theme(axis.text=element_text(size=14))
plotSelDen
ggsave(filename = "Images/Selenium Level Distribution with cutoff.png", 
       plotSelDen,
       width = 42.3, height = 23.15, units = "cm", device='png')

#--------------------------------------

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

save(mods, coefMat, rssMat, file = "clean/lmResults.rda")

#-------------------------------------

#--- graphs of time based facet models ---

#This shows if the correlations changes base on vegetation type or train type.

GGPSelVSVarByYear<- function(dataset, target){
    result <- dataset %>% 
            filter(ID != "brine") %>% 
            ggplot(aes_string(target, "Selenium")) +
            geom_point(alpha = 1, aes(color = month(date, label = TRUE))) + # plot factor by month
            facet_wrap(~year(date), 4) + # use wrap when faceting by one variable
            scale_color_manual(values = plasma(15)) + #value is 15 to avoid using the lighter colors
            theme(
                panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                                size = 2, linetype = "solid"),
                panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                colour = "white"), 
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                colour = "white")
            ) + 
            ylab("Selenium mg/L")
    
    return(result)
}

#nitrate is key
plotSelvNit <- GGPSelVSVarByYear(dfDataSel, "Nitrate")
plotSelvNit + xlab("Nitrate mg/L") +
    labs(title = "Nitrate vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs Nitrate.png", 
       plotSelvNit,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Arsenic
plotSelvArs <- GGPSelVSVarByYear(dfDataSel, "Arsenic")
plotSelvArs + xlab("Arsenic mg/L") +
    labs(title = "Arsenic vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs Arsenic.png", 
       plotSelvArs,
       width = 42.3, height = 23.15, units = "cm", device='png')

#COD
plotSelvCOD <- GGPSelVSVarByYear(dfDataSel, "COD")
plotSelvCOD + xlab("COD mg/L") +
    labs(title = "COD vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs COD.png", 
       plotSelvCOD,
       width = 42.3, height = 23.15, units = "cm", device='png')
#pH
plotSelvpH <- GGPSelVSVarByYear(dfDataSel, "pH")
plotSelvpH + xlab("pH") +
    labs(title = "pH vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs pH.png", 
       plotSelvpH,
       width = 42.3, height = 23.15, units = "cm", device='png')

#DO
plotSelvDOmgl <- GGPSelVSVarByYear(dfDataSel, "DO.mg.L")
plotSelvDOmgl + xlab("DO mg/L") +
    labs(title = "DO vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs DO.png", 
       plotSelvDOmgl,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Temp
plotSelvT <- GGPSelVSVarByYear(dfDataSel, "Temp..Celsius")
plotSelvT + xlab("Temp..Celsius") +
    labs(title = "Temp vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs Temp.png", 
       plotSelvT,
       width = 42.3, height = 23.15, units = "cm", device='png')

#TODO: clean out outlier in 2012
#Not much on phosphorus
plotSelvPho <- GGPSelVSVarByYear(dfDataSel, "Phosphorus")
plotSelvPho + xlab("Phosphorus mg/L") +
    labs(title = "Phosphorus vs Selenium")

#Not much on Copper
plotSelvCop <- GGPSelVSVarByYear(dfDataSel, "Copper")
plotSelvCop + xlab("Copper mg/L") +
    labs(title = "Copper vs Selenium")

#TDS - Worst predictor
plotSelvTDS <- GGPSelVSVarByYear(dfDataSel, "TDS")
plotSelvTDS + xlab("TDS mg/L") +
    labs(title = "TDS vs Selenium")

#---Below are variables we found to be low correlation

#Sulfate
plotSelvSulfa <- GGPSelVSVarByYear(dfDataSt, "Sulfate")
plotSelvSulfa + xlab("Sulfate mg/L") +
    labs(title = "Sulfate vs Selenium") + 
    scale_y_continuous(trans = "log10")

#Chloride
plotSelvChlor <- GGPSelVSVarByYear(dfDataSt, "Chloride")
plotSelvChlor + xlab("Chloride mg/L") +
    labs(title = "Chloride vs Selenium") + 
    scale_y_continuous(trans = "log10")

#Chromium
plotSelvChrom <- GGPSelVSVarByYear(dfDataSt, "Chromium")
plotSelvChrom + xlab("Chromium mg/L") +
    labs(title = "Chromium vs Selenium")

#Zinc
plotSelvZinc <- GGPSelVSVarByYear(dfDataSt, "Zinc")
plotSelvZinc + xlab("Zinc mg/L") +
    labs(title = "Zinc vs Selenium")

#Nitrite
plotSelvNitri <- GGPSelVSVarByYear(dfDataSt, "Nitrite")
plotSelvNitri + xlab("Nitrite mg/L") +
    labs(title = "Nitrite vs Selenium")

#Boron
plotSelvBor <- GGPSelVSVarByYear(dfDataSt, "Boron")
plotSelvBor + xlab("Boron mg/L") +
    labs(title = "Boron vs Selenium")

#Thallium
plotSelvTha <- GGPSelVSVarByYear(dfDataSt, "Thallium")
plotSelvTha + xlab("Thallium mg/L") +
    labs(title = "Thallium vs Selenium")

#Sulfide
plotSelvSulfi <- GGPSelVSVarByYear(dfDataSt, "Sulfide")
plotSelvSulfi + xlab("Sulfide mg/L") +
    labs(title = "Sulfide vs Selenium")

#Conductivity.S.m
plotSelvCon <- GGPSelVSVarByYear(dfDataSt, "Conductivity.S.m")
plotSelvCon + xlab("Conductivity.S.m") +
    labs(title = "Conductivity.S.m vs Selenium")

#ORP
plotSelvORP <- GGPSelVSVarByYear(dfDataSt, "ORP")
plotSelvORP + xlab("ORP") +
    labs(title = "ORP vs Selenium")

#--------------------------

#--- veg type comparision ---

GGPSelVSVarByTrainVeg<- function(dataset, varX, varY){
    result <- dataset %>% 
        filter(ID != "brine") %>% 
        ggplot(aes_string(varX, varY)) +
        geom_point(alpha = 1, aes(color = TrainGroup)) +
        facet_wrap(~Veg, 4) + 
        scale_color_brewer(palette = "Dark2") +
        theme(
            panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                            size = 2, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white")
        ) + 
        ylab("Log of Selenium mg/L") + 
        scale_y_continuous(trans = "log10")
    
    return(result)
}

#Comparison base on Veg Type by train
#nitrate
plotTrainVegSelvNit <- GGPSelVSVarByTrainVeg(dfDataSel, "Nitrate", "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs Nitrate base on Train")
plotTrainVegSelvNit
ggsave(filename = "Images/Veg Type Analysis of Selenium vs Nitrate base on Train.png", 
       plotTrainVegSelvNit,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Arsenic
plotTrainVegSelvAr <- GGPSelVSVarByTrainVeg(dfDataSel, "Arsenic", "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs Arsenic base on Train")
plotTrainVegSelvAr
ggsave(filename = "Images/Veg Type Analysis of Selenium vs Arsenic base on Train.png", 
       plotTrainVegSelvAr,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Look at Temp base on VegType
plotTrainVegSelvT <- GGPSelVSVarByTrainVeg(dfDataSel, "Temp..Celsius", "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs Temp base on Train")
plotTrainVegSelvT
ggsave(filename = "Images/Veg Type Analysis of Selenium vs Temp base on Train.png", 
       plotTrainVegSelvT,
       width = 42.3, height = 23.15, units = "cm", device='png')

#DO
plotTrainVegSelvDO <- GGPSelVSVarByTrainVeg(dfDataSel, "DO.mg.L", "Selenium") +
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs DO base on Train")
plotTrainVegSelvDO
ggsave(filename = "Images/Veg Type Analysis of Selenium vs DO base on Train.png", 
       plotTrainVegSelvDO,
       width = 42.3, height = 23.15, units = "cm", device='png')

#pH
plotTrainVegSelvpH <- GGPSelVSVarByTrainVeg(dfDataSel, "pH", "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs pH base on Train")
plotTrainVegSelvpH
ggsave(filename = "Images/Veg Type Analysis of Selenium vs pH base on Train.png", 
       plotTrainVegSelvpH,
       width = 42.3, height = 23.15, units = "cm", device='png')

#COD
plotTrainVegSelvCOD <- GGPSelVSVarByTrainVeg(dfDataSel, "COD", "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs COD base on Train")
plotTrainVegSelvCOD
ggsave(filename = "Images/Veg Type Analysis of Selenium vs COD base on Train.png", 
       plotTrainVegSelvCOD,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Phosphorous
plotTrainVegSelvPho <- GGPSelVSVarByTrainVeg(dfDataSel, "Phosphorus", "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs Phosphorus base on Train")
plotTrainVegSelvPho
ggsave(filename = "Images/Veg Type Analysis of Selenium vs Phosphorus base on Train.png", 
       plotTrainVegSelvPho,
       width = 42.3, height = 23.15, units = "cm", device='png')

#--- Comparison base on veg type and bin

#By Bin type, from 2016 (stable periods after management change)
GGPSelVSVarByBinVegStable<- function(dataset, varX, varY){
    result <- dataset %>% 
        filter(ID != "brine") %>% 
        filter(date >= unstablePeriodEnd) %>% 
        ggplot(aes_string(varX, varY)) +
        geom_point(alpha = 1, aes(color = ID)) +
        facet_wrap(~Veg, 4) + 
        scale_color_brewer(palette = "Dark2") +
        theme(
            panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                            size = 2, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white")
        ) + 
        ylab("Log of Selenium mg/L") + 
        scale_y_continuous(trans = "log10")
    
    return(result)
}

#COD Stable by Bin
plotBinVegStabSelvCOD <- GGPSelVSVarByBinVegStable(dfDataSel, "COD", "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs COD base on Bin from 2016")
plotBinVegStabSelvCOD
ggsave(filename = "Images/Veg Type Analysis of Selenium vs COD base on Bin 2016.png", 
       plotBinVegStabSelvCOD,
       width = 42.3, height = 23.15, units = "cm", device='png')

#pH
plotBinVegStabSelvpH <- GGPSelVSVarByBinVegStable(dfDataSel, "pH", "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs pH base on Bin from 2016")
plotBinVegStabSelvpH
ggsave(filename = "Images/Veg Type Analysis of Selenium vs pH base on Bin 2016.png", 
       plotBinVegStabSelvpH,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Temp
plotBinVegStabSelvT <- GGPSelVSVarByBinVegStable(dfDataSel, "Temp..Celsius", 
                                                 "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs Temp base on Bin from 2016")
plotBinVegStabSelvT
ggsave(filename = "Images/Veg Type Analysis of Selenium vs Temp base on Bin 2016.png", 
       plotBinVegStabSelvT,
       width = 42.3, height = 23.15, units = "cm", device='png')

#DO
plotBinVegStabSelvDO <- GGPSelVSVarByBinVegStable(dfDataSel, "DO.mg.L", 
                                                 "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs DO base on Bin from 2016")
plotBinVegStabSelvDO
ggsave(filename = "Images/Veg Type Analysis of Selenium vs DO base on Bin 2016.png", 
       plotBinVegStabSelvDO,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Nitrate
plotBinVegStabSelvNit <- GGPSelVSVarByBinVegStable(dfDataSel, "Nitrate", 
                                                  "Selenium") + 
    geom_smooth(formula = y~x, method = "lm") + 
    labs(title = "Veg Type Analysis of Selenium vs Nitrate base on Bin from 2016")
plotBinVegStabSelvNit
ggsave(filename = "Images/Veg Type Analysis of Selenium vs Nitrate base on Bin 2016.png", 
       plotBinVegStabSelvNit,
       width = 42.3, height = 23.15, units = "cm", device='png')

#--- vs plots
#Nitrate
plotNitvs <- ggarrange(plotTrainVegSelvNit, plotBinVegStabSelvNit, ncol = 2, 
                              common.legend = FALSE)
plotNitvs
ggsave(filename = "Images/Veg Type compare Nitrate.png", 
       plotNitvs,
       width = 42.3, height = 23.15, units = "cm", device='png')

#COD
plotCODvs <- ggarrange(plotTrainVegSelvCOD, plotBinVegStabSelvCOD, ncol = 2, 
                       common.legend = FALSE)
plotCODvs
ggsave(filename = "Images/Veg Type compare COD.png", 
       plotCODvs,
       width = 42.3, height = 23.15, units = "cm", device='png')

#DO
plotDOvs <- ggarrange(plotTrainVegSelvDO, plotBinVegStabSelvDO, ncol = 2, 
                       common.legend = FALSE)
plotDOvs
ggsave(filename = "Images/Veg Type compare DO.png", 
       plotDOvs,
       width = 42.3, height = 23.15, units = "cm", device='png')

#Temp
plotTvs <- ggarrange(plotTrainVegSelvT, plotBinVegStabSelvT, ncol = 2, 
                       common.legend = FALSE)
plotTvs
ggsave(filename = "Images/Veg Type compare Temp.png", 
       plotTvs,
       width = 42.3, height = 23.15, units = "cm", device='png')

#pH
plotpHvs <- ggarrange(plotTrainVegSelvpH, plotBinVegStabSelvpH, ncol = 2, 
                     common.legend = FALSE)
plotpHvs
ggsave(filename = "Images/Veg Type compare pH.png", 
       plotpHvs,
       width = 42.3, height = 23.15, units = "cm", device='png')

#--------------------------

#--- get the lower pH values, lower 25% percent ---

temp <- dfDataSel %>% 
    filter(quantile(pH, 0.25, na.rm = TRUE)>pH)
glimpse(temp)
range(dfDataSel$pH, na.rm = TRUE)

#--------------------------

GGPVarDiffByVeg<- function(dataset, varX, varY){
    result <- dataset %>% 
        ggplot(aes_string(varX, varY)) +
        geom_point(alpha = 1, aes(color = diff_ID)) +
        facet_wrap(~Veg, 4) + 
        scale_color_brewer(palette = "Dark2") +
        theme(
            panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                            size = 2, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white")
        )
    
    return(result)
}

plotSelNitDiff <- GGPVarDiffByVeg(dfDiff, "diff_Nitrate", "diff_Selenium")
plotSelNitDiff

#diff_COD vs diff_Selenium
plotSelCODDiff <- GGPVarDiffByVeg(dfDiff, "diff_COD", "diff_Selenium")
plotSelCODDiff
#Not much correlation

#diff_Temp diff vs diff_Selenium
plotSelTDDiff <- GGPVarDiffByVeg(dfDiff, "diff_Temp..Celsius", "diff_Selenium")
plotSelTDDiff
#no correlation?

#Temp diff vs diff_Selenium
plotSelTDiff <- GGPVarDiffByVeg(dfDiff, "Temp..Celsius", "diff_Selenium")
plotSelTDiff
#Still no correlation

#DO vs diff_Selenium
plotSelDODiff <- GGPVarDiffByVeg(dfDiff, "DO.mg.L", "diff_Selenium")
plotSelDODiff
#No correlation

#------------------------------------------------

#--- Clustering ---

#Template/test run code
#dfDiff %>% lm(Nitrate ~ diff_Selenium, data = .)

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

save(modsDiff, coefDiffMat, rssDiffMat, file = "clean/lmDiffResults.rda")

#Multivariate testing
fit <- dfDataSel %>% 
    lm( Selenium ~ Nitrate + COD, data = .)
summary(fit)


#--------------------------------------


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

