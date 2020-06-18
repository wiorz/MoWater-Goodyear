

setwd("Baylor/MoWater/proj6/MoWater-Goodyear")
load("clean/goodyearMoWater0.rda" )
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

#lean data with mainly relevant variables 
dfDataStLn <- dfDataSt %>% 
                select(ID, date, TDS, Selenium, Arsenic, Nitrate, Phosphorus, 
                       COD, DOC, DO.mg.L, pH, Temp..Celsius, Inflow, Outflow)

#Data with Selenium focus, removing all NA rows from Selenium
dfDataSel <- dfDataStLn[!is.na(dfDataStLn$Selenium), ]

save(dfDataSel, dfDataStLn, file = "clean/cleanedObjects.rda")


#---------------------------------------------
#Set functions here

#NOTE: Do NOT use this! this function doesn't work yet!
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

#reordering boxplot
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
#t-test

binsTTest <- dfDataSel %>% 
    t_test(Selenium ~ ID) %>%
    add_significance()
binsTTest


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
#Note: a visual look into 

GGPSelVSVarByYear<- function(dataset, target){
    result <- dataset %>% 
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

plotSelvCOD <- GGPSelVSVarByYear(dfDataSel, "COD")
plotSelvCOD + xlab("COD mg/L") +
    labs(title = "COD vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs COD.png", 
       plotSelvCOD,
       width = 42.3, height = 23.15, units = "cm", device='png')

plotSelvpH <- GGPSelVSVarByYear(dfDataSel, "pH")
plotSelvpH + xlab("pH") +
    labs(title = "pH vs Selenium")
ggsave(filename = "Images/Yearly Comparison Selenium vs pH.png", 
       plotSelvpH,
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

plotSelvDOmgl <- GGPSelVSVarByYear(dfDataSel, "DO.mg.L")
plotSelvDOmgl + xlab("DO mg/L") +
    labs(title = "DO vs Selenium")

#TDS - Worst predictor
plotSelvTDS <- GGPSelVSVarByYear(dfDataSel, "TDS")
plotSelvTDS + xlab("TDS mg/L") +
    labs(title = "TDS vs Selenium")

#-more columns

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

#--- get the lower pH values, lower 25% percent ---

temp <- dfDataSel %>% 
    filter(quantile(pH, 0.25, na.rm = TRUE)>pH)
glimpse(temp)
range(dfDataSel$pH, na.rm = TRUE)


